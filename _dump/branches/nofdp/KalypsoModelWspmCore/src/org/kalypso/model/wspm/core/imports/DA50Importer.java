/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.wspm.core.imports;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.java.util.DoubleComparator;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.KalypsoModelWspmCorePlugin;
import org.kalypso.model.wspm.core.gml.ProfileFeatureFactory;
import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypso.model.wspm.core.i18n.Messages;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.core.util.WspmGeometryUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;

/**
 * Helper class to import a DA50 file and apply its values to a list of profiles.
 * 
 * @author Gernot Belger
 * @author kimwerner
 */
public class DA50Importer
{
  /**
   * @param bRefFirst
   *            Where to apply the reference: if true, the start-point is applied to the first point of the profile,
   *            else to the point with the breite-coordinate zero.
   */
  public static FeatureChange[] importDA50( final File da50File, final FeatureList profileFeatures, final boolean bRefFirst ) throws CoreException
  {
    final List<FeatureChange> changes = new ArrayList<FeatureChange>();

    LineNumberReader da50reader = null;
    try
    {
      /* Read and parse d50 file */
      da50reader = new LineNumberReader( new FileReader( da50File ) );
      final DA50Entry[] entries = readDA50( da50reader );

      /* Index features by station */
      final Map<Double, DA50Entry> entryMap = new TreeMap<Double, DA50Entry>( new DoubleComparator( Math.pow( 10, -WspmProfile.STATION_SCALE ) ) );
      for( final DA50Entry entry : entries )
        entryMap.put( entry.station, entry );

      /* Apply d50 information to profiles */
      for( final Object o : profileFeatures )
      {
        final Feature profileFeature = (Feature) o;
        final WspmProfile profile = new WspmProfile( profileFeature );
        final double station = profile.getStation();
        final DA50Entry d50Entry = entryMap.get( station );
        if( d50Entry != null )
        {
          final IProfil profil = profile.getProfil();
          applyD50Entry( profil, d50Entry, bRefFirst );
          final FeatureChange[] fcs = ProfileFeatureFactory.toFeatureAsChanges( profil, profileFeature );
          Collections.addAll( changes, fcs );
        }
      }

      return changes.toArray( new FeatureChange[changes.size()] );
    }
    catch( final IOException e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e, Messages.DA50Importer_0 );
      throw new CoreException( status );
    }
    finally
    {
      IOUtils.closeQuietly( da50reader );
    }

  }

  /**
   * @param bRefFirst
   *            Where to apply the reference: if true, the start-point is applied to the first point of the profile,
   *            else to the point with the breite-coordinate zero.
   */
  private static void applyD50Entry( final IProfil profil, final DA50Entry entry, final boolean bRefFirst ) throws CoreException
  {
    final GM_Position startPos = entry.start.getPosition();
    final GM_Position endPos = entry.end.getPosition();

    // adjust for 0?
    // den Vektor der Profilachse ausrechnen
    double vx = endPos.getX() - startPos.getX();
    double vy = endPos.getY() - startPos.getY();
    final double vl = Math.sqrt( vx * vx + vy * vy );
    if( vl == 0.0 )
    {
      final String message = String.format( Messages.DA50Importer_1, entry.station );
      throw new CoreException( StatusUtilities.createErrorStatus( message ) );
    }

    // den Vektor normieren
    vx /= vl;
    vy /= vl;

    /* Only do reference the first and last point */
    final IRecord[] points = profil.getPoints();
    if( points.length < 2 )
      return;

    final IRecord firstPP = points[0];
    final IRecord lastPP = points[points.length - 1];
    final Double firstBreite = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, firstPP );
    final Double lastBreite = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, lastPP );

    double yOffset = 0;
    if( bRefFirst )
      yOffset = firstBreite;

    final double yFirst = firstBreite - yOffset;
    final double rwFirst = startPos.getX() + yFirst * vx;
    final double hwFirst = startPos.getY() + yFirst * vy;

    final double yLast = lastBreite - yOffset;
    final double rwLast = startPos.getX() + yLast * vx;
    final double hwLast = startPos.getY() + yLast * vy;

    final IComponent cRechtswert = profil.getPointPropertyFor( IWspmConstants.POINT_PROPERTY_RECHTSWERT );
    final IComponent cHochwert = profil.getPointPropertyFor( IWspmConstants.POINT_PROPERTY_HOCHWERT );

    if( !profil.hasPointProperty( cRechtswert ) )
      profil.addPointProperty( cRechtswert );
    if( !profil.hasPointProperty( cHochwert ) )
      profil.addPointProperty( cHochwert );

    final int iHochwert = profil.indexOfProperty( cHochwert );
    final int iRechtswert = profil.indexOfProperty( cRechtswert );

    firstPP.setValue( iRechtswert, rwFirst );
    firstPP.setValue( iHochwert, hwFirst );
    lastPP.setValue( iRechtswert, rwLast );
    lastPP.setValue( iHochwert, hwLast );
  }

  private static DA50Entry[] readDA50( final LineNumberReader lnr ) throws IOException, CoreException
  {
    final List<DA50Entry> result = new ArrayList<DA50Entry>();

    final MultiStatus logStatus = new MultiStatus( KalypsoModelWspmCorePlugin.getID(), 0, Messages.DA50Importer_2, null );

    while( lnr.ready() )
    {
      final String line = lnr.readLine();
      if( line == null )
        break;

      try
      {

        if( line.length() < 60 || !line.startsWith( "50" ) ) //$NON-NLS-1$
          continue;

        // Station auslesen und mit 0en auff�llen, sonst klappt das umrechnen in m nicht immer
        // CString help_str = str.Mid( 9,9 );
        // help_str.TrimRight();
        // help_str = help_str + CString( '0', 9 - help_str.GetLength() );
        final String stationString = line.substring( 9, 18 ).trim();
        final StringBuffer stationBuf = new StringBuffer( stationString );
        for( int i = stationString.length() - 9; i < 0; i++ )
          stationBuf.append( '0' );

        // int temp = 0;
        // sscanf( help_str, "%d", &temp );
        // double station = (double)temp / 1000000;
        final double station = Double.parseDouble( stationBuf.toString() ) / 1000000;

        // logstream << "Geokoordinaten gefunden f�r Station: " << station << std::endl;

        // logstream << "Querprofil mit gleicher Station gefunden" << std::endl;

        // double rw_start, rw_end,hw_start,hw_end;
        // __int64 rh_wert;

        // help_str=str.Mid(21-1,10);

        final String rwStartString = line.substring( 20, 30 );
        final String hwStartString = line.substring( 30, 40 );
        final String rwEndString = line.substring( 40, 50 );
        final String hwEndString = line.substring( 50, 60 );

        final double rwStart = Double.parseDouble( rwStartString ) / 1000.0;
        final double hwStart = Double.parseDouble( hwStartString ) / 1000.0;
        final double rwEnd = Double.parseDouble( rwEndString ) / 1000.0;
        final double hwEnd = Double.parseDouble( hwEndString ) / 1000.0;

        final GM_Point start = WspmGeometryUtilities.pointFromRwHw( rwStart, hwStart, 0.0 );
        final GM_Point end = WspmGeometryUtilities.pointFromRwHw( rwEnd, hwEnd, 0.0 );

        result.add( new DA50Entry( station, start, end ) );
      }
      catch( final Exception e )
      {
        final IStatus status = StatusUtilities.statusFromThrowable( e, Messages.DA50Importer_4 + lnr.getLineNumber() );
        logStatus.add( status );
      }
    }

    if( !logStatus.isOK() )
      throw new CoreException( logStatus );

    return result.toArray( new DA50Entry[result.size()] );
  }

  public static final class DA50Entry
  {
    public final double station;

    public final GM_Point start;

    public final GM_Point end;

    public DA50Entry( @SuppressWarnings("hiding")//$NON-NLS-1$
    final double station, @SuppressWarnings("hiding")//$NON-NLS-1$
    final GM_Point start, @SuppressWarnings("hiding")//$NON-NLS-1$
    final GM_Point end )
    {
      this.station = station;
      this.start = start;
      this.end = end;
    }
  }
}