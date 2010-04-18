/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
package org.kalypso.model.wspm.tuhh.ui.export;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Formatter;
import java.util.Locale;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.observation.result.IRecord;

/**
 * @author Gernot Belger
 */
public class SobekProfileExporter
{
  private final Formatter m_formatter;

  private final Collection<IStatus> m_stati = new ArrayList<IStatus>();

  public SobekProfileExporter( final File file ) throws FileNotFoundException
  {
    m_formatter = new Formatter( file );
  }

  public void writeProfile( final IProfil profil )
  {
    final String profileName = profil.getName();
    final String pnam = getPNam( profileName );
    final String userPrefix = "";

    // TODO: get id-pattern from user: see .prf export how to do it
    // final Object id = String.format( "%s%s%.4f", userPrefix, profil.getStation() );

    final Object id = String.format( "%s%s", userPrefix, pnam );

    final IRecord[] points = getPointsToExport( profil );

    if( points == null )
      return;

    m_formatter.format( "CRDS id '%s' nm '%s' ty 10 st 0 lt sw 0 0 gl 0 gu 0 lt yz%n", id, profileName );
    m_formatter.format( "TBLE%n" );

    final int widhtIndex = profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_BREITE );
    final int heightIndex = profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_HOEHE );

    for( final IRecord point : points )
    {
      final Number y = (Number) point.getValue( widhtIndex );
      final Number z = (Number) point.getValue( heightIndex );
      m_formatter.format( Locale.US, "%.4f %.4f <%n", y, z );
    }
    m_formatter.format( "tble%n" );
    m_formatter.format( "crds%n" );
  }

  private String getPNam( final String profileName )
  {
    final String[] split = profileName.split( " " );
    if( split.length == 0 )
      return "-";

    return split[0];
  }

  private IRecord[] getPointsToExport( final IProfil profil )
  {
    // TODO: let user choose get marker type
    final String pointMarkerId = IWspmTuhhConstants.MARKER_TYP_BORDVOLL;
    // TODO: get from some registry
    final String markerLabel = "Bordvollpunkte";

    if( pointMarkerId == null )
      return profil.getPoints();

    final IProfilPointMarker[] markers = profil.getPointMarkerFor( pointMarkerId );
    if( markers.length < 2 )
    {
      final String message = String.format( "Gew‰hlte Markierung (%s) bei Profil %.4f (%s) nicht gesetzt. Es wurden alle Profilpunkte exportiert.", markerLabel, profil.getStation(), profil.getName() );
      final IStatus status = new Status( IStatus.WARNING, KalypsoModelWspmTuhhUIPlugin.getID(), message );
      m_stati.add( status );
      return profil.getPoints();
    }

    final IRecord startPoint = markers[0].getPoint();
    final IRecord endPoint = markers[1].getPoint();

    final int startIndex = profil.indexOfPoint( startPoint );
    final int endIndex = profil.indexOfPoint( endPoint );

    return profil.getPoints( startIndex, endIndex );
  }

  public void close( ) throws IOException
  {
    m_formatter.flush();
    checkIO();
    m_formatter.close();
    checkIO();
  }

  private void checkIO( ) throws IOException
  {
    final IOException ioException = m_formatter.ioException();
    if( ioException != null )
      throw ioException;
  }

  public void closeQuiet( )
  {
    m_formatter.close();
  }

  public IStatus getStatus( )
  {
    final IStatus[] children = m_stati.toArray( new IStatus[m_stati.size()] );
    if( children.length > 0 )
      return new MultiStatus( KalypsoModelWspmTuhhUIPlugin.getID(), -1, children, "Es sind Probleme beim SOBEK-Export aufgetreten", null );
    return Status.OK_STATUS;
  }

}
