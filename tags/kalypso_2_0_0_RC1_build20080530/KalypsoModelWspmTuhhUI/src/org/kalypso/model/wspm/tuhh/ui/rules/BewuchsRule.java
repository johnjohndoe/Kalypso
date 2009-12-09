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
package org.kalypso.model.wspm.tuhh.ui.rules;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.core.profil.validator.AbstractValidatorRule;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.resolutions.DelBewuchsResolution;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

/**
 * kein Bewuchs im Flu�schlauch wenn Bewuchs, dann (AX und AY und DP) != 0.0 wenn Bewuchs, dann mu� auch an Trennfl�chen
 * Bewuchs definiert sein
 * 
 * @author kimwerner
 */
public class BewuchsRule extends AbstractValidatorRule
{
  public void validate( final IProfil profil, final IValidatorMarkerCollector collector ) throws CoreException
  {

    if( profil == null )
      return;

    final IRecord[] points = profil.getPoints();
    if( points == null )
      return;

    final int iOKW = profil.indexOfProperty( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR );
    final int iOKB = profil.indexOfProperty( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE );
    final int iAX = profil.indexOfProperty( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AX );

    /**
     * ohne Bewuchs oder Br�cke bzw Wehr(Bewuchs wird in der Bauwerksregel gepr�ft) vorhanden ?
     */
    if( iOKW >= 0 || iOKB >= 0 || iAX < 0 )
      return;

    /**
     * Bewuchs im Flu�schlauch ?
     */
    final IComponent cTrennF = profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    final IProfilPointMarker[] devider = profil.getPointMarkerFor( cTrennF );
    if( devider.length < 2 )
      return;

    final IRecord leftP = devider[0].getPoint();
    final IRecord rightP = devider[devider.length - 1].getPoint();
    if( leftP == null || rightP == null )
      return;

    final int leftIndex = profil.indexOfPoint( leftP );
    final int rightIndex = profil.indexOfPoint( rightP );
    final IRecord[] VorlandL = profil.getPoints( 0, leftIndex - 1 );
    final IRecord[] VorlandR = profil.getPoints( rightIndex, points.length - 1 );
    final IRecord[] Flussschl = profil.getPoints( leftIndex, rightIndex - 1 );
    final String pluginId = PluginUtilities.id( KalypsoModelWspmTuhhUIPlugin.getDefault() );
    final boolean VorlandLhasValues = validateArea( profil, collector, VorlandL, pluginId );
    final boolean VorlandRhasValues = validateArea( profil, collector, VorlandR, pluginId );

    try
    {
      if( (VorlandLhasValues | VorlandRhasValues) && Flussschl.length > 0 )
      {
        int i = leftIndex;
        for( final IRecord point : Flussschl )
        {
          final Double ax = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AX, point );
          final Double ay = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AY, point );
          final Double dp = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_DP, point );

          if( ax.isNaN() || ay.isNaN() || dp.isNaN() )
          {
            continue;
          }
          else
          {
            if( ax + ay + dp != 0 )
              collector.createProfilMarker( IMarker.SEVERITY_WARNING, "Bewuchsparameter im Flu�schlauch werden ignoriert", "km " + Double.toString( profil.getStation() ), i, IWspmConstants.POINT_PROPERTY_BEWUCHS_AX, pluginId, new DelBewuchsResolution() );
          }
          i++;
        }
        final int lastIndex = leftIndex > 0 ? leftIndex - 1 : leftIndex;

        if( profil.getProfileObjects().length == 0 )
        {
          final Double ax1 = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AX, points[lastIndex] );

          if( (VorlandLhasValues == true && !ax1.isNaN()) && (VorlandLhasValues && ax1 == 0.0) )
            collector.createProfilMarker( IMarker.SEVERITY_INFO, "Bewuchsparameter an Trennfl�chen �berpr�fen", "km " + profil.getStation(), lastIndex, IWspmConstants.POINT_PROPERTY_BEWUCHS_AX, pluginId );// ,

          final Double ax2 = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AX, rightP );

          if( (VorlandLhasValues == true && !ax2.isNaN()) && (VorlandLhasValues && ax2 == 0.0) )
            collector.createProfilMarker( IMarker.SEVERITY_INFO, "Bewuchsparameter an Trennfl�chen �berpr�fen", "km " + profil.getStation(), rightIndex, IWspmConstants.POINT_PROPERTY_BEWUCHS_AX, pluginId );// ,

        }
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new CoreException( new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getDefault().getBundle().getSymbolicName(), 0, "Profilfehler", e ) );
    }
  }

  private boolean validateArea( final IProfil profil, final IValidatorMarkerCollector collector, final IRecord[] subList, final String pluginId ) throws CoreException
  {
    boolean hasValues = false;
    if( subList.length < 1 )
      return false;
    for( final IRecord point : subList )
    {
      final Double ax = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BEWUCHS_AX, point );
      final Double ay = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BEWUCHS_AY, point );
      final Double dp = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BEWUCHS_DP, point );
      if( ax.isNaN() || ay.isNaN() || dp.isNaN() )
      {
        // TODO: f�hrt zu 1000 Fehlern, wenn der Bewuchs nicht gesetzt ist (nach Zuweisung Landnutzung z.b.)
// collector.createProfilMarker( IMarker.SEVERITY_ERROR, "ung�ltiges Datenformat", "km " + Double.toString(
// profil.getStation() ), profil.indexOfPoint( point ), IWspmConstants.POINT_PROPERTY_BEWUCHS_AX, pluginId );
        continue;
      }
      else
      {
        if( ax + ay + dp != 0.0 )
          if( ax * ay * dp == 0.0 )
          {
            final StringBuffer stringBuffer = new StringBuffer( "Bewuchsparameter(" );
            if( ax == 0.0 )
              stringBuffer.append( "aX, " );
            if( ay == 0.0 )
              stringBuffer.append( "aY, " );
            if( dp == 0.0 )
              stringBuffer.append( "dP" );
            stringBuffer.append( ") fehlt" );
            collector.createProfilMarker( IMarker.SEVERITY_ERROR, stringBuffer.toString(), "km " + Double.toString( profil.getStation() ), profil.indexOfPoint( point ), IWspmConstants.POINT_PROPERTY_BEWUCHS_AX, pluginId );
          }
          else
            hasValues = true;
      }
    }
    return hasValues;
  }
}
