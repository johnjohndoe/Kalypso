/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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

import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.util.ProfilObsHelper;
import org.kalypso.model.wspm.core.profil.validator.AbstractValidatorRule;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.resolutions.AbstractProfilMarkerResolution;
import org.kalypso.model.wspm.tuhh.ui.resolutions.AddBewuchsResolution;
import org.kalypso.model.wspm.tuhh.ui.resolutions.DelBewuchsResolution;
import org.kalypso.observation.result.IRecord;

/**
 * Brückenkanten dürfen nicht unterhalb des Geländeniveaus liegen Oberkante darf nicht unter Unterkante
 * 
 * @author belger
 */
public class BewuchsRule extends AbstractValidatorRule
{
  public void validate( final IProfil profil, final IValidatorMarkerCollector collector ) throws CoreException
  {
    if( profil == null )
      return;

    final LinkedList<IRecord> points = profil.getPoints();
    if( points == null )
      return;
    if( profil.hasPointProperty( ProfilObsHelper.getPropertyFromId( profil, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR ) )
        || profil.hasPointProperty( ProfilObsHelper.getPropertyFromId( profil, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE ) )
        || !profil.hasPointProperty( ProfilObsHelper.getPropertyFromId( profil, IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AX ) ) )
      return;

    final IProfilPointMarker[] devider = profil.getPointMarkerFor( ProfilObsHelper.getPropertyFromId( profil, IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );
    final IRecord leftP = (devider.length > 0) ? devider[0].getPoint() : null;
    final IRecord rightP = (devider.length > 1) ? devider[devider.length - 1].getPoint() : null;
    if( (leftP == null) || (rightP == null) )
      return;
    final int leftIndex = points.indexOf( leftP );
    final int rightIndex = points.indexOf( rightP );
    final List<IRecord> VorlandL = points.subList( 0, leftIndex );
    final List<IRecord> VorlandR = points.subList( rightIndex, points.size() );
    final List<IRecord> Flussschl = points.subList( leftIndex, rightIndex );
    final String pluginId = PluginUtilities.id( KalypsoModelWspmTuhhUIPlugin.getDefault() );
    final boolean VorlandLhasValues = validateArea( collector, VorlandL, 0, pluginId );
    final boolean VorlandRhasValues = validateArea( collector, VorlandR, 0, pluginId );

    try
    {
      if( VorlandLhasValues | VorlandRhasValues && !Flussschl.isEmpty() )
      {
        int i = leftIndex;
        for( final IRecord point : Flussschl )
        {
          final double ax = (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AX ) );
          final double ay = (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AY ) );
          final double dp = (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_DP ) );
          if( ax + ay + dp != 0 )
            collector.createProfilMarker( false, "Bewuchsparameter im Flußschlauch werden ignoriert", "", i, IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AX, pluginId, new AbstractProfilMarkerResolution[] { new DelBewuchsResolution() } );
          i++;
        }
        final int lastIndex = (leftIndex > 0) ? leftIndex - 1 : leftIndex;
        if( VorlandLhasValues && (Double) points.get( lastIndex ).getValue( ProfilObsHelper.getPropertyFromId( points.get( lastIndex ), IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AX ) ) == 0 )
          collector.createProfilMarker( false, "Bewuchsparameter an Trennflächen überprüfen", "", lastIndex, IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AX, pluginId, new AbstractProfilMarkerResolution[] { new AddBewuchsResolution( 0 ) } );
        if( VorlandRhasValues && (Double) rightP.getValue( ProfilObsHelper.getPropertyFromId( rightP, IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AX ) ) == 0 )
          collector.createProfilMarker( false, "Bewuchsparameter an Trennflächen überprüfen", "", rightIndex, IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AX, pluginId, new AbstractProfilMarkerResolution[] { new AddBewuchsResolution( devider.length - 1 ) } );
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new CoreException( new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getDefault().getBundle().getSymbolicName(), 0, "Profilfehler", e ) );
    }
  }

  private boolean validateArea( final IValidatorMarkerCollector collector, final List<IRecord> subList, final int fromIndex, final String pluginId ) throws CoreException
  {
    boolean hasValues = false;
    if( subList.isEmpty() )
      return false;
    int i = fromIndex;
    for( final IRecord point : subList )
    {
      final double ax = (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AX ) );
      final double ay = (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AY ) );
      final double dp = (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_DP ) );
      if( ax + ay + dp != 0.0 )
      {
        if( ax * ay * dp == 0.0 )
        {
          final StringBuffer stringBuffer = new StringBuffer( "Bewuchsparameter(" );
          if( ax == 0.0 )
          {
            stringBuffer.append( "aX, " );
          }
          if( ay == 0.0 )
          {
            stringBuffer.append( "aY, " );
          }
          if( dp == 0.0 )
          {
            stringBuffer.append( "dP" );
          }
          stringBuffer.append( ") fehlt" );
          collector.createProfilMarker( true, stringBuffer.toString(), "", i, IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AX, pluginId, null );
        }
        else
          hasValues = true;
      }
      i++;
    }
    return hasValues;
  }
}
