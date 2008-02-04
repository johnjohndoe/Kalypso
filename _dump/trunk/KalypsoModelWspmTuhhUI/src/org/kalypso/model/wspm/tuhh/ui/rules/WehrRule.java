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

import java.util.List;

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.IMarkerResolution2;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.util.ProfilObsHelper;
import org.kalypso.model.wspm.core.profil.validator.AbstractValidatorRule;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.resolutions.EditPointResolution;
import org.kalypso.model.wspm.tuhh.ui.resolutions.MoveDeviderResolution;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

public class WehrRule extends AbstractValidatorRule
{
  public void validate( final IProfil profil, final IValidatorMarkerCollector collector ) throws CoreException
  {
    // TODO IProfileObjects now returned as list from IProfile
    final IProfileObject[] profileObjects = profil.getProfileObjects();
    IProfileObject building = null;
    if( profileObjects.length > 0 )
      building = profileObjects[0];

    if( profil == null || building == null || !IWspmTuhhConstants.BUILDING_TYP_WEHR.equals( building.getId() ) )
      return;
    try
    {
      // FIXMEfinal String pluginId = PluginUtilities.id( KalypsoModelWspmTuhhUIPlugin.getDefault() );
      // FIXMEfinal IRecord leftP = validateLimits( profil, collector, pluginId );
      // FIXMEvalidateProfilLines( profil, collector, pluginId );
      // FIXME validateDevider( profil, collector, pluginId );
      // FIXME validateParams( profil, collector, pluginId, leftP );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new CoreException( new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getDefault().getBundle().getSymbolicName(), 0, "Profilfehler", e ) );
    }
  }

  private void validateDevider( final IProfil profil, final IValidatorMarkerCollector collector, final String pluginId ) throws Exception
  {
    final IProfilPointMarker[] wehrDevider = profil.getPointMarkerFor( ProfilObsHelper.getPropertyFromId( profil, IWspmTuhhConstants.MARKER_TYP_WEHR ) );
    if( wehrDevider == null || wehrDevider.length < 1 )
      return;
    final IProfilPointMarker[] deviders = profil.getPointMarkerFor( ProfilObsHelper.getPropertyFromId( profil, IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );
    final IRecord[] points = profil.getPoints();
    final int index1 = ArrayUtils.indexOf( points, wehrDevider[0].getPoint() );
    final int index2 = ArrayUtils.indexOf( points, wehrDevider[wehrDevider.length - 1].getPoint() );
    final int index3 = ArrayUtils.indexOf( points, deviders[0].getPoint() );
    final int index4 = ArrayUtils.indexOf( points, deviders[deviders.length - 1].getPoint() );
    if( index1 < index3 )
      collector.createProfilMarker( true, "Wehrfeldtrenner: ung�ltige Position", "", index1, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR, pluginId, new IMarkerResolution2[] { new MoveDeviderResolution( 0, ProfilObsHelper.getPropertyFromId( profil, IWspmTuhhConstants.MARKER_TYP_WEHR ), index3 ) } );
    if( index2 > index4 )
      collector.createProfilMarker( true, "Wehrfeldtrenner: ung�ltige Position", "", index2, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR, pluginId, new IMarkerResolution2[] { new MoveDeviderResolution( wehrDevider.length - 1, ProfilObsHelper.getPropertyFromId( profil, IWspmTuhhConstants.MARKER_TYP_WEHR ), index4 ) } );
  }

  private void validateParams( final IProfil profil, final IValidatorMarkerCollector collector, final String pluginId, final IRecord p ) throws Exception
  {

    final IProfilPointMarker[] deviders = profil.getPointMarkerFor( ProfilObsHelper.getPropertyFromId( profil, IWspmTuhhConstants.MARKER_TYP_WEHR ) );

    // TODO IProfileObjects now returned as list from IProfile
    final IProfileObject[] profileObjects = profil.getProfileObjects();
    IProfileObject building = null;
    if( profileObjects.length > 0 )
      building = profileObjects[0];

    final Double beiwert = (Double) building.getValue( ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_FORMBEIWERT ) );
    IRecord point = beiwert == null || beiwert == 0.0 ? p : null;
    if( deviders != null )
      for( final IProfilPointMarker devider : deviders )
      {
        final double value = devider.getValue() == null ? 0.0 : (Double) devider.getValue();
        if( value == 0.0 )
        {
          point = devider.getPoint();
          break;
        }
      }
    for( final IComponent property : building.getObjectProperties() )
    {
      final Object prop = building.getValue( property );
      if( prop instanceof Double && ((Double) prop).isNaN() )
      {
        collector.createProfilMarker( true, "Parameter <" + property.getName() + "> fehlt", "", 0, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE, pluginId, null );
        break;
      }
    }
    if( point != null )
      collector.createProfilMarker( true, "ung�ltiger Kronenparameter: 0.0", "", ArrayUtils.indexOf( profil.getPoints(), point ), IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR, pluginId, null );
  }

  private void validateProfilLines( final IProfil profil, final IValidatorMarkerCollector collector, final String pluginId ) throws Exception
  {
    final IRecord[] points = profil.getPoints();
    final IProfilPointMarker[] deviders = profil.getPointMarkerFor( ProfilObsHelper.getPropertyFromId( profil, IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );
    if( deviders.length < 1 )
      return;
    final int left = ArrayUtils.indexOf( points, deviders[0].getPoint() );
    final int right = ArrayUtils.indexOf( points, deviders[deviders.length - 1].getPoint() );
    if( left + 1 > right )
      return;
    final List<IRecord> midPoints = profil.getResult().subList( left + 1, right );
    for( final IRecord point : midPoints )
    {
      final double h = (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_HOEHE ) );
      final double wk = (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR ) );
      if( wk < h )
        collector.createProfilMarker( true, "Wehrkante[" + String.format( FMT_BREITE, point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_BREITE ) ) )
            + "] unterhalb Gel�ndeniveau", "", ArrayUtils.indexOf( points, point ), IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR, pluginId, null );
    }
  }

  private IRecord validateLimits( final IProfil profil, final IValidatorMarkerCollector collector, final String pluginId ) throws Exception
  {
    final IProfilPointMarker[] devider = profil.getPointMarkerFor( ProfilObsHelper.getPropertyFromId( profil, IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );
    if( devider.length < 2 )
      return null;
    final IRecord firstPoint = devider[0].getPoint();
    final IRecord lastPoint = devider[devider.length - 1].getPoint();
    IRecord point = null;
    final double p1H = (Double) firstPoint.getValue( ProfilObsHelper.getPropertyFromId( firstPoint, IWspmConstants.POINT_PROPERTY_HOEHE ) );
    final double p1OkW = (Double) firstPoint.getValue( ProfilObsHelper.getPropertyFromId( firstPoint, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR ) );
    final double p2H = (Double) lastPoint.getValue( ProfilObsHelper.getPropertyFromId( lastPoint, IWspmConstants.POINT_PROPERTY_HOEHE ) );
    final double p2OkW = (Double) lastPoint.getValue( ProfilObsHelper.getPropertyFromId( lastPoint, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR ) );
    // final double dB = profil.getPointProperty( IWspmTuhhConstants.POINT_PROPERTY_BREITE ).getPrecision();
    final double dOkW = ProfilObsHelper.getPrecision( ProfilObsHelper.getPropertyFromId( profil, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR ) );
    if( Math.abs( p1H - p1OkW ) > dOkW )
      point = firstPoint;
    if( Math.abs( p2H - p2OkW ) > dOkW )
      point = lastPoint;
    if( point != null )
    {
      final int index = ArrayUtils.indexOf( profil.getPoints(), point );
      collector.createProfilMarker( true, "ung�ltige Randbedingung ["
          + String.format( FMT_BREITE, (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_BREITE ) ) ) + "]", "", ArrayUtils.indexOf( profil.getPoints(), point ), IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR, pluginId, new IMarkerResolution2[] { new EditPointResolution( index, ProfilObsHelper.getPropertyFromId( point, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR ), (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_HOEHE ) ) ) } );
    }
    return firstPoint;
  }
}
