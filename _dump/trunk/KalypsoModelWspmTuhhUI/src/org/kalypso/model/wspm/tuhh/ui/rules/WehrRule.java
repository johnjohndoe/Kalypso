/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraï¿½e 22
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

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.util.ProfilObsHelper;
import org.kalypso.model.wspm.core.profil.validator.AbstractValidatorRule;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

/**
 * @author kimwerner
 */
public class WehrRule extends AbstractValidatorRule
{
  public void validate( final IProfil profil, final IValidatorMarkerCollector collector ) throws CoreException
  {
    if( profil == null )
      return;
    final IProfileObject[] profileObjects = profil.getProfileObjects();
    IProfileObject building = null;
    if( profileObjects.length > 0 )
      building = profileObjects[0];

    if( building == null || !IWspmTuhhConstants.BUILDING_TYP_WEHR.equals( building.getId() ) )
      return;
    try
    {
      final String pluginId = PluginUtilities.id( KalypsoModelWspmTuhhUIPlugin.getDefault() );
      final IRecord leftP = validateLimits( profil, collector, pluginId );
      validateProfilLines( profil, collector, pluginId );
      validateDevider( profil, collector, pluginId );
      validateParams( profil, collector, pluginId, leftP );
      validateBewuchs( profil, collector, pluginId );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new CoreException( new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getDefault().getBundle().getSymbolicName(), 0, "Profilfehler", e ) );
    }
  }

  private void validateDevider( final IProfil profil, final IValidatorMarkerCollector collector, final String pluginId ) throws Exception
  {
    final IProfilPointMarker[] wehrDevider = profil.getPointMarkerFor( profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_WEHR ) );
    if( wehrDevider == null || wehrDevider.length < 1 )
      return;
    final IProfilPointMarker[] deviders = profil.getPointMarkerFor( profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );
    final int index1 = profil.indexOfPoint( wehrDevider[0].getPoint() );
    final int index2 = profil.indexOfPoint( wehrDevider[wehrDevider.length - 1].getPoint() );
    final int index3 = profil.indexOfPoint( deviders[0].getPoint() );
    final int index4 = profil.indexOfPoint( deviders[deviders.length - 1].getPoint() );
    if( index1 < index3 )
      collector.createProfilMarker( true, "Wehrfeldtrenner: ungültige Position", "", index1, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR, pluginId, null );// new
    // IMarkerResolution2[]
    // {
    // new
    // MoveDeviderResolution(
    // 0,
    // ProfilObsHelper.getPropertyFromId(
    // profil,
    // IWspmTuhhConstants.MARKER_TYP_WEHR
    // ),
    // index3
    // ) }
    // );
    if( index2 > index4 )
      collector.createProfilMarker( true, "Wehrfeldtrenner: ungültige Position", "", index2, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR, pluginId, null );// new
    // IMarkerResolution2[]
    // {
    // new
    // MoveDeviderResolution(
    // wehrDevider.length
    // - 1,
    // ProfilObsHelper.getPropertyFromId(
    // profil,
    // IWspmTuhhConstants.MARKER_TYP_WEHR
    // ),
    // index4
    // ) }
    // );
  }

  private void validateBewuchs( final IProfil profil, final IValidatorMarkerCollector collector, final String pluginId ) throws Exception
  {

    final int iAX = profil.indexOfProperty( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AX );
    if( iAX < 0 )
      return;
    final IProfilPointMarker[] deviders = profil.getPointMarkerFor( profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );
    if( deviders.length < 2 )
      return;
    final int index1 = profil.indexOfPoint( deviders[0].getPoint() );
    final int index2 = profil.indexOfPoint( deviders[deviders.length - 1].getPoint() );

    final int iAY = profil.indexOfProperty( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AY );
    final int iDP = profil.indexOfProperty( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_DP );
    for( int i = index1; i < index2; i++ )
    {
      final IRecord point = profil.getPoint( i );
      final Double vAX = (Double) point.getValue( iAX );
      final Double vAY = (Double) point.getValue( iAY );
      final Double vDP = (Double) point.getValue( iDP );
      if( vAX + vAY + vDP > 0 )
      {
        collector.createProfilMarker( true, "Bewuchs im Wehrbereich", "", i, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR, pluginId, null );
        // new
        // IMarkerResolution2[]
        // {
        // new
        // MoveDeviderResolution(
        // 0,
        // ProfilObsHelper.getPropertyFromId(
        // profil,
        // IWspmTuhhConstants.MARKER_TYP_WEHR
        // ),
        // index3
        // ) }
        // );
        break;
      }
    }
  }

  private void validateParams( final IProfil profil, final IValidatorMarkerCollector collector, final String pluginId, final IRecord p ) throws Exception
  {

    final IProfilPointMarker[] deviders = profil.getPointMarkerFor( profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_WEHR ) );

    final IProfileObject[] profileObjects = profil.getProfileObjects();
    IProfileObject building = null;
    if( profileObjects.length > 0 )
      building = profileObjects[0];

    final Object beiwert = building.getValue( ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_FORMBEIWERT ) );
    IRecord point = (beiwert == null || (Double) beiwert == 0.0) ? p : null;
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
        collector.createProfilMarker( true, "Parameter <" + property.getName() + "> fehlt", "", 0, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR, pluginId, null );
        break;
      }
    }
    if( point != null )
      collector.createProfilMarker( true, "ungültiger Kronenparameter: 0.0", "", profil.indexOfPoint( point ), IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR, pluginId, null );
  }

  private void validateProfilLines( final IProfil profil, final IValidatorMarkerCollector collector, final String pluginId ) throws Exception
  {

    final IProfilPointMarker[] deviders = profil.getPointMarkerFor( profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );
    if( deviders.length < 1 )
      return;
    final IComponent cHoehe = profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_HOEHE );
    final IComponent cOKWehr = profil.hasPointProperty( IWspmTuhhConstants.BUILDING_TYP_WEHR );
    if( cOKWehr == null || cHoehe == null )
      return;
    final int left = profil.indexOfPoint( deviders[0].getPoint() );
    final int right = profil.indexOfPoint( deviders[deviders.length - 1].getPoint() );
    if( left + 1 > right )
      return;
    final IRecord[] midPoints = profil.getPoints( left + 1, right - 1 );
    for( final IRecord point : midPoints )
    {
      final Object h = point.getValue( profil.indexOfProperty( cHoehe ) );
      final Object wk = point.getValue( profil.indexOfProperty( cOKWehr ) );
      if( (h instanceof Double) && (wk instanceof Double) && (Double) wk < (Double) h )
        collector.createProfilMarker( true, "Wehrkante[" + String.format( FMT_BREITE, point.getValue( profil.indexOfProperty( cHoehe ) ) ) + "] unterhalb Geländeniveau", "", profil.indexOfPoint( point ), IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR, pluginId, null );
    }
  }

  private IRecord validateLimits( final IProfil profil, final IValidatorMarkerCollector collector, final String pluginId ) throws Exception
  {
    final IProfilPointMarker[] devider = profil.getPointMarkerFor( profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );
    if( devider.length < 2 )
      return null;
    final IRecord firstPoint = devider[0].getPoint();
    final IRecord lastPoint = devider[devider.length - 1].getPoint();
    IRecord point = null;
    final int iHoehe = profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_HOEHE );
    final int iOKWehr = profil.indexOfProperty( IWspmTuhhConstants.BUILDING_TYP_WEHR );
    final int iBreite = profil.indexOfProperty( IWspmTuhhConstants.POINT_PROPERTY_BREITE );
    if( iOKWehr < 0 || iHoehe < 0 )
      return null;
    final Object p1H = firstPoint.getValue( iHoehe );
    final Object p1OkW = firstPoint.getValue( iOKWehr );
    final Object p2H = lastPoint.getValue( iHoehe );
    final Object p2OkW = lastPoint.getValue( iOKWehr );
    if( p1H == null || p1OkW == null || p2H == null || p2OkW == null )
      return null;

    final double dOkW = ProfilObsHelper.getPrecision( profil.hasPointProperty( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR ) );
    if( Math.abs( (Double) p1H - (Double) p1OkW ) > dOkW )
      point = firstPoint;
    if( Math.abs( (Double) p2H - (Double) p2OkW ) > dOkW )
      point = lastPoint;
    if( point != null )
    {
      collector.createProfilMarker( true, "ungültige Randbedingung [" + String.format( FMT_BREITE, point.getValue( iBreite ) ) + "]", "", profil.indexOfPoint( point ), IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR, pluginId, null );// new
      // IMarkerResolution2[]
      // {
      // new
      // EditPointResolution(
      // index,
      // ProfilObsHelper.getPropertyFromId(
      // point,
      // IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR
      // ),
      // (Double)
      // point.getValue(
      // ProfilObsHelper.getPropertyFromId(
      // point,
      // IWspmConstants.POINT_PROPERTY_HOEHE
      // ) )
      // ) }
      // );
    }
    return firstPoint;
  }
}
