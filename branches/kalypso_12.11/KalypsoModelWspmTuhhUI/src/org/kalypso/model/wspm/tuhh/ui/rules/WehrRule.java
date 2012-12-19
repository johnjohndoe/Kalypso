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

import java.util.Map;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.CoreException;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfilePointMarker;
import org.kalypso.model.wspm.core.profil.util.ProfileUtil;
import org.kalypso.model.wspm.core.profil.validator.AbstractValidatorRule;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingWehr;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingWehr.WeirType;
import org.kalypso.model.wspm.tuhh.core.util.river.line.WspmSohlpunkte;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.tuhh.ui.resolutions.DelBewuchsResolution;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

/**
 * @author Kim Werner
 */
public class WehrRule extends AbstractValidatorRule
{
  @Override
  public void validate( final IProfile profil, final IValidatorMarkerCollector collector ) throws CoreException
  {
    if( profil == null )
      return;

    final BuildingWehr building = WspmSohlpunkte.getBuilding( profil, BuildingWehr.class );
    if( building == null )
      return;

    validateLimits( profil, collector );
    validateProfilLines( profil, collector );
    validateDevider( profil, collector );
    validateParams( profil, collector );
    validateBewuchs( profil, collector );
  }

  private void validateDevider( final IProfile profil, final IValidatorMarkerCollector collector ) throws CoreException
  {
    final IProfilePointMarker[] wehrDevider = profil.getPointMarkerFor( profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_WEHR ) );
    final IProfilePointMarker[] deviders = profil.getPointMarkerFor( profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );
    if( wehrDevider == null || wehrDevider.length < 1 || deviders == null || deviders.length < 2 )
      return;

    final int index1 = wehrDevider[0].getPoint().getIndex();
    final int index2 = wehrDevider[wehrDevider.length - 1].getPoint().getIndex();
    final int index3 = deviders[0].getPoint().getIndex();
    final int index4 = deviders[deviders.length - 1].getPoint().getIndex();
    if( index1 < index3 )
      collector.createProfilMarker( IMarker.SEVERITY_ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.WehrRule.1" ), String.format( "km %.4f", profil.getStation() ), index1, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR ); //$NON-NLS-1$ //$NON-NLS-2$

    if( index2 > index4 )
      collector.createProfilMarker( IMarker.SEVERITY_ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.WehrRule.3" ), String.format( "km %.4f", profil.getStation() ), index2, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  // TODO: in die Bewuchsregel verschieben -> doppelter Code
  private void validateBewuchs( final IProfile profil, final IValidatorMarkerCollector collector ) throws CoreException
  {

    if( profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_AX ) == null )
      return;
    final IProfilePointMarker[] deviders = profil.getPointMarkerFor( profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );
    if( deviders.length < 2 )
      return;
    final int index1 = deviders[0].getPoint().getIndex();
    final int index2 = deviders[deviders.length - 1].getPoint().getIndex();
    final IRecord[] points = profil.getPoints();
    for( int i = index1; i < index2; i++ )
    {
      final Double vAX = ProfileUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BEWUCHS_AX, points[i] );
      final Double vAY = ProfileUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BEWUCHS_AY, points[i] );
      final Double vDP = ProfileUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BEWUCHS_DP, points[i] );

      if( !(vAX.isNaN() || vAY.isNaN() || vDP.isNaN()) && vAX + vAY + vDP > 0 )
      {
        collector.createProfilMarker( IMarker.SEVERITY_ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.WehrRule.5" ), String.format( "km %.4f", profil.getStation() ), i, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR, new DelBewuchsResolution() ); //$NON-NLS-1$ //$NON-NLS-2$
        break;
      }
    }
  }

  private void validateParams( final IProfile profil, final IValidatorMarkerCollector collector ) throws CoreException
  {
    final IProfilePointMarker[] deviders = profil.getPointMarkerFor( profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_WEHR ) );

    final BuildingWehr building = WspmSohlpunkte.getBuilding( profil, BuildingWehr.class );
    if( building == null )
      return;

    if( building.getWehrart() == WeirType.scharfkantig )
      return;

    if( deviders != null )
    {
      for( final IProfilePointMarker devider : deviders )
      {
        final Object objValue = devider.getValue();
        if( objValue == null || !(objValue instanceof Double) || ((Double)objValue).isNaN() || (Double)objValue == 0.0 )
        {
          collector.createProfilMarker( IMarker.SEVERITY_ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.WehrRule.7" ), String.format( "km %.4f", profil.getStation() ), devider.getPoint().getIndex(), IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR ); //$NON-NLS-1$ //$NON-NLS-2$
          break;
        }
      }
    }

    final Double formbeiwert = building.getFormbeiwert();
    if( formbeiwert == null || formbeiwert.isNaN() || formbeiwert.doubleValue() == 0.0 )
      collector.createProfilMarker( IMarker.SEVERITY_ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.WehrRule.9", BuildingWehr.KEY_FORMBEIWERT ), String.format( "km %.4f", profil.getStation() ), 0, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  private void validateProfilLines( final IProfile profil, final IValidatorMarkerCollector collector ) throws CoreException
  {
    final ProfileAltitudeValidator pav = new ProfileAltitudeValidator( profil, collector );
    final IRecord[] points = profil.getPoints();
    final int outerLeft = pav.whileNaN( 0, points.length - 1, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR );
    final int outerRight = pav.whileNaN( points.length - 1, 0, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR );
    final Map<Integer, Double> OKW = pav.getInterpolatedValues( outerLeft, outerRight, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR );
    final int innerLeft = pav.whileEqual( outerLeft, outerRight, OKW );
    final int innerRight = pav.whileEqual( outerRight, innerLeft, OKW );
    final IProfilePointMarker[] deviders = profil.getPointMarkerFor( profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );
    if( deviders.length < 2 )
      return;

    // final int iHoehe = profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_HOEHE );
    // final int iOKWehr = profil.indexOfProperty( IWspmTuhhConstants.BUILDING_TYP_WEHR );
    // if( iOKWehr < 0 || iHoehe < 0 )
    // return;

    final int left = deviders[0].getPoint().getIndex();
    final int right = deviders[deviders.length - 1].getPoint().getIndex();
    if( left != innerLeft )
    {
      collector.createProfilMarker( IMarker.SEVERITY_ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.WehrRule.14" ), String.format( "km %.4f", profil.getStation() ), innerLeft, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR ); //$NON-NLS-1$ //$NON-NLS-2$
      return;
    }
    if( right != innerRight )
    {
      collector.createProfilMarker( IMarker.SEVERITY_ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.WehrRule.14" ), String.format( "km %.4f", profil.getStation() ), innerRight, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR ); //$NON-NLS-1$ //$NON-NLS-2$
      return;
    }

    // final IRecord[] midPoints = profil.getPoints( left + 1, right - 1 );
    for( int i = innerLeft; i <= innerRight; i++ )
    {
      final Double h = ProfileUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, points[i] );
      final Double wk = OKW.get( i );// ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.BUILDING_TYP_WEHR, point );
      if( !h.isNaN() && wk != null && wk < h )
      {
        collector.createProfilMarker( IMarker.SEVERITY_ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.WehrRule.12" ), String.format( "km %.4f", profil.getStation() ), i, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR ); //$NON-NLS-1$ //$NON-NLS-2$
        return;
      }
    }
  }

  private void validateLimits( final IProfile profil, final IValidatorMarkerCollector collector ) throws CoreException
  {
    final IProfilePointMarker[] devider = profil.getPointMarkerFor( profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );
    for( final IProfilePointMarker marker : devider )
    {
      final IProfileRecord point = marker.getPoint();
      validateLimit( profil, collector, point );
    }
  }

  private void validateLimit( final IProfile profil, final IValidatorMarkerCollector collector, final IProfileRecord point ) throws CoreException
  {
    final int iHoehe = profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_HOEHE );
    final int iOKWehr = profil.indexOfProperty( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR );
    if( iOKWehr < 0 || iHoehe < 0 )
      return;

    final IComponent okWeir = profil.hasPointProperty( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR );
    final double deltaOkW = okWeir.getPrecision();

    final Double groundValue = ProfileUtil.getDoubleValueFor( iHoehe, point );
    final Double weirValue = ProfileUtil.getDoubleValueFor( iOKWehr, point );

    if( !weirValue.isNaN() && !groundValue.isNaN() && Math.abs( groundValue - weirValue ) > deltaOkW )
    {
      final String location = String.format( "km %.4f", profil.getStation() ); //$NON-NLS-1$
      final int indexOfPoint = point.getIndex();
      final String message = Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.WehrRule.14" ); //$NON-NLS-1$
      collector.createProfilMarker( IMarker.SEVERITY_ERROR, message, location, indexOfPoint, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR );
    }
  }
}