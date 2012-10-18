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
package org.kalypso.model.wspm.tuhh.ui.rules;

import java.math.BigDecimal;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.CoreException;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.IWspmPointProperties;
import org.kalypso.model.wspm.core.gml.classifications.helper.WspmClassifications;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.IProfilePointMarker;
import org.kalypso.model.wspm.core.profil.validator.AbstractValidatorRule;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.ICulvertBuilding;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.IProfileBuilding;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.tuhh.ui.resolutions.AddRoughnessResolution;

/**
 * @author kimwerner
 * @author Dirk Kuch
 */
public class RauheitRule extends AbstractValidatorRule
{
  @Override
  public void validate( final IProfile profile, final IValidatorMarkerCollector collector ) throws CoreException
  {
    if( profile == null || isDurchlass( profile.getProfileObjects( IProfileBuilding.class ) ) )
      return;

    final String stationId = String.format( "km %.4f", profile.getStation() );//$NON-NLS-1$

    if( !WspmClassifications.hasRoughnessProperties( profile ) && !WspmClassifications.hasRoughnessClass( profile ) )
    {
      final AddRoughnessResolution resolution = new AddRoughnessResolution( new String[] { IWspmConstants.POINT_PROPERTY_RAUHEIT_KS, IWspmConstants.POINT_PROPERTY_RAUHEIT_KST } );
      collector.createProfilMarker( IMarker.SEVERITY_ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.RauheitRule.3" ), stationId, 0, StringUtils.EMPTY, resolution ); //$NON-NLS-1$
      return;
    }

    final IProfilePointMarker[] durchS = profile.getPointMarkerFor( profile.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE ) );
    if( durchS.length < 2 )
      return;

    final int leftD = durchS[0].getPoint().getIndex();
    final int rightD = durchS[durchS.length - 1].getPoint().getIndex();
    final IProfileRecord[] points = profile.getPoints( leftD, rightD );
    if( points.length == 0 )
      return;

    checkRoughnessValues( collector, stationId, points );
  }

  private void checkRoughnessValues( final IValidatorMarkerCollector collector, final String stationId, final IProfileRecord[] points ) throws CoreException
  {
    for( final IProfileRecord point : points )
    {
      final Pair<String, Double> result = getValue( point );
      if( result == null )
      {
        final String msg = String.format( Messages.getString( "RauheitRule.0" ), point.getBreite() ); //$NON-NLS-1$
        // TODO: this resolution makes no sense, as it adds a component, but this is already here; problem is the empty vlaue, we need another resolution instead
        // final AddRoughnessResolution resolution = new AddRoughnessResolution( new String[] { IWspmConstants.POINT_PROPERTY_RAUHEIT_KS, IWspmConstants.POINT_PROPERTY_RAUHEIT_KST,
        // IWspmPointProperties.POINT_PROPERTY_ROUGHNESS_CLASS } );
        collector.createProfilMarker( IMarker.SEVERITY_ERROR, msg, stationId, point.getIndex(), StringUtils.EMPTY );

        return;
      }

      final String id = result.getKey();
      final Double value = result.getValue();

      if( value == null || value.isNaN() )
      {
        // FIXME: this prefix is nonsense -> use real label
        // ComponentUtilities.getComponentLabel( null );

        final String prefix = StringUtils.EMPTY;

        final String message = prefix + Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.RauheitRule.0" ); //$NON-NLS-1$

        collector.createProfilMarker( IMarker.SEVERITY_ERROR, message, stationId, point.getIndex(), id ); //$NON-NLS-1$ //$NON-NLS-2$
        return;
      }

      if( value <= 0.0 )
      {
        // FIXME: this prefix is nonsense -> use real label
        // ComponentUtilities.getComponentLabel( null );
        final String prefix = StringUtils.EMPTY;

        final String message = prefix + Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.RauheitRule.1" ); //$NON-NLS-1$

        collector.createProfilMarker( IMarker.SEVERITY_ERROR, message, stationId, point.getIndex(), id ); //$NON-NLS-1$ //$NON-NLS-2$
        return;
      }
    }
  }

  private Pair<String, Double> getValue( final IProfileRecord point )
  {
    // FIXME: ks preceedes over kst, why?

    // TODO: instead, we should independently validate ks and kst -> two rules

    final boolean preferClasses = false;

    final BigDecimal ksValue = WspmClassifications.getRoughnessValue( point, IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KS, preferClasses );
    if( ksValue != null )
      return Pair.of( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KS, ksValue.doubleValue() );

    final BigDecimal kstValue = WspmClassifications.getRoughnessValue( point, IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KST, preferClasses );
    if( kstValue != null )
      return Pair.of( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KST, kstValue.doubleValue() );

    return null;
  }

  private boolean isDurchlass( final IProfileObject[] objects )
  {
    if( objects == null || objects.length < 1 )
      return false;

    final IProfileObject object = objects[0];
    return object instanceof ICulvertBuilding;
  }
}