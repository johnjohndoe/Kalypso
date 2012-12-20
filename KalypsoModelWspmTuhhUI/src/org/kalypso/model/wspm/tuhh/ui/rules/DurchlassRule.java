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

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.CoreException;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.validator.AbstractValidatorRule;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingEi;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingKreis;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingMaul;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingTrapez;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.ICulvertBuilding;
import org.kalypso.model.wspm.tuhh.core.util.river.line.WspmSohlpunkte;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.tuhh.ui.resolutions.DelRoughnessResolution;
import org.kalypso.observation.result.IComponent;

/**
 * @author Kim Werner
 */
public class DurchlassRule extends AbstractValidatorRule
{
  @Override
  public void validate( final IProfile profil, final IValidatorMarkerCollector collector ) throws CoreException
  {
    final ICulvertBuilding building = WspmSohlpunkte.getBuilding( profil, ICulvertBuilding.class );
    if( building == null )
      return;

    if( building instanceof BuildingEi )
    {
      final BuildingEi eiBuilding = (BuildingEi)building;

      final Double b = eiBuilding.getBreite();
      final Double h = eiBuilding.getHoehe();
      if( b != null && h != null && h.doubleValue() <= b.doubleValue() )
        collector.createProfilMarker( IMarker.SEVERITY_ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.DurchlassRule.0" ), String.format( "km %.4f", profil.getStation() ), 0, null ); //$NON-NLS-1$ //$NON-NLS-2$
    }

    if( building instanceof BuildingMaul )
    {
      final BuildingMaul maulBuilding = (BuildingMaul)building;

      final Double b = maulBuilding.getBreite();
      final Double h = maulBuilding.getHoehe();
      if( b != null && h != null && b.doubleValue() <= h.doubleValue() )
        collector.createProfilMarker( IMarker.SEVERITY_ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.DurchlassRule.2" ), String.format( "km %.4f", profil.getStation() ), 0, null ); //$NON-NLS-1$ //$NON-NLS-2$
    }

    final IComponent compKS = profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_RAUHEIT_KS );
    final IComponent compKST = profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_RAUHEIT_KST );
    if( compKS != null )
      collector.createProfilMarker( IMarker.SEVERITY_WARNING, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.DurchlassRule.1", compKS.getName() ), String.format( "km %.4f", profil.getStation() ), 0, null, new DelRoughnessResolution( IWspmConstants.POINT_PROPERTY_RAUHEIT_KS ) ); //$NON-NLS-1$//$NON-NLS-2$

    if( compKST != null )
      collector.createProfilMarker( IMarker.SEVERITY_WARNING, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.DurchlassRule.1", compKST.getName() ), String.format( "km %.4f", profil.getStation() ), 0, null, new DelRoughnessResolution( IWspmConstants.POINT_PROPERTY_RAUHEIT_KST ) ); //$NON-NLS-1$//$NON-NLS-2$

    if( building instanceof BuildingEi )
      validateCulvertParameter( (BuildingEi)building, profil, collector );

    if( building instanceof BuildingKreis )
      validateCulvertParameter( (BuildingKreis)building, profil, collector );

    if( building instanceof BuildingMaul )
      validateCulvertParameter( (BuildingMaul)building, profil, collector );

    if( building instanceof BuildingTrapez )
      validateCulvertParameter( (BuildingTrapez)building, profil, collector );
  }

  private void validateCulvertParameter( final BuildingEi building, final IProfile profil, final IValidatorMarkerCollector collector ) throws CoreException
  {
    final Double bezugspunktX = building.getBezugspunktX();
    final Double bezugspunktY = building.getBezugspunktY();
    final Double hoehe = building.getHoehe();
    final Double breite = building.getBreite();
    final Double sohlgefaelle = building.getSohlgefaelle();
    final Double rauheit = building.getRauheit();

    if( !validateCulvertParameter( bezugspunktX, BuildingEi.KEY_BEZUGSPUNKT_X, profil, collector ) )
      return;

    if( !validateCulvertParameter( bezugspunktY, BuildingEi.KEY_BEZUGSPUNKT_Y, profil, collector ) )
      return;

    if( !validateCulvertParameter( hoehe, BuildingEi.KEY_HOEHE, profil, collector ) )
      return;

    if( !validateCulvertParameter( breite, BuildingEi.KEY_BREITE, profil, collector ) )
      return;

    if( !validateCulvertParameter( sohlgefaelle, BuildingEi.KEY_SOHLGEFAELLE, profil, collector ) )
      return;

    if( !validateCulvertParameter( rauheit, BuildingEi.KEY_RAUHEIT, profil, collector ) )
      return;
  }

  private void validateCulvertParameter( final BuildingKreis building, final IProfile profil, final IValidatorMarkerCollector collector ) throws CoreException
  {
    final Double bezugspunktX = building.getBezugspunktX();
    final Double bezugspunktY = building.getBezugspunktY();
    final Double breite = building.getBreite();
    final Double sohlgefaelle = building.getSohlgefaelle();
    final Double rauheit = building.getRauheit();

    if( !validateCulvertParameter( bezugspunktX, BuildingKreis.KEY_BEZUGSPUNKT_X, profil, collector ) )
      return;

    if( !validateCulvertParameter( bezugspunktY, BuildingKreis.KEY_BEZUGSPUNKT_Y, profil, collector ) )
      return;

    if( !validateCulvertParameter( breite, BuildingKreis.KEY_BREITE, profil, collector ) )
      return;

    if( !validateCulvertParameter( sohlgefaelle, BuildingKreis.KEY_SOHLGEFAELLE, profil, collector ) )
      return;

    if( !validateCulvertParameter( rauheit, BuildingKreis.KEY_RAUHEIT, profil, collector ) )
      return;
  }

  private void validateCulvertParameter( final BuildingMaul building, final IProfile profil, final IValidatorMarkerCollector collector ) throws CoreException
  {
    final Double bezugspunktX = building.getBezugspunktX();
    final Double bezugspunktY = building.getBezugspunktY();
    final Double hoehe = building.getHoehe();
    final Double breite = building.getBreite();
    final Double sohlgefaelle = building.getSohlgefaelle();
    final Double rauheit = building.getRauheit();

    if( !validateCulvertParameter( bezugspunktX, BuildingMaul.KEY_BEZUGSPUNKT_X, profil, collector ) )
      return;

    if( !validateCulvertParameter( bezugspunktY, BuildingMaul.KEY_BEZUGSPUNKT_Y, profil, collector ) )
      return;

    if( !validateCulvertParameter( hoehe, BuildingMaul.KEY_HOEHE, profil, collector ) )
      return;

    if( !validateCulvertParameter( breite, BuildingMaul.KEY_BREITE, profil, collector ) )
      return;

    if( !validateCulvertParameter( sohlgefaelle, BuildingMaul.KEY_SOHLGEFAELLE, profil, collector ) )
      return;

    if( !validateCulvertParameter( rauheit, BuildingMaul.KEY_RAUHEIT, profil, collector ) )
      return;
  }

  private void validateCulvertParameter( final BuildingTrapez building, final IProfile profil, final IValidatorMarkerCollector collector ) throws CoreException
  {
    final Double bezugspunktX = building.getBezugspunktX();
    final Double bezugspunktY = building.getBezugspunktY();
    final Double hoehe = building.getHoehe();
    final Double breite = building.getBreite();
    final Double steigung = building.getSteigung();
    final Double sohlgefaelle = building.getSohlgefaelle();
    final Double rauheit = building.getRauheit();

    if( !validateCulvertParameter( bezugspunktX, BuildingTrapez.KEY_BEZUGSPUNKT_X, profil, collector ) )
      return;

    if( !validateCulvertParameter( bezugspunktY, BuildingTrapez.KEY_BEZUGSPUNKT_Y, profil, collector ) )
      return;

    if( !validateCulvertParameter( hoehe, BuildingTrapez.KEY_HOEHE, profil, collector ) )
      return;

    if( !validateCulvertParameter( breite, BuildingTrapez.KEY_BREITE, profil, collector ) )
      return;

    if( !validateCulvertParameter( steigung, BuildingTrapez.KEY_STEIGUNG, profil, collector ) )
      return;

    if( !validateCulvertParameter( sohlgefaelle, BuildingTrapez.KEY_SOHLGEFAELLE, profil, collector ) )
      return;

    if( !validateCulvertParameter( rauheit, BuildingTrapez.KEY_RAUHEIT, profil, collector ) )
      return;
  }

  private boolean validateCulvertParameter( final Double oValue, final String propertyName, final IProfile profil, final IValidatorMarkerCollector collector ) throws CoreException
  {
    if( oValue == null || oValue.isNaN() )
    {
      collector.createProfilMarker( IMarker.SEVERITY_ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.DurchlassRule.4", propertyName ), String.format( "km %.4f", profil.getStation() ), 0, null ); //$NON-NLS-1$ //$NON-NLS-2$
      return false;
    }

    return true;
  }
}