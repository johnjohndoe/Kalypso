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
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.validator.AbstractValidatorRule;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.IProfileBuilding;
import org.kalypso.model.wspm.tuhh.core.util.WspmProfileHelper;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.tuhh.ui.resolutions.DelRoughnessResolution;
import org.kalypso.observation.result.IComponent;

/**
 * @author kimwerner
 */
public class DurchlassRule extends AbstractValidatorRule
{
  @Override
  public void validate( final IProfil profil, final IValidatorMarkerCollector collector ) throws CoreException
  {
    final IProfileBuilding building = WspmProfileHelper.getBuilding( profil, IProfileBuilding.class );
    if( building == null )
      return;

    if( IWspmTuhhConstants.BUILDING_TYP_BRUECKE.equals( building.getId() ) )
      return;
    else if( IWspmTuhhConstants.BUILDING_TYP_WEHR.equals( building.getId() ) )
      return;

    else if( IWspmTuhhConstants.BUILDING_TYP_EI.equals( building.getId() ) )
    {
      final Object b = building.getValue( building.getObjectProperty( IWspmTuhhConstants.BUILDING_PROPERTY_BREITE ) );
      final Object h = building.getValue( building.getObjectProperty( IWspmTuhhConstants.BUILDING_PROPERTY_HOEHE ) );
      if( b instanceof Double && h instanceof Double && (Double) h <= (Double) b )
        collector.createProfilMarker( IMarker.SEVERITY_ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.DurchlassRule.0" ), String.format( "km %.4f", profil.getStation() ), 0, null ); //$NON-NLS-1$ //$NON-NLS-2$
    }
    else if( IWspmTuhhConstants.BUILDING_TYP_MAUL.equals( building.getId() ) )
    {
      final Object b = building.getValue( building.getObjectProperty( IWspmTuhhConstants.BUILDING_PROPERTY_BREITE ) );
      final Object h = building.getValue( building.getObjectProperty( IWspmTuhhConstants.BUILDING_PROPERTY_HOEHE ) );
      if( b instanceof Double && h instanceof Double && (Double) b <= (Double) h )
        collector.createProfilMarker( IMarker.SEVERITY_ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.DurchlassRule.2" ), String.format( "km %.4f", profil.getStation() ), 0, null ); //$NON-NLS-1$ //$NON-NLS-2$

    }
    final IComponent compKS = profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_RAUHEIT_KS );
    final IComponent compKST = profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_RAUHEIT_KST );
    if( compKS != null )
    {
      collector.createProfilMarker( IMarker.SEVERITY_WARNING, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.DurchlassRule.1", compKS.getName() ), String.format( "km %.4f", profil.getStation() ), 0, null, new DelRoughnessResolution( new String[] {}, IWspmConstants.POINT_PROPERTY_RAUHEIT_KS ) ); //$NON-NLS-1$//$NON-NLS-2$ 
    }
    if( compKST != null )
    {
      collector.createProfilMarker( IMarker.SEVERITY_WARNING, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.DurchlassRule.1", compKST.getName() ), String.format( "km %.4f", profil.getStation() ), 0, null, new DelRoughnessResolution( new String[] {}, IWspmConstants.POINT_PROPERTY_RAUHEIT_KST ) ); //$NON-NLS-1$//$NON-NLS-2$ 
    }
    for( final IComponent property : building.getObjectProperties() )
    {
      final Object oValue = building.getValue( property );
      if( oValue == null || ((Double) oValue).isNaN() )
      {
        collector.createProfilMarker( IMarker.SEVERITY_ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.DurchlassRule.4", property.getName() ), String.format( "km %.4f", profil.getStation() ), 0, null ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        break;
      }
    }
  }
}
