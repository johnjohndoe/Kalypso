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
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.core.profil.validator.AbstractValidatorRule;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.tuhh.ui.resolutions.DelRoughnessResolution;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

/**
 * @author kimwerner
 */
public class RauheitRule extends AbstractValidatorRule
{
  public void validate( final IProfil profil, final IValidatorMarkerCollector collector ) throws CoreException
  {
    if( profil == null || istDurchlass( profil.getProfileObjects() ) )
      return;
    final String pluginId = PluginUtilities.id( KalypsoModelWspmTuhhUIPlugin.getDefault() );
    final String stationId = String.format( "km %.4f", profil.getStation() );//$NON-NLS-1$
    IComponent pointPropKS = profil.hasPointProperty( IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KS );
    IComponent pointPropKST = profil.hasPointProperty( IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KST );
    if( pointPropKS == null && pointPropKST == null )
    {
      collector.createProfilMarker( IMarker.SEVERITY_ERROR, Messages.getString("org.kalypso.model.wspm.tuhh.ui.rules.RauheitRule.3"), stationId, 0, "", pluginId );  //$NON-NLS-1$//$NON-NLS-2$
      return;
    }

    if( pointPropKS != null && pointPropKST != null )
    {
      collector.createProfilMarker( IMarker.SEVERITY_ERROR, Messages.getString("org.kalypso.model.wspm.tuhh.ui.rules.RauheitRule.1"), stationId, 0, "", pluginId ,new DelRoughnessResolution(new String[]{IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KS,IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KST},null));  //$NON-NLS-1$//$NON-NLS-2$
      return;
    }

    final IComponent pointProp = pointPropKST == null ? pointPropKS : pointPropKST;
    final IProfilPointMarker[] durchS = profil.getPointMarkerFor( profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE ) );

    if( durchS.length < 2 )
      return;
    final int leftD = profil.indexOfPoint( durchS[0].getPoint() );
    final int rightD = profil.indexOfPoint( durchS[durchS.length - 1].getPoint() );
    final IRecord[] points = profil.getPoints( leftD, rightD );

    if( points.length == 0 )
      return;

    for( final IRecord point : points )
    {
      final Double value = ProfilUtil.getDoubleValueFor( pointProp.getId(), point );
      if( value.isNaN() )
      {
        Object oVal = point.getValue( profil.indexOfProperty( pointProp ) );
        collector.createProfilMarker( IMarker.SEVERITY_ERROR,  Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.RauheitRule.0" , oVal ), stationId, profil.indexOfPoint( point ), pointProp.getId(), pluginId ); //$NON-NLS-1$ //$NON-NLS-2$
        break;
      }
      else if( value <= 0.0 )
      {
        collector.createProfilMarker( IMarker.SEVERITY_ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.RauheitRule.2", value ), stationId, profil.indexOfPoint( point ), pointProp.getId(), pluginId ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        break;
      }
    }
  }

  private final boolean istDurchlass( final IProfileObject[] objects )
  {
    if( objects == null || objects.length < 1 || objects[0] == null )
      return false;
    final String building = objects[0].getId();
    if( building.equals( IWspmTuhhConstants.BUILDING_TYP_EI ) || building.equals( IWspmTuhhConstants.BUILDING_TYP_MAUL ) || building.equals( IWspmTuhhConstants.BUILDING_TYP_KREIS )
        || building.equals( IWspmTuhhConstants.BUILDING_TYP_TRAPEZ ) )
      return true;
    return false;

  }
}
