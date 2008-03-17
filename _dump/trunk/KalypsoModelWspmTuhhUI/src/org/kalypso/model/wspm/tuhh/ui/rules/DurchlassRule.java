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

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.validator.AbstractValidatorRule;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.observation.result.IComponent;

/**
 * @author kimwerner
 */
public class DurchlassRule extends AbstractValidatorRule
{
  public void validate( final IProfil profil, final IValidatorMarkerCollector collector ) throws CoreException
  {

    final IProfileObject[] profileObjects = profil.getProfileObjects();
    IProfileObject building = null;
    if( profileObjects.length > 0 )
      building = profileObjects[0];

    if( (profil == null) || (building == null) )
      return;
    final String pluginId = PluginUtilities.id( KalypsoModelWspmTuhhUIPlugin.getDefault() );
    if( IWspmTuhhConstants.BUILDING_TYP_BRUECKE.equals( building.getId() ) )
      return;
    else if( IWspmTuhhConstants.BUILDING_TYP_WEHR.equals( building.getId() ) )
      return;

    else if( IWspmTuhhConstants.BUILDING_TYP_EI.equals( building.getId() ) )
    {
      final Double b = (Double) building.getValue( building.getObjectProperty( IWspmTuhhConstants.BUILDING_PROPERTY_BREITE ) );
      final Double h = (Double) building.getValue( building.getObjectProperty( IWspmTuhhConstants.BUILDING_PROPERTY_HOEHE ) );
      if( h <= b )
        collector.createProfilMarker( IMarker.SEVERITY_ERROR, "Eiprofil muss per Definition höher sein als breit", "km "+Double.toString( profil.getStation()), 0, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE, pluginId, null );
    }
    else if( IWspmTuhhConstants.BUILDING_TYP_MAUL.equals( building.getId() ) )
    {
      final Double b = (Double) building.getValue( building.getObjectProperty( IWspmTuhhConstants.BUILDING_PROPERTY_BREITE ) );
      final Double h = (Double) building.getValue( building.getObjectProperty( IWspmTuhhConstants.BUILDING_PROPERTY_HOEHE ) );
      if( b <= h )
        collector.createProfilMarker( IMarker.SEVERITY_ERROR, "Maulprofil muss per Definition breiter sein als hoch", "km "+Double.toString( profil.getStation()), 0, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE, pluginId, null );

    }

    try
    {
      for( final IComponent property : building.getObjectProperties() )
      {
        final Object oValue = building.getValue( property );
        if( oValue == null || ((Double) oValue).isNaN() )
        {
          collector.createProfilMarker( IMarker.SEVERITY_ERROR, "Parameter <" + property.getName() + "> fehlt", "km "+Double.toString( profil.getStation()), 0, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE, pluginId, null );
          break;
        }
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new CoreException( new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getDefault().getBundle().getSymbolicName(), 0, "Profilfehler", e ) );
    }

  }

}
