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
package org.kalypso.risk.model.simulation;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.kalypso.simulation.core.simspec.Modeldata;
import org.kalypso.simulation.core.util.SimulationUtilitites;

/**
 * @author Antanaskovic Dejan
 * 
 */
public class SimulationKalypsoRiskModelspecHelper implements ISimulationSpecKalypsoRisk
{
  public static final Modeldata getModeldata( final SIMULATION_KALYPSORISK_TYPEID typeID )
  {
    final List<MODELSPEC_KALYPSORISK> inputs = new ArrayList<MODELSPEC_KALYPSORISK>();
    inputs.add( MODELSPEC_KALYPSORISK.CONTROL_MODEL );
    inputs.add( MODELSPEC_KALYPSORISK.RASTER_MODEL );
    inputs.add( MODELSPEC_KALYPSORISK.VECTOR_MODEL );
    inputs.add( MODELSPEC_KALYPSORISK.INPUT_RASTER_FOLDER );
    inputs.add( MODELSPEC_KALYPSORISK.OUTPUT_RASTER_FOLDER );

    final List<MODELSPEC_KALYPSORISK> outputs = new ArrayList<MODELSPEC_KALYPSORISK>();
    outputs.add( MODELSPEC_KALYPSORISK.RASTER_MODEL );
    outputs.add( MODELSPEC_KALYPSORISK.OUTPUT_RASTER_FOLDER );

    return SimulationUtilitites.createModelData( typeID.getValue(), getMap( inputs ), true, getMap( outputs ), true );
  }

  public static final Map<String, String> getMap( final List<MODELSPEC_KALYPSORISK> keys )
  {
    final Map<String, String> map = new HashMap<String, String>();
    for( final MODELSPEC_KALYPSORISK key : keys )
      map.put( key.name(), key.getValue() );
    return map;
  }
}
