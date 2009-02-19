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
package org.kalypso.gaja3d.simulation;

import java.io.File;
import java.net.URL;
import java.util.ArrayList;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.kalypso.commons.bind.JaxbUtilities;
import org.kalypso.gaja3d.simulation.grid.Gaja3dGridJobSubmitter;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.simulation.core.simspec.Modelspec;

/**
 * @author kurzbach
 */
public class CreateTinSimulation implements ISimulation
{
  /**
   * The model specification.
   */
  private static final String SIMULATION_SPEC = "createTin_specification.xml";

  public static final String INPUT_BOUNDARY = "Boundary";

  public static final String INPUT_BREAKLINES = "Breaklines";

  public static final String INPUT_MAX_AREA = "MaxArea";

  public static final String INPUT_MIN_ANGLE = "MinAngle";
  
  public static final String INPUT_DEM_GRID = "DemGrid";

  public static final String OUTPUT_MODEL_TIN = "ModelTin";

  public static final String ID = "Gaja3d_createTin";

  /**
   * The constructor.
   */
  public CreateTinSimulation( )
  {
  }

  /**
   * @see org.kalypso.simulation.core.ISimulation#getSpezifikation()
   */
  public URL getSpezifikation( )
  {
    return getClass().getResource( SIMULATION_SPEC );
  }

  /**
   * @see org.kalypso.simulation.core.ISimulation#run(java.io.File, org.kalypso.simulation.core.ISimulationDataProvider,
   *      org.kalypso.simulation.core.ISimulationResultEater, org.kalypso.simulation.core.ISimulationMonitor)
   */
  public void run( final File tmpdir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    final URL spezifikation = getSpezifikation();
    Modelspec modelSpec = null;
    try
    {
      final Unmarshaller unmarshaller = JaxbUtilities.createQuiet( org.kalypso.simulation.core.simspec.ObjectFactory.class ).createUnmarshaller();
      modelSpec = (Modelspec) unmarshaller.unmarshal( spezifikation );
    }
    catch( final JAXBException e )
    {
      throw new SimulationException( "Could not read model spec.", e );
    }

    final ArrayList<String> arguments = new ArrayList<String>();
    arguments.add( "createTin" );
    arguments.add( "true" );
    
    if(inputProvider.hasID( INPUT_DEM_GRID )) {
      arguments.add( "assignElevations" );
      arguments.add( "true" );
    }

    final Gaja3dGridJobSubmitter jobSubmitter = new Gaja3dGridJobSubmitter();
    jobSubmitter.submitJob( modelSpec, tmpdir, inputProvider, resultEater, monitor, arguments );
  }
}