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
package org.kalypso.floodrisk.rasterize.test;

import junit.framework.TestCase;

import org.kalypso.floodrisk.process.impl.ProcessDataProvider;
import org.kalypso.floodrisk.process.impl.ProcessResultEater;
import org.kalypso.floodrisk.rasterize.RasterizeLanduseJob;
import org.kalypso.simulation.core.ISimulationConstants;
import org.kalypso.simulation.core.SimulationDataPath;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.simulation.core.SimulationInfo;

public class RasterizeLanduseJobTest extends TestCase
{

  public void testRun( ) throws SimulationException
  {
    RasterizeLanduseJob job = new RasterizeLanduseJob();
    testRasterizeLanduse( job );
    testGetSpezifikation( job );
  }

  private void testRasterizeLanduse( RasterizeLanduseJob job ) throws SimulationException
  {
    final String base = "D://__test//";
    final SimulationDataPath[] input = new SimulationDataPath[3];
    final SimulationDataPath[] output = new SimulationDataPath[2];
    input[0] = new SimulationDataPath( RasterizeLanduseJob.LanduseVectorDataID, base + "Landuse//LanduseVectorData.gml" );
    input[1] = new SimulationDataPath( RasterizeLanduseJob.ContextModelID, base + "Control//contextModell.gml" );
    input[2] = new SimulationDataPath( RasterizeLanduseJob.BaseRasterID, base + "Waterlevel//wsp_hq100.gml" );
    output[0] = new SimulationDataPath( RasterizeLanduseJob.LanduseGmlFilePath, base + "Landuse//landuseData.gml" );
    output[1] = new SimulationDataPath( RasterizeLanduseJob.LanduseRasterFilePath, base + "Landuse//landuseRaster.dat" );

    final ProcessDataProvider inputProvider = new ProcessDataProvider( input );
    final ProcessResultEater resultEater = new ProcessResultEater( output );
    final SimulationInfo jobBean = new SimulationInfo( "", "", "RasterizeLanduseJob", ISimulationConstants.STATE.RUNNING, -1, "" );
    job.run( null, inputProvider, resultEater, jobBean );
  }

  public void testGetSpezifikation( RasterizeLanduseJob job )
  {
    System.out.println( "Spezifikation: " + job.getSpezifikation() );
  }

}