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
package org.kalypso.model.hydrology.test;

import java.io.File;
import java.net.URL;
import java.util.logging.Logger;

import org.junit.Test;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.convert.namodel.NaSimulationData;
import org.kalypso.convert.namodel.manager.IDManager;
import org.kalypso.model.hydrology.internal.NaAsciiDirs;
import org.kalypso.model.hydrology.internal.preprocessing.NAModelPreprocessor;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.NullSimulationMonitor;

/**
 * Tests preprocessing of KalypsoHydrology simulation.
 * 
 * @author Gernot Belger
 */
public class NaPreprocessingTest
{
  /**
   * <code>
   * Optimize Log:
   * Initially: ~9.9s
   * 
   * 
   * </code>
   */
  @Test
  public void testDemoModel( ) throws Exception
  {
    final File outputDir = FileUtilities.createNewTempDir( "naDemoModelPreprocessingTest" );
    final File asciiDir = new File( outputDir, "ascii" );

    final NAConfiguration conf = new NAConfiguration( asciiDir );
    final NaAsciiDirs outputDirs = new NaAsciiDirs( asciiDir );
    final IDManager idManager = new IDManager();
    final NaSimulationData simulationData = createDemoModelsimulationData( outputDir );
    final Logger logger = Logger.getAnonymousLogger();

    final URL context = simulationData.getModelWorkspace().getContext();
    conf.setZMLContext( context );

    final NAModelPreprocessor preprocessor = new NAModelPreprocessor( conf, outputDirs, idManager, simulationData, logger );

    final ISimulationMonitor monitor = new NullSimulationMonitor();
    preprocessor.process( monitor );

    // TODO: compare with some expected results
  }

  private NaSimulationData createDemoModelsimulationData( final File outputDir ) throws Exception
  {
    final Class< ? extends NaPreprocessingTest> myClass = getClass();

    final URL modelUrl = myClass.getResource( "resources/demoModel_Langzeit/calcCase.gml" );
    final File newModellFile = new File( outputDir, "newModell.gml" );
    final URL controlUrl = myClass.getResource( "resources/demoModel_Langzeit/expertControl.gml" );
    final URL metaUrl = myClass.getResource( "resources/demoModel_Langzeit/.calculation" );
    final URL parameterUrl = myClass.getResource( "resources/demoModel_Langzeit/calcParameter.gml" );
    final URL hydrotopUrl = myClass.getResource( "resources/demoModel_Langzeit/calcHydrotop.gml" );
    final URL sudsUrl = null;
    final URL syntNUrl = null;
    final URL lzsimUrl = null;

    return new NaSimulationData( modelUrl, newModellFile, controlUrl, metaUrl, parameterUrl, hydrotopUrl, sudsUrl, syntNUrl, lzsimUrl );
  }

}
