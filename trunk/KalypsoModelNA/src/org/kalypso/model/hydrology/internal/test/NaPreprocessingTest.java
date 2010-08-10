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
package org.kalypso.model.hydrology.internal.test;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.logging.Logger;

import junit.framework.Assert;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.ObjectUtils;
import org.eclipse.compare.structuremergeviewer.Differencer;
import org.eclipse.compare.structuremergeviewer.IDiffElement;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.junit.Test;
import org.kalypso.commons.compare.CompareUtils;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.eclipse.compare.FileStructureComparator;
import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.convert.namodel.NaSimulationData;
import org.kalypso.convert.namodel.manager.IDManager;
import org.kalypso.model.hydrology.binding.NAControl;
import org.kalypso.model.hydrology.internal.NaAsciiDirs;
import org.kalypso.model.hydrology.internal.preprocessing.NAModelPreprocessor;
import org.kalypso.simulation.core.NullSimulationMonitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * Tests preprocessing of KalypsoHydrology simulation.
 * 
 * @author Gernot Belger
 */
public class NaPreprocessingTest
{
  @Test
  public void testDemoModel( ) throws Exception
  {
    final File outputDir = FileUtilities.createNewTempDir( "naDemoModelPreprocessingTest" );
    final File asciiDir = new File( outputDir, "ascii" );
    final File asciiExpectedDir = new File( outputDir, "asciiExpected" );

    final NAModelPreprocessor preprocessor = initPreprocessor( asciiDir );

    preprocessor.process( new NullSimulationMonitor() );

    checkResult( asciiDir, asciiExpectedDir );

    FileUtils.forceDelete( outputDir );
  }

  private NAModelPreprocessor initPreprocessor( final File asciiDir ) throws Exception
  {

    final NaAsciiDirs outputDirs = new NaAsciiDirs( asciiDir );
    final IDManager idManager = new IDManager();
    final NaSimulationData simulationData = createDemoModelsimulationData();

    final NAConfiguration conf = createNAConfiguration( asciiDir, simulationData );

    final URL context = simulationData.getModelWorkspace().getContext();
    conf.setZMLContext( context );

    // FIXME: let preprocessor log into normal logfile!
    final Logger logger = Logger.getAnonymousLogger();
    final NAModelPreprocessor preprocessor = new NAModelPreprocessor( conf, outputDirs, idManager, simulationData, logger );
    return preprocessor;
  }

  private NAConfiguration createNAConfiguration( final File asciiDir, final NaSimulationData simulationData )
  {
    final NAConfiguration conf = new NAConfiguration( asciiDir );

    // FIXME: remove these setters/getters from m_conf
    // we should not use the NAConfiguration any more, as long as we do
    // we need the workspaces
    final NAControl metaControl = simulationData.getMetaControl();
    final GMLWorkspace modelWorkspace = simulationData.getModelWorkspace();
    final GMLWorkspace hydrotopWorkspace = simulationData.getHydrotopWorkspace();
    final GMLWorkspace sudsWorkspace = simulationData.getSudsWorkspace();
    final GMLWorkspace parameterWorkspace = simulationData.getParameterWorkspace();
    final GMLWorkspace synthNWorkspace = simulationData.getSynthNWorkspace();

    conf.setMetaControl( metaControl );
    conf.setModelWorkspace( modelWorkspace );
    conf.setParameterWorkspace( parameterWorkspace );
    conf.setHydrotopeWorkspace( hydrotopWorkspace );
    conf.setSynthNWorkspace( synthNWorkspace );
    conf.setSudsWorkspace( sudsWorkspace );

    return conf;
  }

  private NaSimulationData createDemoModelsimulationData( ) throws Exception
  {
    final Class< ? extends NaPreprocessingTest> myClass = getClass();

    final URL modelUrl = myClass.getResource( "resources/demoModel_Langzeit/gmlInput/calcCase.gml" );
    final URL controlUrl = myClass.getResource( "resources/demoModel_Langzeit/gmlInput/expertControl.gml" );
    final URL metaUrl = myClass.getResource( "resources/demoModel_Langzeit/gmlInput/.calculation" );
    final URL parameterUrl = myClass.getResource( "resources/demoModel_Langzeit/gmlInput/calcParameter.gml" );
    final URL hydrotopUrl = myClass.getResource( "resources/demoModel_Langzeit/gmlInput/calcHydrotop.gml.gz" );
    final URL sudsUrl = null;
    final URL syntNUrl = null;
    final URL lzsimUrl = null;

    return new NaSimulationData( modelUrl, controlUrl, metaUrl, parameterUrl, hydrotopUrl, sudsUrl, syntNUrl, lzsimUrl );
  }

  private void checkResult( final File asciiDir, final File asciiExpectedDir ) throws IOException
  {
    /* Fetch the expected results */
    asciiExpectedDir.mkdir();
    ZipUtilities.unzip( getClass().getResource( "resources/demoModel_Langzeit/expectedAscii.zip" ), asciiExpectedDir );

    /* compare with expected results */
    final FileStructureComparator actualComparator = new FileStructureComparator( asciiDir );
    final FileStructureComparator expectedComparator = new FileStructureComparator( asciiExpectedDir );

    final Differencer differencer = new Differencer();
    final Object differences = differencer.findDifferences( false, new NullProgressMonitor(), null, null, expectedComparator, actualComparator );
    dumpDifferences( differences );
  }

  private void dumpDifferences( final Object differences )
  {
    if( differences == null )
      return;

    if( !(differences instanceof IDiffElement) )
      Assert.fail( "Unknown differencer result: " + ObjectUtils.toString( differences ) );

    final IDiffElement element = (IDiffElement) differences;
    CompareUtils.dumpDiffElement( element, 0 );

    Assert.fail( "Expected ascii files are different from actual ones. See console dump" );
  }
}
