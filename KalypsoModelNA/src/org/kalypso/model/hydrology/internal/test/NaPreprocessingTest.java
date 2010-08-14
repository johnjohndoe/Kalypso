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
import org.eclipse.compare.structuremergeviewer.Differencer;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.junit.Test;
import org.kalypso.commons.compare.DifferenceDumper;
import org.kalypso.commons.compare.FileContentAssertDumper;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.eclipse.compare.FileStructureComparator;
import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.convert.namodel.NaSimulationData;
import org.kalypso.convert.namodel.manager.IDManager;
import org.kalypso.model.hydrology.internal.NaAsciiDirs;
import org.kalypso.model.hydrology.internal.preprocessing.NAModelPreprocessor;
import org.kalypso.simulation.core.NullSimulationMonitor;

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
    testRunPreprocessing( "naDemoModel", "resources/demoModel_Langzeit" );
  }

  @Test
  public void testWeisseElsterLangzeit( ) throws Exception
  {
    testRunPreprocessing( "WeisseElsterLangzeit", "resources/weisseElster_langzeit" );
  }

  @Test
  public void testWeisseElsterLangzeitPerformance( ) throws Exception
  {
    for( int i = 0; i < 10; i++ )
      testRunPreprocessing( "WeisseElsterLangzeit", "resources/weisseElster_langzeit" );
  }

  private void testRunPreprocessing( final String label, final String baseResourceLocation ) throws Exception
  {
    final File outputDir = FileUtilities.createNewTempDir( label + "PreprocessingTest" );
    final File asciiDir = new File( outputDir, "ascii" );
    final File asciiExpectedDir = new File( outputDir, "asciiExpected" );

    final NAConfiguration conf = new NAConfiguration( asciiDir );

    final NAModelPreprocessor preprocessor = initPreprocessor( baseResourceLocation, asciiDir, conf );

    preprocessor.process( new NullSimulationMonitor() );


    final File idMapFile = new File( outputDir, "IdMap.txt" ); //$NON-NLS-1$
    conf.getIdManager().dump( idMapFile );

    checkResult( baseResourceLocation, asciiDir, asciiExpectedDir );

    FileUtils.forceDelete( outputDir );
  }

  private NAModelPreprocessor initPreprocessor( final String baseResourceLocation, final File asciiDir, final NAConfiguration conf ) throws Exception
  {
    final NaAsciiDirs outputDirs = new NaAsciiDirs( asciiDir );

    final URL gmlInputZipLocation = getClass().getResource( baseResourceLocation + "/gmlInput.zip" );
    final URL baseURL = new URL( String.format( "jar:%s!/", gmlInputZipLocation.toExternalForm() ) );

    final NaSimulationData simulationData = createDemoModelsimulationData( baseURL );

    conf.setSimulationData( simulationData );

    final URL context = simulationData.getModelWorkspace().getContext();
    conf.setZMLContext( context );

    final Logger logger = Logger.getAnonymousLogger();
    final IDManager idManager = conf.getIdManager();
    final NAModelPreprocessor preprocessor = new NAModelPreprocessor( conf, outputDirs, idManager, simulationData, logger );
    return preprocessor;
  }

  private NaSimulationData createDemoModelsimulationData( final URL base ) throws Exception
  {
    final URL modelUrl = new URL( base, "calcCase.gml" );
    final URL controlUrl = new URL( base, "expertControl.gml" );
    final URL metaUrl = new URL( base, ".calculation" );
    final URL parameterUrl = new URL( base, "calcParameter.gml" );
    final URL hydrotopUrl = new URL( base, "calcHydrotop.gml" );
    final URL sudsUrl = null;
    final URL syntNUrl = null;
    final URL lzsimUrl = null;

    return new NaSimulationData( modelUrl, controlUrl, metaUrl, parameterUrl, hydrotopUrl, sudsUrl, syntNUrl, lzsimUrl );
  }

  private void checkResult( final String baseResourceLocation, final File asciiDir, final File asciiExpectedDir ) throws IOException
  {
    /* Fetch the expected results */
    asciiExpectedDir.mkdir();
    ZipUtilities.unzip( getClass().getResource( baseResourceLocation + "/expectedAscii.zip" ), asciiExpectedDir );

    /* compare with expected results */
    final FileStructureComparator actualComparator = new FileStructureComparator( asciiDir );
    final FileStructureComparator expectedComparator = new FileStructureComparator( asciiExpectedDir );

    final Differencer differencer = new Differencer();
    final Object differences = differencer.findDifferences( false, new NullProgressMonitor(), null, null, expectedComparator, actualComparator );
    dumpDifferences( differences );
  }

  private static void dumpDifferences( final Object differences )
  {
    final FileContentAssertDumper elementDumper = new FileContentAssertDumper();
    final DifferenceDumper differenceDumper = new DifferenceDumper( differences, elementDumper );
    differenceDumper.dumpDifferences();

    if( differenceDumper.hasDifferences() )
      Assert.fail( "Expected ascii files are different from actual ones. See console dump" );
  }
}
