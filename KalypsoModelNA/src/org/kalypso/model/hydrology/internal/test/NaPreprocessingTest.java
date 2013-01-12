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
import org.junit.Before;
import org.junit.Test;
import org.kalypso.commons.compare.DifferenceDumper;
import org.kalypso.commons.compare.FileContentAssertDumper;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.java.net.UrlUtilities;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.eclipse.compare.FileStructureComparator;
import org.kalypso.core.IKalypsoCoreConstants;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.core.preferences.IKalypsoCorePreferences;
import org.kalypso.model.hydrology.INaSimulationData;
import org.kalypso.model.hydrology.NaSimulationDataFactory;
import org.kalypso.model.hydrology.internal.NaAsciiDirs;
import org.kalypso.model.hydrology.internal.preprocessing.NAModelPreprocessor;
import org.kalypso.simulation.core.NullSimulationMonitor;
import org.osgi.framework.Version;

/**
 * Tests preprocessing of KalypsoHydrology simulation.
 * 
 * @author Gernot Belger
 */
public class NaPreprocessingTest
{
  public NaPreprocessingTest( )
  {
    KalypsoCorePlugin.getDefault().getPreferenceStore().setValue( IKalypsoCorePreferences.DISPLAY_TIMEZONE, "GMT+1" ); //$NON-NLS-1$
  }

  @Before
  public void init( )
  {
    System.setProperty( IKalypsoCoreConstants.CONFIG_PROPERTY_TIMEZONE, "GMT+1" ); //$NON-NLS-1$
  }

  @Test
  public void testDemoModel( ) throws Exception
  {
    testRunPreprocessing( "naDemoModel", "/etc/test/resources/simulation/demoModel_Langzeit" ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  @Test
  public void testWeisseElsterLangzeit( ) throws Exception
  {
    testRunPreprocessing( "WeisseElsterLangzeit", "/etc/test/resources/simulation/weisseElster_langzeit" ); //$NON-NLS-1$ //$NON-NLS-2$
  }

// @Test
// public void testWeisseElsterLangzeitPerformance( ) throws Exception
// {
// for( int i = 0; i < 10; i++ )
// testRunPreprocessing( "WeisseElsterLangzeit", "/etc/test/resources/simulation/weisseElster_langzeit" );
// }

  private void testRunPreprocessing( final String label, final String baseResourceLocation ) throws Exception
  {
    final File outputDir = FileUtilities.createNewTempDir( label + "PreprocessingTest" ); //$NON-NLS-1$
    final File asciiDir = new File( outputDir, "ascii" ); //$NON-NLS-1$
    final File asciiExpectedDir = new File( outputDir, "asciiExpected" ); //$NON-NLS-1$

    final NAModelPreprocessor preprocessor = initPreprocessor( baseResourceLocation, asciiDir );

    preprocessor.process( new NullSimulationMonitor() );

    final File idMapFile = new File( outputDir, "IdMap.txt" ); //$NON-NLS-1$
    preprocessor.getIdManager().dump( idMapFile );

    checkResult( baseResourceLocation, asciiDir, asciiExpectedDir );

    FileUtils.forceDelete( outputDir );
  }

  private NAModelPreprocessor initPreprocessor( final String baseResourceLocation, final File asciiDir ) throws Exception
  {
    final NaAsciiDirs outputDirs = new NaAsciiDirs( asciiDir );
    final Logger logger = Logger.getAnonymousLogger();

    final URL gmlInputZipLocation = getClass().getResource( baseResourceLocation + "/gmlInput.zip" ); //$NON-NLS-1$
    final URL baseURL = new URL( String.format( "jar:%s!/", gmlInputZipLocation.toExternalForm() ) ); //$NON-NLS-1$

    final INaSimulationData simulationData = createDemoModelsimulationData( baseURL );

    final Version calcCoreVersion = new Version( "2.2.0.1" ); //$NON-NLS-1$

    return new NAModelPreprocessor( outputDirs, simulationData, calcCoreVersion, logger );
  }

  private INaSimulationData createDemoModelsimulationData( final URL base ) throws Exception
  {
    final URL modelUrl = new URL( base, "modell.gml" ); //$NON-NLS-1$
    final URL controlUrl = new URL( base, "expertControl.gml" ); //$NON-NLS-1$
    final URL metaUrl = new URL( base, ".calculation" ); //$NON-NLS-1$
    final URL parameterUrl = new URL( base, "parameter.gml" ); //$NON-NLS-1$
    final URL hydrotopUrl = new URL( base, "hydrotop.gml" ); //$NON-NLS-1$
    final URL syntNUrl = null;
    final URL lzsimUrl = checkUrlExists( new URL( base, "Anfangswerte/lzsim.gml" ) ); //$NON-NLS-1$

    return NaSimulationDataFactory.load( modelUrl, controlUrl, metaUrl, parameterUrl, hydrotopUrl, syntNUrl, lzsimUrl, null, null, null, null );
  }

  private URL checkUrlExists( final URL url )
  {
    if( UrlUtilities.checkIsAccessible( url ) )
      return url;

    return null;
  }

  private void checkResult( final String baseResourceLocation, final File asciiDir, final File asciiExpectedDir ) throws IOException
  {
    /* Fetch the expected results */
    asciiExpectedDir.mkdir();
    ZipUtilities.unzip( getClass().getResource( baseResourceLocation + "/expectedAscii.zip" ), asciiExpectedDir ); //$NON-NLS-1$
    // FIXED: make sure this directory always exists, as this will always exist in the results; but we cannot unzip an
    // empty directory...
    new File( asciiExpectedDir, "lzsim" ).mkdirs(); //$NON-NLS-1$

    checkDifferences( asciiExpectedDir, asciiDir );

  }

  // FIXME: move into helper
  static void checkDifferences( final File expectedDir, final File actualDir )
  {
    /* compare with expected results */
    final FileStructureComparator actualComparator = new FileStructureComparator( actualDir );
    final FileStructureComparator expectedComparator = new FileStructureComparator( expectedDir );

    final Differencer differencer = new Differencer();
    final Object differences = differencer.findDifferences( false, new NullProgressMonitor(), null, null, expectedComparator, actualComparator );
    dumpDifferences( differences );
  }

  static void dumpDifferences( final Object differences )
  {
    final FileContentAssertDumper elementDumper = new FileContentAssertDumper();
    final DifferenceDumper differenceDumper = new DifferenceDumper( differences, elementDumper );
    differenceDumper.dumpDifferences();

    if( differenceDumper.hasDifferences() )
      Assert.fail( "Expected ascii files are different from actual ones. See console dump" ); //$NON-NLS-1$
  }
}
