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
import java.util.logging.Handler;
import java.util.logging.Logger;

import org.apache.commons.io.FileUtils;
import org.eclipse.compare.structuremergeviewer.Differencer;
import org.eclipse.compare.structuremergeviewer.ICompareInput;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.junit.Test;
import org.kalypso.commons.compare.DifferenceDumper;
import org.kalypso.commons.compare.FileContentAssertDumper;
import org.kalypso.commons.compare.IElementDumper;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.eclipse.compare.FileStructureComparator;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.core.preferences.IKalypsoCorePreferences;
import org.kalypso.model.hydrology.binding.HydrotopeCollection;
import org.kalypso.model.hydrology.binding.control.NAModellControl;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.binding.parameter.Parameter;
import org.kalypso.model.hydrology.internal.IDManager;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypso.model.hydrology.internal.NaAsciiDirs;
import org.kalypso.model.hydrology.internal.NaSimulationDirs;
import org.kalypso.model.hydrology.internal.postprocessing.NaPostProcessor;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.NaCatchmentData;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.ParameterHash;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author Gernot Belger
 */
public class NAPostprocessingTest
{
  public NAPostprocessingTest( )
  {
    KalypsoCorePlugin.getDefault().getPreferenceStore().setValue( IKalypsoCorePreferences.DISPLAY_TIMEZONE, "GMT+1" ); //$NON-NLS-1$
  }

  @Test
  public void testDemoModel( ) throws Exception
  {
    testPostprocessing( "naDemoModel", "/etc/test/resources/simulation/demoModel_Langzeit" ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  public void testPostprocessing( final String label, final String resourceBasePath ) throws Exception
  {
    // prepare (unzip)
    final File outputDir = FileUtilities.createNewTempDir( label + "PostprocessingTest" ); //$NON-NLS-1$
    final File asciiResults = prepareAsciiResults( resourceBasePath, outputDir );

    // run postprocessing
    final File resultsDir = doPostprocessing( resourceBasePath, outputDir, asciiResults );

    // compare results with expected
    final File expectedResultsDir = new File( outputDir, "expectedResults" ); //$NON-NLS-1$
    checkResult( resultsDir, expectedResultsDir );

    FileUtils.forceDelete( outputDir );
  }

  private File doPostprocessing( final String baseResourceLocation, final File outputDir, final File asciiBaseDir ) throws Exception
  {
    final File resultsDir = new File( outputDir, "results" ); //$NON-NLS-1$

    final URL gmlInputZipLocation = getClass().getResource( baseResourceLocation + "/gmlInput.zip" ); //$NON-NLS-1$
    final URL baseURL = new URL( String.format( "jar:%s!/", gmlInputZipLocation.toExternalForm() ) ); //$NON-NLS-1$

    final Logger logger = Logger.getAnonymousLogger();
    logger.setUseParentHandlers( false );
    final Handler[] handlers = logger.getHandlers();
    for( final Handler handler : handlers )
      logger.removeHandler( handler );

    final URL modelResource = new URL( baseURL, "modell.gml" ); //$NON-NLS-1$
    final GMLWorkspace modelWorkspace = GmlSerializer.createGMLWorkspace( modelResource, null );

    final URL parameterResource = new URL( baseURL, "parameter.gml" ); //$NON-NLS-1$
    final GMLWorkspace parameterWorkspace = GmlSerializer.createGMLWorkspace( parameterResource, null );
    final Parameter parameter = (Parameter)parameterWorkspace.getRootFeature();

    final URL controlResource = new URL( baseURL, "expertControl.gml" ); //$NON-NLS-1$
    final GMLWorkspace controlWorkspace = GmlSerializer.createGMLWorkspace( controlResource, null );
    final NAModellControl naControl = (NAModellControl)controlWorkspace.getRootFeature();

    final NaAsciiDirs naAsciiDirs = new NaAsciiDirs( asciiBaseDir );
    final NaSimulationDirs naSimulationDirs = new NaSimulationDirs( resultsDir );

    final URL hydrotopResource = new URL( baseURL, "hydrotop.gml" ); //$NON-NLS-1$
    final GMLWorkspace hydrotopWorkspace = GmlSerializer.createGMLWorkspace( hydrotopResource, null );
    final HydrotopeCollection naHydrotop = (HydrotopeCollection)hydrotopWorkspace.getRootFeature();

    final NaModell model = (NaModell)modelWorkspace.getRootFeature();

    final ParameterHash landuseHash = new ParameterHash( parameter );

    final NaCatchmentData catchmentData = new NaCatchmentData( landuseHash );
    catchmentData.addHydrotopes( model, naHydrotop, false );

    // REMARK: for backards compatibility: touch each catchment id, so it is known
    final IDManager idManager = new IDManager();
    final Catchment[] catchments = catchmentData.getCatchments();
    for( final Catchment catchment : catchments )
      idManager.getAsciiID( catchment );

    final MultiStatus processStatus = new MultiStatus( ModelNA.PLUGIN_ID, 0, "Processing", null ); //$NON-NLS-1$

    final NaPostProcessor postProcessor = new NaPostProcessor( idManager, modelWorkspace, naControl, catchmentData, processStatus );
    postProcessor.process( naAsciiDirs, naSimulationDirs );

    return resultsDir;
  }

  private File prepareAsciiResults( final String resourceBasePath, final File outputDir ) throws IOException
  {
    final File asciiDir = new File( outputDir, "ascii" ); //$NON-NLS-1$
    asciiDir.mkdirs();
    final URL asciiResultsResource = getClass().getResource( resourceBasePath + "/asciiAfterCalc.zip" ); //$NON-NLS-1$
    ZipUtilities.unzip( asciiResultsResource, asciiDir );
    return asciiDir;
  }

  private void checkResult( final File resultsDir, final File resultExpectedDir ) throws IOException
  {
    /* Fetch the expected results */
    resultExpectedDir.mkdirs();
    ZipUtilities.unzip( getClass().getResource( "/etc/test/resources/simulation/demoModel_Langzeit/results.zip" ), resultExpectedDir ); //$NON-NLS-1$

    /* compare with expected results */
    final File currentResultsDir = new File( resultsDir, "results/Ergebnisse" ); //$NON-NLS-1$
    final FileStructureComparator actualComparator = new FileStructureComparator( currentResultsDir );
    final FileStructureComparator expectedComparator = new FileStructureComparator( resultExpectedDir );

    final Differencer differencer = new Differencer();
    final Object differences = differencer.findDifferences( false, new NullProgressMonitor(), null, null, expectedComparator, actualComparator );

    final IElementDumper elementDumper = new IElementDumper()
    {
      @Override
      public void dumpElement( final ICompareInput input )
      {
        // We ignore this gml file at the moment. It is different, as the gml-id changes each time.
        if( "20020728.gml".equals( input.getName() ) ) //$NON-NLS-1$
          return;

        new FileContentAssertDumper().dumpElement( input );
      }
    };

    final DifferenceDumper dumper = new DifferenceDumper( differences, elementDumper );
    dumper.dumpDifferences();
  }
}
