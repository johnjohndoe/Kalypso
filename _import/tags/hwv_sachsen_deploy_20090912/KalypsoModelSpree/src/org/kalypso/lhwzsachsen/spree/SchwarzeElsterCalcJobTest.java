/*--------------- Kalypso-Header ------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 --------------------------------------------------------------------------*/

package org.kalypso.lhwzsachsen.spree;

import java.io.File;
import java.net.URL;
import java.util.logging.Level;

import junit.framework.TestCase;

import org.kalypso.commons.diff.DiffComparatorRegistry;
import org.kalypso.commons.diff.DiffUtils;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.java.util.logging.ILogger;
import org.kalypso.ogc.sensor.zml.diff.ZMLDiffComparator;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.SimulationInfo;
import org.kalypso.simulation.core.util.DefaultSimulationDataProvider;
import org.kalypso.simulation.core.util.DefaultSimulationResultEater;

/**
 * @author Gernot Belger
 */
public class SchwarzeElsterCalcJobTest extends TestCase
{
  @Override
  public void setUp( ) throws Exception
  {
    super.setUp();

    DiffComparatorRegistry.getInstance().register( ".zml", new ZMLDiffComparator() );
  }

  public void testCalcJob( ) throws Exception
  {
    final File tmpDir = FileUtilities.createNewTempDir( "SchwarzeElsterCalcJobTest" );

    final SchwarzeElsterCalcJob job = new SchwarzeElsterCalcJob();
    final ISimulationMonitor monitor = new SimulationInfo();

    final String resourceBase = "/" + getClass().getPackage().getName().replace( '.', '/' ) + "/resources/test/schwarzeelster/Ereignis95/";

    final ClassLoader classLoader = getClass().getClassLoader();
    final DefaultSimulationDataProvider inputProvider = new DefaultSimulationDataProvider();
    inputProvider.put( "CONTROL_GML", classLoader.getResource( resourceBase + ".calculation" ) );
    inputProvider.put( "GML", classLoader.getResource( resourceBase + "calcCase.gml" ) );
    inputProvider.put( "ZML", classLoader.getResource( resourceBase + "Zeitreihen" ) );

    final DefaultSimulationResultEater resultEater = new DefaultSimulationResultEater();

    final File jobTmpDir = new File( tmpDir, "jobTmp" );
    job.run( jobTmpDir, inputProvider, resultEater, monitor );

    compareResults( resultEater );
    FileUtilities.deleteRecursive( tmpDir );
  }

  private void compareResults( final DefaultSimulationResultEater resultEater ) throws Exception
  {
    // nur die Zeitreihen vergleichen
    final File resultDir = (File) resultEater.getResult( "ERGEBNISSE" );
    final URL resultURL = getClass().getResource( "resources/test/schwarzeelster/Ereignis95/Ergebnisse/" );

    final File zmlDir = new File( resultDir, "Zeitreihen" );
    final URL zmlURL = new URL( resultURL, "Zeitreihen/" );

    final String[] zmls = zmlDir.list();
    for( final String name : zmls )
    {
      final File zmlFile = new File( zmlDir, name );
      final URL resourceURL = new URL( zmlURL, name );
      assertFileUrlEquals( zmlFile, resourceURL );
    }

    // // auch die Optimierung
    // final File gmlFile = new File( resultDir, "calcCase.gml" );
    // final URL gmlURL = new URL( resultURL, "calcCase.gml" );
    // assertFileUrlEquals( gmlFile, gmlURL );
  }

  private void assertFileUrlEquals( final File file, final URL resource ) throws Exception
  {
    System.out.print( "Checking file: " + file.getName() );

    final ILogger logger = new ILogger()
    {
      public void log( final Level level, final int code, final String message )
      {
        System.out.println( message );
      }
    };

    final boolean failed = DiffUtils.diffUrls( logger, resource, file.toURL() );
    if( failed )
    {
      System.out.println( "\t\tNot ok" );
      fail( "Files should be equal" );
    }
    else
      System.out.println( "\t\tOK" );

    System.out.println();

    // BufferedReader fileReader = null;
    // BufferedReader urlReader = null;
    //
    // try
    // {
    // fileReader = new BufferedReader( new FileReader( file ) );
    // urlReader = new BufferedReader( new InputStreamReader( resource.openStream() ) );
    //
    // final String fileContent = IOUtils.toString( fileReader );
    // final String urlContent = IOUtils.toString( urlReader );
    //
    // assertEquals( "File is not equal to test date: " + file.getName(), urlContent, fileContent );
    //
    // System.out.println( "\t\tOK" );
    // }
    // finally
    // {
    // IOUtils.closeQuietly( fileReader );
    // IOUtils.closeQuietly( urlReader );
    // }
  }
}
