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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;

import junit.framework.TestCase;

import org.apache.commons.io.IOUtils;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.java.net.IUrlCatalog;
import org.kalypso.contribs.java.net.MultiUrlCatalog;
import org.kalypso.ogc.gml.typehandler.GM_ObjectTypeHandler;
import org.kalypso.ogc.sensor.deegree.ObservationLinkHandler;
import org.kalypso.services.calculation.job.ICalcMonitor;
import org.kalypso.services.calculation.job.impl.ResourceCalcDataProvider;
import org.kalypso.services.calculation.job.impl.SimpleCalcResultEater;
import org.kalypso.services.calculation.service.CalcJobInfoBean;
import org.kalypsodeegree_impl.extension.ITypeRegistry;
import org.kalypsodeegree_impl.extension.MarshallingTypeRegistrySingleton;
import org.kalypsodeegree_impl.gml.schema.GMLSchemaCatalog;
import org.kalypsodeegree_impl.gml.schema.schemata.DeegreeUrlCatalog;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * @author belger
 */
public class SpreeCalcJobTest extends TestCase
{
  public void setUp() throws Exception
  {
    super.setUp();

    final ITypeRegistry registry = MarshallingTypeRegistrySingleton.getTypeRegistry();
    registry.registerTypeHandler( new ObservationLinkHandler() );
    registry.registerTypeHandler( new GM_ObjectTypeHandler( "PointPropertyType", GeometryUtilities.getPointClass() ) );
    registry.registerTypeHandler( new GM_ObjectTypeHandler( "MultiPointPropertyType", GeometryUtilities
        .getMultiPointClass() ) );

    registry.registerTypeHandler( new GM_ObjectTypeHandler( "LineStringPropertyType", GeometryUtilities
        .getLineStringClass() ) );
    registry.registerTypeHandler( new GM_ObjectTypeHandler( "MultiLineStringPropertyType", GeometryUtilities
        .getMultiLineStringClass() ) );

    registry
        .registerTypeHandler( new GM_ObjectTypeHandler( "PolygonPropertyType", GeometryUtilities.getPolygonClass() ) );
    registry.registerTypeHandler( new GM_ObjectTypeHandler( "MultiPolygonPropertyType", GeometryUtilities
        .getMultiPolygonClass() ) );
  }

  public void testCalcJob() throws IOException
  {
    final File tmpDir = FileUtilities.createNewTempDir( "SpreeCalcJobTest" );

    final File schemaCatalogDir = new File( tmpDir, "schemaCache" );
    schemaCatalogDir.mkdir();

    final MultiUrlCatalog urlCatalog = new MultiUrlCatalog( new IUrlCatalog[]
    {
        new SpreeUrlCatalog(),
        new DeegreeUrlCatalog() } );

    GMLSchemaCatalog.init( urlCatalog, schemaCatalogDir );

    final SpreeCalcJob job = new SpreeCalcJob();
    final ICalcMonitor monitor = new CalcJobInfoBean();

    final String resourceBase = "/" + getClass().getPackage().getName().replace( '.', '/' )
        + "/resources/test/TestvarianteHoltendorf/";

    final ResourceCalcDataProvider inputProvider = new ResourceCalcDataProvider( resourceBase );
    inputProvider.addResource( "CONTROL_GML", ".calculation" );
    inputProvider.addResource( "GML", "calcCase.gml" );
    inputProvider.addResource( "ZML", "Zeitreihen" );

    final SimpleCalcResultEater resultEater = new SimpleCalcResultEater();

    final File jobTmpDir = new File( tmpDir, "jobTmp" );
    job.run( jobTmpDir, inputProvider, resultEater, monitor );

    compareResults( resultEater );
    FileUtilities.deleteRecursive( tmpDir );
  }

  private void compareResults( final SimpleCalcResultEater resultEater ) throws IOException
  {
    // nur die Zeitreihen vergleichen
    final File resultDir = resultEater.getResult( "ERGEBNISSE" );
    final URL resultURL = getClass().getResource( "resources/test/TestvarianteHoltendorf/Ergebnisse/" );

    final File zmlDir = new File( resultDir, "Zeitreihen" );
    final URL zmlURL = new URL( resultURL, "Zeitreihen/" );

    final String[] zmls = zmlDir.list();
    for( int i = 0; i < zmls.length; i++ )
    {
      final String name = zmls[i];
      final File zmlFile = new File( zmlDir, name );
      final URL resourceURL = new URL( zmlURL, name );
      assertFileUrlEquals( zmlFile, resourceURL );
    }

    // auch die Optimierung
    final File gmlFile = new File( resultDir, "calcCase.gml" );
    final URL gmlURL = new URL( resultURL, "calcCase.gml" );
    assertFileUrlEquals( gmlFile, gmlURL );
  }

  private void assertFileUrlEquals( final File file, final URL resource ) throws FileNotFoundException, IOException
  {
    System.out.print( "Checking file: " + file.getName() );
    
    BufferedReader fileReader = null;
    BufferedReader urlReader = null;

    try
    {
      fileReader = new BufferedReader( new FileReader( file ) );
      urlReader = new BufferedReader( new InputStreamReader( resource.openStream() ) );

      final String fileContent = IOUtils.toString( fileReader );
      final String urlContent = IOUtils.toString( urlReader );

      assertEquals( "File is not equal to test date: " + file.getName(), urlContent, fileContent );
      
      
      System.out.println( "\t\tOK" );
    }
    finally
    {
      IOUtils.closeQuietly( fileReader );
      IOUtils.closeQuietly( urlReader );
    }
  }
}
