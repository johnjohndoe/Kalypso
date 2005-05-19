package org.kalypso.floodrisk.rasterize.test;

import junit.framework.TestCase;

import org.kalypso.floodrisk.process.impl.ProcessDataProvider;
import org.kalypso.floodrisk.process.impl.ProcessResultEater;
import org.kalypso.floodrisk.rasterize.RasterizeLanduseJob;
import org.kalypso.floodrisk.schema.UrlCatalogFloodRisk;
import org.kalypso.java.io.FileUtilities;
import org.kalypso.java.net.IUrlCatalog;
import org.kalypso.java.net.MultiUrlCatalog;
import org.kalypso.services.calculation.common.ICalcServiceConstants;
import org.kalypso.services.calculation.service.CalcJobClientBean;
import org.kalypso.services.calculation.service.CalcJobInfoBean;
import org.kalypso.services.calculation.service.CalcJobServiceException;
import org.kalypsodeegree_impl.extension.ITypeRegistry;
import org.kalypsodeegree_impl.extension.TypeRegistryException;
import org.kalypsodeegree_impl.extension.TypeRegistrySingleton;
import org.kalypsodeegree_impl.gml.schema.GMLSchemaCatalog;
import org.kalypsodeegree_impl.gml.schema.schemata.DeegreeUrlCatalog;
import org.kalypsodeegree_impl.model.cv.RangeSetTypeHandler;
import org.kalypsodeegree_impl.model.cv.RectifiedGridDomainTypeHandler;

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

public class RasterizeLanduseJobTest extends TestCase
{

  public void testRun()
  {
    // initialize schemaCatalog
    final IUrlCatalog catalog = new MultiUrlCatalog( new IUrlCatalog[]
    {
        new DeegreeUrlCatalog(),
        new UrlCatalogFloodRisk() } );
    GMLSchemaCatalog.init( catalog, FileUtilities.createNewTempDir( "schemaCache" ) );

    //register typeHandler
    final ITypeRegistry registry = TypeRegistrySingleton.getTypeRegistry();
    try
    {
      registry.registerTypeHandler( new RangeSetTypeHandler() );
      registry.registerTypeHandler( new RectifiedGridDomainTypeHandler() );
    }
    catch( TypeRegistryException e )
    {
      e.printStackTrace();
    }

    RasterizeLanduseJob job = new RasterizeLanduseJob();
    testRasterizeLanduse( job );
  }

  private void testRasterizeLanduse( RasterizeLanduseJob job )
  {
    String base = "D://Nadja//eclipse//runtime-workspace//Test//";
    //Input
    int numInputBeans = 5;
    CalcJobClientBean[] input = new CalcJobClientBean[numInputBeans];
    CalcJobClientBean input1 = new CalcJobClientBean( RasterizeLanduseJob.LanduseVectorDataID, base
        + "Landuse//landuse.shp" );
    input[0] = input1;
    CalcJobClientBean input2 = new CalcJobClientBean(
        RasterizeLanduseJob.VectorDataCoordinateSystemID, "EPSG:31467" );
    input[1] = input2;
    CalcJobClientBean input3 = new CalcJobClientBean( RasterizeLanduseJob.ContextModelID, base
        + "Control//contextModell.gml" );
    input[2] = input3;
    CalcJobClientBean input4 = new CalcJobClientBean( RasterizeLanduseJob.LandusePropertyNameID,
        "NUTZUNG" );
    input[3] = input4;
    CalcJobClientBean input5 = new CalcJobClientBean( RasterizeLanduseJob.BaseRasterID, base
        + "Waterlevel//wsp_hq100.gml" );
    input[4] = input5;
    ProcessDataProvider inputProvider = new ProcessDataProvider( input );
    //Output
    int numOutputBeans = 1;
    CalcJobClientBean[] output = new CalcJobClientBean[numOutputBeans];
    CalcJobClientBean output1 = new CalcJobClientBean( RasterizeLanduseJob.LanduseRasterDataID,
        base + "Landuse//landuseData.gml" );
    output[0] = output1;
    ProcessResultEater resultEater = new ProcessResultEater( output );

    CalcJobInfoBean jobBean = new CalcJobInfoBean( "", "", "RasterizeLanduseJob",
        ICalcServiceConstants.RUNNING, -1, "" );

    try
    {
      job.run( null, inputProvider, resultEater, jobBean );
    }
    catch( CalcJobServiceException e )
    {
      e.printStackTrace();
    }
  }

  public void testGetSpezifikation()
  {
    System.out.println( "testGetSpezifikation" );
  }

}