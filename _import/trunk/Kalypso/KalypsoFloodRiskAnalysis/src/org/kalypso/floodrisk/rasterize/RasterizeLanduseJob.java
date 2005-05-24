package org.kalypso.floodrisk.rasterize;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Hashtable;
import java.util.List;

import org.kalypso.floodrisk.data.ContextModel;
import org.kalypso.floodrisk.data.RasterDataModel;
import org.kalypso.floodrisk.process.IProcessDataProvider;
import org.kalypso.floodrisk.process.IProcessResultEater;
import org.kalypso.java.io.FileUtilities;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.services.calculation.job.ICalcDataProvider;
import org.kalypso.services.calculation.job.ICalcJob;
import org.kalypso.services.calculation.job.ICalcMonitor;
import org.kalypso.services.calculation.job.ICalcResultEater;
import org.kalypso.services.calculation.service.CalcJobClientBean;
import org.kalypso.services.calculation.service.CalcJobServiceException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.kalypsodeegree_impl.model.cv.RectifiedGridCoverage;
import org.opengis.cs.CS_CoordinateSystem;

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

public class RasterizeLanduseJob implements ICalcJob
{

  //IDs
  //input
  public static final String LanduseVectorDataID = "LanduseVectorData";

  public static final String VectorDataCoordinateSystemID = "VectorDataCoordinateSystem";

  public static final String ContextModelID = "ContextModel";

  public static final String LandusePropertyNameID = "LandusePropertyName";

  public static final String BaseRasterID = "BaseRaster";

  //output
  public static final String LanduseRasterDataID = "LanduseRasterData";

  public RasterizeLanduseJob()
  {
    super();

  }

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#run(java.io.File,
   *      org.kalypso.services.calculation.job.ICalcDataProvider,
   *      org.kalypso.services.calculation.job.ICalcResultEater,
   *      org.kalypso.services.calculation.job.ICalcMonitor)
   */
  public void run( File tmpdir, ICalcDataProvider inputProvider, ICalcResultEater resultEater,
      ICalcMonitor monitor ) throws CalcJobServiceException
  {
    monitor.setMessage( "Generate input" );
    String csName = (String)( (IProcessDataProvider)inputProvider )
        .getObjectForID( VectorDataCoordinateSystemID );
    CS_CoordinateSystem cs = ConvenienceCSFactory.getInstance().getOGCCSByName( csName );
    String shapeBaseFile = (String)( (IProcessDataProvider)inputProvider )
        .getObjectForID( LanduseVectorDataID );
    List featureList = getFeatureList( FileUtilities.nameWithoutExtension( shapeBaseFile ), cs );
    String propertyName = (String)( (IProcessDataProvider)inputProvider )
        .getObjectForID( LandusePropertyNameID );
    File contextModelGML = new File( (String)( (IProcessDataProvider)inputProvider )
        .getObjectForID( ContextModelID ) );
    Hashtable landuseTypeList;
    try
    {
      ContextModel contextModel = new ContextModel( contextModelGML.toURL() );
      landuseTypeList = contextModel.getLanduseList();
    }
    catch( MalformedURLException e )
    {
      throw new CalcJobServiceException( e.getMessage(), e );
    }
    try
    {
      File baseRasterGML = new File( (String)( (IProcessDataProvider)inputProvider )
          .getObjectForID( BaseRasterID ) );
      RasterDataModel rasterDataModel = new RasterDataModel();
      RectifiedGridCoverage baseRaster = rasterDataModel.getRectifiedGridCoverage( baseRasterGML
          .toURL() );
      monitor.setMessage( "Start calculation" );
      RectifiedGridCoverage resultGrid = VectorToGridConverter.toGrid( featureList, propertyName,
          landuseTypeList, baseRaster, monitor );

      CalcJobClientBean outputBean = (CalcJobClientBean)( (IProcessResultEater)resultEater )
          .getOutputMap().get( LanduseRasterDataID );
      File resultFile = new File( outputBean.getPath() );
      if( !resultFile.exists() )
        resultFile.createNewFile();
      monitor.setMessage( "Generate output" );
      rasterDataModel.toFile( resultFile, resultGrid );
      resultEater.addResult( outputBean.getId(), null );
    }
    catch( Exception e1 )
    {
      throw new CalcJobServiceException( e1.getMessage(), e1 );
    }
  }

  /**
   * returns a list of Features for a given shapeFile
   * 
   * @param shapeFileBase
   *          (base of shape)
   * 
   * @return List of Features
   */
  private List getFeatureList( String shapeFileBase, CS_CoordinateSystem cs )
  {
    try
    {
      GMLWorkspace workspace = ShapeSerializer.deserialize( shapeFileBase, cs, null );
      Feature root = workspace.getRootFeature();
      List featureList = (List)root.getProperty( "featureMember" );
      return featureList;
    }
    catch( Exception e )
    {
      System.out.println( e );
      return null;
    }
  }

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#getSpezifikation()
   */
  public URL getSpezifikation()
  {
    return null;
  }

}