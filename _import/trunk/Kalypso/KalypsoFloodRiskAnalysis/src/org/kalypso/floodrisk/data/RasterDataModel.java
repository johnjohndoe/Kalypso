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

package org.kalypso.floodrisk.data;

import java.io.File;
import java.io.FileWriter;
import java.net.URL;

import javax.xml.namespace.QName;

import org.kalypso.floodrisk.schema.UrlCatalogFloodRisk;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.GMLSchemaCatalog;
import org.kalypso.gmlschema.KalypsoGMLSchemaPlugin;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.cv.RectifiedGridCoverage;
import org.kalypsodeegree_impl.model.cv.RectifiedGridCoverageFactory;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.GMLWorkspace_Impl;

/**
 * RasterDataModel
 * <p>
 * Methods for reading and writing rasterdata (Schema: RasterDataModel.xsd) created by
 * 
 * @author Nadja Peiler (14.06.2005)
 */
public class RasterDataModel
{

  public RasterDataModel( )
  {
    super();
  }

  /**
   * creates a rectifiedGridCoverage object of a gml object
   * 
   * @param gmlURL
   *          (Schema: RasterDataModel.xsd)
   * @return RectifiedGridCoverage
   * @throws Exception
   */
  public RectifiedGridCoverage getRectifiedGridCoverage( URL gmlURL ) throws Exception
  {

    GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( gmlURL, null );
    Feature rootFeature = workspace.getRootFeature();

    return RectifiedGridCoverageFactory.createRectifiedGridCoverage( rootFeature );
  }

  /**
   * creates a rectifiedGridCoverage object of a gml workspace
   * 
   * @param workspace
   * @return RectifiedGridCoverage
   * @throws Exception
   */
  public static RectifiedGridCoverage getRectifiedGridCoverage( GMLWorkspace workspace ) throws Exception
  {

    Feature rootFeature = workspace.getRootFeature();

    return RectifiedGridCoverageFactory.createRectifiedGridCoverage( rootFeature );
  }

  /**
   * creates gml workspace of a rectifiedGridCoverage object and serializes it to a file
   * 
   * @param rasterDataModelGML
   * @param grid
   * @throws Exception
   */
  public void toFile( File rasterDataModelGML, RectifiedGridCoverage grid ) throws Exception
  {
    // load schema
    final GMLSchemaCatalog schemaCatalog = KalypsoGMLSchemaPlugin.getDefault().getSchemaCatalog();
    final GMLSchema schema = schemaCatalog.getSchema( UrlCatalogFloodRisk.NS_RASTERDATAMODEL, (String)null );
    //final GMLSchema schema = schemaCatalog.getSchema( UrlCatalogFloodRisk.NS_RASTERDATAMODEL, "3.1" );

    // create feature and workspace gml
    final IFeatureType[] types = schema.getAllFeatureTypes();

    QName rootFeatureTypeName = new QName(UrlCatalogFloodRisk.NS_RASTERDATAMODEL, "RasterDataModel");
    //QName rgcFeatureTypePropertyName = new QName(UrlCatalogFloodRisk.NS_RECTIFIEDGRIDCOVERAGE, "RectifiedGridCoverageMember");

    IFeatureType rootFeatureType = schema.getFeatureType( rootFeatureTypeName );
    Feature rootFeature = FeatureFactory.createFeature( null, "RasterDataModel0", rootFeatureType, true );

    QName rgcFeatureTypePropertyName = new QName(UrlCatalogFloodRisk.NS_RASTERDATAMODEL, "RectifiedGridCoverageMember");
    IPropertyType ftp_rgc = rootFeatureType.getProperty( rgcFeatureTypePropertyName );
    
    // TODO Dejan: Ubaciti gridDomain i rangeSet u rectifiedGridCoverageFeature
    
    // create feature: RectifiedGridCoverage
    
    //Object[] properties = new Object[] { null, null, null, null, null, new RectifiedGridDomainAsFeature(rootFeature, rootFeatureType, null, null, grid.getGridDomain()), grid.getRangeSet() };
    
    //Feature gridFeature = FeatureFactory.createFeature( null, "RectifiedGridDomain0", schema.getFeatureType( new QName(UrlCatalogFloodRisk.NS_RASTERDATAMODEL, "rectifiedGridDomain") ), true );
    
    //Object[] properties = new Object[] { null, null, null, null, null, grid.getGridDomain(), grid.getRangeSet() };
    //Object[] properties = new Object[] { grid.getGridDomain(), grid.getRangeSet() };
    final Feature rectifiedGridCoverageFeature = FeatureFactory.createFeature( rootFeature, "RectifiedGridCoverage0", rootFeatureType, true );
    
    //rectifiedGridCoverageFeature.setProperty( new QName(UrlCatalogFloodRisk.NS_RASTERDATAMODEL, "rectifiedGridDomain"), grid.getGridDomain() );
    //rectifiedGridCoverageFeature.setProperty( new QName(UrlCatalogFloodRisk.NS_RASTERDATAMODEL, "rangeSet"), grid.getRangeSet() );
    
    //IPropertyType aaa = schema.getFeatureType( rootFeatureTypeName ).getProperty( new QName(UrlCatalogFloodRisk.NS_RASTERDATAMODEL, "rectifiedGridDomain") );
    //FeatureHelper.addProperty(rectifiedGridCoverageFeature, aaa, grid.getGridDomain());
    //FeatureHelper.setProperties( result, props )
    //final Feature rectifiedGridCoverageFeature = FeatureFactory.createFeature( rootFeature, "RectifiedGridCoverage0", ((IFeatureType) ftp_rgc).getTargetFeatureType(), properties );
    //FeatureHelper.addProperty(rootFeature, rootFeatureType., rectifiedGridCoverageFeature );
    
    // check if ftp_rgc == null
    FeatureHelper.addProperty(rootFeature, ftp_rgc, rectifiedGridCoverageFeature );
    
    // create workspace
    GMLWorkspace workspace = new GMLWorkspace_Impl( schema, types, rootFeature, rasterDataModelGML.toURL(), "", null );

    // serialize Workspace
    FileWriter fw = new FileWriter( rasterDataModelGML );
    //final ITypeRegistry<IMarshallingTypeHandler> registry = MarshallingTypeRegistrySingleton.getTypeRegistry();
    //registry.toString();
    GmlSerializer.serializeWorkspace( fw, workspace );
    fw.close();
  }

}