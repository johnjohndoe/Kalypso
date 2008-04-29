/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.dwd;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.io.IOUtils;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureProperty;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_MultiPoint;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.gml.schema.GMLSchema;
import org.kalypsodeegree_impl.gml.schema.GMLSchemaCatalog;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.visitors.FindSomeNearestVisitor;
import org.kalypsodeegree_impl.model.feature.visitors.ResortVisitor;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * TODO: this does not work correctly, as we are using deegree 1 here, which has a problem projecting 
 * between crs with different datums (Datumverschiebung!).
 * <br>
 * DWDRasterGeoLayer
 * <br>
 * 
 * @author doemming (15.06.2005)
 */
public class DWDRasterGeoLayer
{
  private final DWDRaster m_xRaster;

  private final DWDRaster m_yRaster;

  private FeatureType m_positionFeature;

  private static final String GEO_PROP_POINT = "center";

  private final String GEO_PROP_SURFACE = "surface";

  private static final String POS_PROP = "pos";

  final CS_CoordinateSystem m_srcCS;

  GMLWorkspace m_workspace;

  private static final String PROP_CELLMEMBER = "cellMember";

  /**
   * decorates the DWDRaster elements that have Geographic information
   * 
   * @throws Exception
   */
  public DWDRasterGeoLayer( String epsgTarget, DWDRaster xRaster, DWDRaster yRaster ) throws Exception
  {
    final ConvenienceCSFactoryFull csFac = new ConvenienceCSFactoryFull();
    m_srcCS = org.kalypsodeegree_impl.model.cs.Adapters.getDefault().export( csFac.getCSByName( "EPSG:4326" ) );
    final CS_CoordinateSystem targetCS = org.kalypsodeegree_impl.model.cs.Adapters.getDefault().export(
        csFac.getCSByName( epsgTarget ) );
    m_xRaster = xRaster;
    m_yRaster = yRaster;
    init( targetCS );
  }

  public void saveAsGML( File file ) throws IOException, GmlSerializeException
  {
    FileWriter writer = null;
    try
    {
      writer = new FileWriter( file );
      GmlSerializer.serializeWorkspace( writer, m_workspace );
      System.out.println( "wrote DWDRaster to File:" + file.toString() );
    }
    catch( IOException e )
    {
      throw ( e );
    }
    catch( GmlSerializeException e )
    {
      throw ( e );
    }
    finally
    {
      IOUtils.closeQuietly( writer );
    }
  }

  private void init( final CS_CoordinateSystem targetCS ) throws Exception
  {
    final GMLSchema schema = GMLSchemaCatalog.getSchema( "org.kalypso.dwd.geolayer" );
    m_positionFeature = schema.getFeatureType( "DWDCell" );
    final FeatureType layerFT = schema.getFeatureType( "DWDLayer" );
    final Feature rootFE = FeatureFactory.createFeature( "main", layerFT, false );
    m_workspace = FeatureFactory.createGMLWorkspace( schema, rootFE, null );

    // create all feature with point geom property
    for( int pos = 0, size = m_xRaster.size(); pos < size; pos++ )
    {
      final Feature feature = createFeature( pos );
      m_workspace.addFeatureAsComposition( rootFE, PROP_CELLMEMBER, 0, feature );
    }
    
    m_workspace.accept( new TransformVisitor( targetCS ), rootFE, FeatureVisitor.DEPTH_INFINITE );
    
    // resort it before using the query
    m_workspace.accept( new ResortVisitor(), rootFE, FeatureVisitor.DEPTH_INFINITE );
    
    // now update the surface geometry
    m_workspace
        .accept( new UpdateFeatureSurfaceGeometry(), m_workspace.getRootFeature(), FeatureVisitor.DEPTH_INFINITE );
    
    // resort again before using the query
    m_workspace.accept( new ResortVisitor(), rootFE, FeatureVisitor.DEPTH_INFINITE );
  }

  private Feature createFeature( final int pos ) throws Exception
  {
    final Feature feature = FeatureFactory.createFeature( Integer.toString( pos ), m_positionFeature, false );
    GM_Object point = createGeometryPoint( pos );
    feature.setProperty( FeatureFactory.createFeatureProperty( GEO_PROP_POINT, point ) );
    feature.setProperty( FeatureFactory.createFeatureProperty( POS_PROP, new Integer( pos ) ) );
    return feature;
  }

  private GM_Object createGeometryPoint( int pos ) throws Exception
  {
    double mx = m_xRaster.getValueAt( pos );
    double my = m_yRaster.getValueAt( pos );
    return GeometryFactory.createGM_Point( mx, my, m_srcCS );
  }

  class UpdateFeatureSurfaceGeometry implements FeatureVisitor
  {
    public boolean visit( Feature f )
    {
      if( !f.getFeatureType().getName().equals( "DWDCell" ) )
        return true;
      if( ( (Integer)f.getProperty( POS_PROP ) ).intValue() == 2421 )
        System.out.println( "debug" );
      GM_Point point = (GM_Point)f.getProperty( GEO_PROP_POINT );
      GM_Position centerPos = point.getPosition();
      //      GM_Envelope cellEnv = GeometryFactory.createGM_Envelope( centerPos, centerPos );
      FeatureList listFE = (FeatureList)m_workspace.getRootFeature().getProperty( PROP_CELLMEMBER );
      final int numPoints = 9;
      final FindSomeNearestVisitor visitor = new FindSomeNearestVisitor( centerPos, Double.MAX_VALUE, numPoints,
          GEO_PROP_POINT );
      listFE.accept( visitor );
      //      m_workspace.accept( visitor, m_workspace.getRootFeature(),
      // FeatureVisitor.DEPTH_INFINITE );
      // get the next 5 points and make a boundingbox out of this
      final Feature[] result = visitor.getResult();
      List points = new ArrayList();
      for( int i = 0; i < result.length; i++ )
      {
        final GM_Point p = (GM_Point)result[i].getProperty( GEO_PROP_POINT );
        final GM_Position p2 = p.getPosition();
        final GM_Position newPos = GeometryUtilities.createGM_PositionAverage( new GM_Position[]
        {
            centerPos,
            p2 } );

        points.add( GeometryFactory.createGM_Point( newPos, p.getCoordinateSystem() ) );
      }

      GM_MultiPoint multiPoint = GeometryFactory.createGM_MultiPoint( (GM_Point[])points.toArray( new GM_Point[points
          .size()] ), point.getCoordinateSystem() );
      GM_Object convexHull;
      try
      {
        convexHull = multiPoint.getConvexHull();
        FeatureProperty property = FeatureFactory.createFeatureProperty( GEO_PROP_SURFACE, convexHull );
        f.setProperty( property );
      }
      catch( GM_Exception e )
      {
        e.printStackTrace();
      }
      return true;
    }
  }

  public RasterPart[] getPositions( final GM_Surface geometry )
  {
    final List result = new ArrayList();
    final FeatureList fList = (FeatureList)m_workspace.getRootFeature().getProperty( "cellMember" );
    final List list = fList.query( geometry.getEnvelope(), null );
    for( final Iterator iter = list.iterator(); iter.hasNext(); )
    {
      final Feature rasterFE = (Feature)iter.next();
      final GM_Surface rasterGeom = (GM_Surface)rasterFE.getProperty( GEO_PROP_SURFACE );
      if( geometry.intersects( rasterGeom ) )
      {
        final GM_Object intersection = geometry.intersection( rasterGeom );
        final double partArea = GeometryUtilities.calcArea( intersection );
        final Integer pos = (Integer)rasterFE.getProperty( POS_PROP );
        result.add( new RasterPart( pos.intValue(), partArea ) );
      }
    }
    return (RasterPart[])result.toArray( new RasterPart[result.size()] );
  }

  public int getNumberOfCells()
  {
    return m_xRaster.size();
  }
}
