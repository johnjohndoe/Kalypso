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
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.io.IOUtils;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.GMLSchemaCatalog;
import org.kalypso.gmlschema.KalypsoGMLSchemaPlugin;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_MultiPoint;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.visitors.FindSomeNearestVisitor;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * DWDRasterGeoLayer
 * <p>
 * created by
 * 
 * @author doemming (15.06.2005)
 */
public class DWDRasterGeoLayer
{
  private final DWDRaster m_xRaster;

  private final DWDRaster m_yRaster;

  private IFeatureType m_positionFeature;

  private static final String GEO_PROP_POINT = "center";

  private final String GEO_PROP_SURFACE = "surface";

  private static final String POS_PROP = "pos";

  final String m_srcCS;

  GMLWorkspace m_workspace;

  private static final String PROP_CELLMEMBER = "cellMember";

  /**
   * decorates the DWDRaster elements that have Geographic information
   * 
   * @throws Exception
   */
  public DWDRasterGeoLayer( String epsgTarget, DWDRaster xRaster, DWDRaster yRaster ) throws Exception
  {
    m_srcCS = "EPSG:4326";
    String targetCS = epsgTarget;
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
      throw (e);
    }
    catch( GmlSerializeException e )
    {
      throw (e);
    }
    finally
    {
      IOUtils.closeQuietly( writer );
    }
  }

  private void init( String targetCS ) throws Exception
  {
    final GMLSchemaCatalog schemaCatalog = KalypsoGMLSchemaPlugin.getDefault().getSchemaCatalog();
    final GMLSchema schema = schemaCatalog.getSchema( "org.kalypso.dwd.geolayer", (String) null );
    m_positionFeature = schema.getFeatureType( "DWDCell" );
    final IFeatureType layerFT = schema.getFeatureType( "DWDLayer" );
    final Feature rootFE = FeatureFactory.createFeature( null, null, "main", layerFT, true );
    m_workspace = FeatureFactory.createGMLWorkspace( schema, rootFE, null, null, null, null );

    IRelationType cellMemeberPT = (IRelationType) layerFT.getProperty( PROP_CELLMEMBER );
    // create all feature with point geom property
    for( int pos = 0, size = m_xRaster.size(); pos < size; pos++ )
    {
      Feature feature = createFeature( rootFE, cellMemeberPT, pos );
      m_workspace.addFeatureAsComposition( rootFE, cellMemeberPT, 0, feature );
    }
// m_workspace.accept( new TransformVisitor( targetCS ), rootFE, FeatureVisitor.DEPTH_INFINITE );
// // resort it before using the query
// m_workspace.accept( new ResortVisitor(), rootFE, FeatureVisitor.DEPTH_INFINITE );
// // now update the surface geometry
// m_workspace.accept( new UpdateFeatureSurfaceGeometry(), m_workspace.getRootFeature(), FeatureVisitor.DEPTH_INFINITE
    // );
// // resort again before using the query
// m_workspace.accept( new ResortVisitor(), rootFE, FeatureVisitor.DEPTH_INFINITE );
  }

  private Feature createFeature( final Feature parent, final IRelationType parentRelation, final int pos ) throws Exception
  {
    final Feature feature = FeatureFactory.createFeature( parent, parentRelation, Integer.toString( pos ), m_positionFeature, true );
    GM_Object point = createGeometryPoint( pos );
    feature.setProperty( GEO_PROP_POINT, point );
    feature.setProperty( POS_PROP, new BigInteger( String.valueOf( pos ) ) );
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
      GM_Point point = (GM_Point) f.getProperty( GEO_PROP_POINT );
      GM_Position centerPos = point.getPosition();
      // GM_Envelope cellEnv = GeometryFactory.createGM_Envelope( centerPos, centerPos );
      FeatureList listFE = (FeatureList) m_workspace.getRootFeature().getProperty( PROP_CELLMEMBER );
      final int numPoints = 9;
      final FindSomeNearestVisitor visitor = new FindSomeNearestVisitor( centerPos, Double.MAX_VALUE, numPoints, GEO_PROP_POINT );
      listFE.accept( visitor );
      // m_workspace.accept( visitor, m_workspace.getRootFeature(),
      // FeatureVisitor.DEPTH_INFINITE );
      // get the next 5 points and make a boundingbox out of this
      final Feature[] result = visitor.getResult();
      final List<GM_Point> points = new ArrayList<GM_Point>();
      for( int i = 0; i < result.length; i++ )
      {
        final GM_Point p = (GM_Point) result[i].getProperty( GEO_PROP_POINT );
        final GM_Position p2 = p.getPosition();
        final GM_Position newPos = GeometryUtilities.createGM_PositionAverage( new GM_Position[] { centerPos, p2 } );

        points.add( GeometryFactory.createGM_Point( newPos, p.getCoordinateSystem() ) );
      }

      GM_MultiPoint multiPoint = GeometryFactory.createGM_MultiPoint( points.toArray( new GM_Point[points.size()] ), point.getCoordinateSystem() );
      GM_Object convexHull;
      try
      {
        convexHull = multiPoint.getConvexHull();
        f.setProperty( GEO_PROP_SURFACE, convexHull );
      }
      catch( GM_Exception e )
      {
        e.printStackTrace();
      }
      return true;
    }
  }

  public RasterPart[] getPositions( GM_Surface geometry )
  {
    final List<RasterPart> result = new ArrayList<RasterPart>();
    final FeatureList fList = (FeatureList) m_workspace.getRootFeature().getProperty( "cellMember" );
    final List list = fList.query( geometry.getEnvelope(), null );
    for( Iterator iter = list.iterator(); iter.hasNext(); )
    {
      final Feature rasterFE = (Feature) iter.next();
      final GM_Surface rasterGeom = (GM_Surface) rasterFE.getProperty( GEO_PROP_SURFACE );
      if( geometry.intersects( rasterGeom ) )
      {
        final GM_Object intersection = geometry.intersection( rasterGeom );
        // double fullArea = GeometryUtilities.calcArea( rasterGeom );
        double partArea = GeometryUtilities.calcArea( intersection );
        // double portion = partArea / fullArea;
        final Integer pos = (Integer) rasterFE.getProperty( POS_PROP );
        result.add( new RasterPart( pos.intValue(), partArea ) );
      }
    }
    return result.toArray( new RasterPart[result.size()] );
  }

  public int getNumberOfCells( )
  {
    return m_xRaster.size();
  }

  /**
   * This function returns the x raster.
   * 
   * @return The x raster.
   */
  public DWDRaster getXRaster( )
  {
    return m_xRaster;
  }

  /**
   * This function returns the y raster.
   * 
   * @return The y raster.
   */
  public DWDRaster getYRaster( )
  {
    return m_yRaster;
  }
}