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
package org.kalypso.ogc.gml.map.widgets;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Polygon;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;

import org.apache.commons.lang.ArrayUtils;
import org.kalypso.commons.command.ICommand;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.ogc.gml.ScrabLayerFeatureTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ui.editor.gmleditor.util.command.AddFeatureCommand;
import org.kalypso.ui.editor.mapeditor.actiondelegates.CreateLineStringWidgetDelegate;
import org.kalypso.ui.editor.mapeditor.actiondelegates.CreatePointWidgetDelegate;
import org.kalypso.ui.editor.mapeditor.actiondelegates.CreatePolygonWidgetDelegate;
import org.kalypsodeegree.graphics.sld.Symbolizer;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureProperty;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.graphics.sld.StyleFactory;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * 
 * TODO: insert type comment here
 * 
 * @author kuepfer
 */
public class CreateGeometeryWidget2 extends AbstractWidget
{
  private final ArrayList m_points = new ArrayList();

  private Point m_currentPoint = null;

  //  private Polygon m_poly;

  private Map m_propMap = new TreeMap();

  private String m_geomType = null;

  private static final String GEOM_PROPERTY_NAME = "GEOM";

  /**
   *  
   */

  public CreateGeometeryWidget2( String name, String toolTip, String geometryTyp )
  {
    super( name, toolTip );
    m_geomType = geometryTyp;
  }

  /**
   *  
   */
  private void clear()
  {
    m_points.clear();
    m_currentPoint = null;
  }

  /**
   * @return
   * @throws GM_Exception
   */
  private Map getPropertyNameGeomMap() throws GM_Exception
  {
    MapPanel mapPanel = getMapPanel();
    final GeoTransform gt = mapPanel.getProjection();
    final CS_CoordinateSystem cs = mapPanel.getMapModell().getCoordinatesSystem();
    final ArrayList positions = getPosArray();
    Map map = new TreeMap();
    if( m_geomType.equals( CreatePolygonWidgetDelegate.GEOM_TYPE ) )
    {
      map.put( ScrabLayerFeatureTheme.POLYGON_GEOM_PROP_NAME, getPolygon() );
    }
    if( m_geomType.equals( CreateLineStringWidgetDelegate.GEOM_TYPE ) )
    {
      map.put( ScrabLayerFeatureTheme.LINESTRING_GEOM_PROP_NAME, getLineString() );
    }
    if( m_geomType.equals( CreatePointWidgetDelegate.GEOM_TYPE ) )
    {
      GM_Position pos = (GM_Position)m_points.get( 0 );
      GM_Point point = GeometryFactory.createGM_Point( pos.getX(), pos.getY(), cs );
      map.put( ScrabLayerFeatureTheme.POINT_GEOM_PROP_NAME, point );
    }
    return map;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftClicked(java.awt.Point)
   */
  public void leftClicked( Point p )
  {
    if( m_geomType.equals( CreatePointWidgetDelegate.GEOM_TYPE ) )
      perform();
    else
      m_points.add( p );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#dragged(java.awt.Point)
   */
  public void moved( Point p )
  {
    if( !m_points.isEmpty() )
      m_currentPoint = p;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  public void paint( Graphics g )
  {
    if( !m_points.isEmpty() && m_currentPoint != null )
    {
      if( m_geomType.equals( CreatePolygonWidgetDelegate.GEOM_TYPE ) )
      {
        //        GM_Surface polygon = null;
        //        try
        //        {
        //          m_points.add( m_currentPoint );
        //          polygon = getPolygon();
        //          m_points.remove( m_currentPoint );
        //
        //          Feature feature = getFeature( polygon );
        //          Symbolizer symbolizer = getUserStyle( polygon );
        //
        //          DisplayElement element = DisplayElementFactory.buildDisplayElement( feature, symbolizer, null );
        //          element.paint( g, getMapPanel().getProjection() );
        //        }
        //        catch( GM_Exception e )
        //        {
        //          e.printStackTrace();
        //        }
        //        catch( IncompatibleGeometryTypeException e )
        //        {
        //          e.printStackTrace();
        //        }
        final Polygon polygon = new Polygon( getXArrayPixel(), getYArrayPixel(), m_points.size() );
        polygon.addPoint( (int)m_currentPoint.getX(), (int)m_currentPoint.getY() );
        g.drawPolygon( polygon );
        drawHandles( g, polygon );

      }
      if( m_geomType.equals( CreateLineStringWidgetDelegate.GEOM_TYPE ) )
      {
        m_points.add( m_currentPoint );
        g.drawPolyline( getXArrayPixel(), getYArrayPixel(), m_points.size() );
        m_points.remove( m_currentPoint );
      }
      if( m_geomType.equals( CreatePointWidgetDelegate.GEOM_TYPE ) )
      {
        for( Iterator iter = m_points.iterator(); iter.hasNext(); )
        {
          Point p = (Point)iter.next();
          g.drawOval( (int)p.getX(), (int)p.getY(), 1, 1 );
        }
      }

    }
  }

  /**
   * @param g
   * @param polygon
   */
  private void drawHandles( Graphics g, Polygon polygon )
  {
    int[] x = polygon.xpoints;
    int[] y = polygon.ypoints;
    int n = polygon.npoints;
    int sizeOuter = 6;
    for( int i = 0; i < y.length; i++ )
    {
      g.drawRect( x[i] - sizeOuter / 2, y[i] - sizeOuter / 2, sizeOuter, sizeOuter );
    }

  }

  /**
   * @return
   */
  private int[] getYArrayPixel()
  {
    ArrayList yArray = new ArrayList();
    for( int i = 0; i < m_points.size(); i++ )
    {
      Point p = (Point)m_points.get( i );
      yArray.add( new Integer( (int)p.getY() ) );

    }
    //    yArray.add( new Integer( (int)m_currentPoint.getY() ) );
    return ArrayUtils.toPrimitive( (Integer[])yArray.toArray( new Integer[m_points.size()] ) );
  }

  /**
   * @return
   */
  private int[] getXArrayPixel()
  {
    ArrayList xArray = new ArrayList();
    for( int i = 0; i < m_points.size(); i++ )
    {
      Point p = (Point)m_points.get( i );
      xArray.add( new Integer( (int)p.getX() ) );

    }
    //    xArray.add( new Integer( (int)m_currentPoint.getX() ) );
    return ArrayUtils.toPrimitive( (Integer[])xArray.toArray( new Integer[m_points.size()] ) );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#perform()
   */
  public void perform()
  {
    MapPanel mapPanel = getMapPanel();
    final GeoTransform gt = mapPanel.getProjection();
    final CS_CoordinateSystem cs = mapPanel.getMapModell().getCoordinatesSystem();
    final ArrayList positions = new ArrayList();
    try
    {
      IFeatureSelectionManager selectionManager = KalypsoCorePlugin.getDefault().getSelectionManager();
      ScrabLayerFeatureTheme scrabLayer = (ScrabLayerFeatureTheme)mapPanel.getMapModell().getScrabLayer();
      CommandableWorkspace workspace = scrabLayer.getWorkspace();
      ICommand command = new AddFeatureCommand( workspace, scrabLayer.getFeatureType(), workspace.getRootFeature(),
          ScrabLayerFeatureTheme.FEATURE_MEMBER, 0, getPropertyNameGeomMap(), KalypsoCorePlugin.getDefault()
              .getSelectionManager() );
      workspace.postCommand( command );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    clear();
  }

  private ArrayList getPosArray()
  {
    ArrayList positions = new ArrayList();
    MapPanel mapPanel = getMapPanel();
    final GeoTransform gt = mapPanel.getProjection();
    final CS_CoordinateSystem cs = mapPanel.getMapModell().getCoordinatesSystem();
    for( int i = 0; i < m_points.size(); i++ )
    {
      final Point p = (Point)m_points.get( i );
      int x = (int)p.getX();
      int y = (int)p.getY();
      GM_Position pos = GeometryFactory.createGM_Position( gt.getSourceX( x ), gt.getSourceY( y ) );
      positions.add( pos );
    }
    return positions;
  }

  private GM_Surface getPolygon() throws GM_Exception
  {
    ArrayList posArray = getPosArray();
    posArray.add( posArray.get( 0 ) );
    return GeometryFactory.createGM_Surface( (GM_Position[])posArray.toArray( new GM_Position[posArray.size()] ), null,
        null, getMapPanel().getMapModell().getCoordinatesSystem() );
  }

  private GM_Curve getLineString() throws GM_Exception
  {
    ArrayList posArray = getPosArray();
    return GeometryFactory.createGM_Curve( (GM_Position[])posArray.toArray( new GM_Position[posArray.size()] ),
        getMapPanel().getMapModell().getCoordinatesSystem() );
  }

  private Symbolizer getUserStyle( GM_Object geom )
  {
    Symbolizer symbolizers = null;
    if( geom instanceof GM_Point )
      symbolizers = StyleFactory.createPointSymbolizer( StyleFactory.createGraphic( null, StyleFactory
          .createMark( "square" ), .7d, 5, 0 ), GEOM_PROPERTY_NAME );
    if( geom instanceof GM_Curve )
      symbolizers = StyleFactory.createLineSymbolizer( StyleFactory.createStroke(), GEOM_PROPERTY_NAME );
    if( geom instanceof GM_Surface )
      symbolizers = StyleFactory.createPolygonSymbolizer( StyleFactory.createStroke(), StyleFactory.createFill(
          Color.GRAY, 0.5d ), GEOM_PROPERTY_NAME );
    //    FeatureTypeStyle featureTypeStyle = StyleFactory.createFeatureTypeStyle( "default", symbolizers );
    //    return (UserStyle)StyleFactory.createStyle( "default", "default", "empty Abstract", featureTypeStyle );
    return symbolizers;
  }

  private Feature getFeature( GM_Object geom )
  {
    FeatureTypeProperty property = FeatureFactory.createFeatureTypeProperty( GEOM_PROPERTY_NAME, geom.getClass()
        .getName(), false );
    FeatureType featureType = FeatureFactory.createFeatureType( "DisplayFeatureType", "www.defaultnamespace.de",
        new FeatureTypeProperty[]
        { property }, new int[]
        { 1 }, new int[]
        { 1 }, null, null );
    FeatureProperty featureProperty = FeatureFactory.createFeatureProperty( GEOM_PROPERTY_NAME, geom );
    return FeatureFactory.createFeature( "geom0", featureType, new FeatureProperty[]
    { featureProperty } );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#doubleClickedLeft(java.awt.Point)
   */
  public void doubleClickedLeft( Point p )
  {
    if( !m_points.isEmpty() )
    {
      if( m_geomType.equals( CreatePolygonWidgetDelegate.GEOM_TYPE ) && m_points.size() >= 3 )
        perform();
      if( m_geomType.equals( CreateLineStringWidgetDelegate.GEOM_TYPE ) && m_points.size() >= 2 )
        perform();
    }

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#doubleClickedRight(java.awt.Point)
   */
  public void doubleClickedRight( Point p )
  {
  // TODO Auto-generated method stub

  }
}
