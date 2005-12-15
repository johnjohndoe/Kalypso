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

import java.awt.Graphics;
import java.awt.Point;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.ArrayUtils;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.editor.gmleditor.util.command.AddFeatureCommand;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.kalypsodeegree_impl.tools.GeometryUtilities;
import org.opengis.cs.CS_CoordinateSystem;

import com.vividsolutions.jts.geom.Geometry;

/**
 *  
 */
public class CreateGeometeryWidget2 extends AbstractWidget
{
  // points in pixel coordinates
  private final List m_points = new ArrayList();

  // this is the point currently under the mouse
  private Point m_currentPoint = null;

  private Class m_apreferedGeometryClass = null;

  private FeatureType m_featureType = null;

  private Feature m_parentFeature = null;

  private FeatureTypeProperty m_linkFTP = null;

  private boolean m_valid = true;

  private CommandableWorkspace m_workspace;

  private FeatureTypeProperty m_geometryProperty = null;

  private CS_CoordinateSystem m_coordinatesSystem = null;

  private GeoTransform m_projection = null;

  private double MIN_DRAG_DISTANCE_PIXEL = 20;

  private GM_Object m_validGeometryValue;

  /**
   *  
   */
  public CreateGeometeryWidget2( String name, String toolTip, Class geometryClass )
  {
    super( name, toolTip );
    m_apreferedGeometryClass = geometryClass;
    update( getMapPanel() );
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
   * @throws GM_Exception
   * @throws NotEnoughPointsExeption
   */
  private GM_Object createGeometry( final List pixelArray ) throws GM_Exception, NotEnoughPointsExeption
  {

    if( m_geometryProperty.getType().equals( GeometryUtilities.getPolygonClass().getName() ) && pixelArray.size() < 3 )
      throw new NotEnoughPointsExeption();
    if( m_geometryProperty.getType().equals( GeometryUtilities.getLineStringClass().getName() )
        && pixelArray.size() < 2 )
      throw new NotEnoughPointsExeption();

    final List posArray = getPositionArray( pixelArray );
    GM_Object result = null;
    if( m_geometryProperty.getType().equals( GeometryUtilities.getPolygonClass().getName() ) )
      result = getPolygon( posArray );
    else if( m_geometryProperty.getType().equals( GeometryUtilities.getLineStringClass().getName() ) )
      result = getLineString( posArray );
    else if( m_geometryProperty.getType().equals( GeometryUtilities.getPointClass().getName() ) )
      result = getPoint( posArray );
    // TODO support the multis ...
    // test it
    return result;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftClicked(java.awt.Point)
   */
  public void leftClicked( Point p )
  {
    if( !isValid() )
      return;
    // first test if vaild...
    final List testList = new ArrayList();
    for( Iterator iter = m_points.iterator(); iter.hasNext(); )
      testList.add( iter.next() );
    testList.add( p );

    try
    {
      final GM_Object gm_geometry;
      gm_geometry = createGeometry( testList );
      final Geometry geometry = JTSAdapter.export( gm_geometry );
      if( geometry.isValid() && geometry.isSimple() )
      {
        m_validGeometryValue = gm_geometry;
        m_points.add( p );
        if( m_geometryProperty.getType().equals( GeometryUtilities.getPointClass().getName() ) )
          perform();
      }
      else
        m_validGeometryValue = null;
    }
    catch( NotEnoughPointsExeption e )
    {
      m_points.add( p );
      m_validGeometryValue = null;
    }
    catch( GM_Exception e )
    {
      e.printStackTrace();
      m_validGeometryValue = null;
    }
    m_currentPoint = p;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#dragged(java.awt.Point)
   */
  public void dragged( Point p )
  {

    if( m_points.isEmpty() || ( (Point)m_points.get( m_points.size() - 1 ) ).distance( p ) > MIN_DRAG_DISTANCE_PIXEL )
      leftClicked( p );
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
    if( !isValid() )
      return;
    if( !m_points.isEmpty() && m_currentPoint != null )
    {
      final int[] arrayX = getXArrayPixel();
      final int[] arrayY = getYArrayPixel();
      if( m_geometryProperty.getType().equals( GeometryUtilities.getPolygonClass().getName() ) )
      {
        // paint polygon
        g.drawPolygon( arrayX, arrayY, arrayX.length );
        drawHandles( g, arrayX, arrayY );
      }
      else if( m_geometryProperty.getType().equals( GeometryUtilities.getLineStringClass().getName() ) )
      {
        // paint linestring
        g.drawPolyline( arrayX, arrayY, arrayX.length );
        drawHandles( g, arrayX, arrayY );
      }
      else if( m_geometryProperty.getType().equals( GeometryUtilities.getPointClass().getName() ) )
      {
        drawHandles( g, arrayX, arrayY );
      }
    }
  }

  /**
   * 
   * @param g
   *          graphics
   * @param x
   *          array x
   * @param y
   *          array y
   */
  private void drawHandles( final Graphics g, final int[] x, final int[] y )
  {
    int sizeOuter = 6;
    for( int i = 0; i < y.length; i++ )
      g.drawRect( x[i] - sizeOuter / 2, y[i] - sizeOuter / 2, sizeOuter, sizeOuter );
  }

  /**
   * 
   * @return all the y poaints as array including the current point
   */
  private int[] getYArrayPixel()
  {
    List yArray = new ArrayList();
    for( int i = 0; i < m_points.size(); i++ )
    {
      Point p = (Point)m_points.get( i );
      yArray.add( new Integer( (int)p.getY() ) );
    }
    if( m_currentPoint != null )
      yArray.add( new Integer( m_currentPoint.y ) );
    return ArrayUtils.toPrimitive( (Integer[])yArray.toArray( new Integer[m_points.size()] ) );
  }

  /**
   * @return all the x points as array including the current point
   */
  private int[] getXArrayPixel()
  {
    final List xArray = new ArrayList();
    for( int i = 0; i < m_points.size(); i++ )
    {
      final Point p = (Point)m_points.get( i );
      xArray.add( new Integer( (int)p.getX() ) );
    }
    if( m_currentPoint != null )
      xArray.add( new Integer( m_currentPoint.x ) );
    return ArrayUtils.toPrimitive( (Integer[])xArray.toArray( new Integer[m_points.size()] ) );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#perform()
   */
  public void perform()
  {
    if( !isValid() )
      return;
    try
    {
      //      final GM_Object geometry = createGeometry( m_points );
      if( m_validGeometryValue == null ) // nothing to perform
        return;
      final Map valueMap = new HashMap();
      valueMap.put( m_geometryProperty, m_validGeometryValue );
      final ICommand command = new AddFeatureCommand( m_workspace, m_featureType, m_parentFeature, m_linkFTP.getName(),
          0, valueMap, KalypsoCorePlugin.getDefault().getSelectionManager() );
      m_workspace.postCommand( command );
    }
    catch( Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    clear();
  }

  private List getPositionArray( final List listPoints )
  {
    final List positions = new ArrayList();
    for( int i = 0; i < listPoints.size(); i++ )
    {
      final Point p = (Point)listPoints.get( i );
      int x = (int)p.getX();
      int y = (int)p.getY();
      final GM_Position pos = GeometryFactory.createGM_Position( m_projection.getSourceX( x ), m_projection
          .getSourceY( y ) );
      positions.add( pos );
    }
    return positions;
  }

  private GM_Surface getPolygon( final List posArray ) throws GM_Exception
  {

    // close the ring
    posArray.add( posArray.get( 0 ) );
    final GM_Position[] positions = (GM_Position[])posArray.toArray( new GM_Position[posArray.size()] );
    return GeometryFactory.createGM_Surface( positions, null, null, m_coordinatesSystem );
  }

  private GM_Curve getLineString( final List posArray ) throws GM_Exception
  {
    return GeometryFactory.createGM_Curve( (GM_Position[])posArray.toArray( new GM_Position[posArray.size()] ),
        m_coordinatesSystem );
  }

  /**
   * @return a point
   */
  private GM_Point getPoint( final List posArray )
  {
    final GM_Position pos = (GM_Position)posArray.get( 0 );
    return GeometryFactory.createGM_Point( pos.getX(), pos.getY(), m_coordinatesSystem );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#doubleClickedLeft(java.awt.Point)
   */
  public void doubleClickedLeft( Point p )
  {
    if( !isValid() )
      return;
    if( !m_points.isEmpty() )
    {
      if( m_geometryProperty.getType().equals( GeometryUtilities.getPolygonClass().getName() ) && m_points.size() >= 3 )
        perform();
      if( m_geometryProperty.getType().equals( GeometryUtilities.getLineStringClass().getName() )
          && m_points.size() >= 2 )
        perform();
    }

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#doubleClickedRight(java.awt.Point)
   */
  public void doubleClickedRight( Point p )
  {
  // nothing
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  public void activate( ICommandTarget commandPoster, MapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );
    update( mapPanel );
  }

  private boolean isValid()
  {
    return m_valid;
  }

  private void update( MapPanel mapPanel )
  {
    try
    {
      final IKalypsoTheme activeTheme = mapPanel.getMapModell().getActiveTheme();
      if( activeTheme != null && activeTheme instanceof IKalypsoFeatureTheme )
      {
        m_coordinatesSystem = mapPanel.getMapModell().getCoordinatesSystem();
        m_projection = mapPanel.getProjection();
        final IKalypsoFeatureTheme fTheme = (IKalypsoFeatureTheme)activeTheme;
        m_workspace = fTheme.getWorkspace();
        m_featureType = fTheme.getFeatureType();
        // TODO ask for substitutions
        final FeatureList featureList = fTheme.getFeatureList();
        m_parentFeature = featureList.getParentFeature();
        m_linkFTP = featureList.getParentFeatureTypeProperty();
        // find geometryproperty
        final FeatureTypeProperty[] allGeomteryProperties = m_featureType.getAllGeomteryProperties();
        final List validGeometryFTPList = new ArrayList();
        for( int i = 0; i < allGeomteryProperties.length; i++ )
        {
          final FeatureTypeProperty property = allGeomteryProperties[i];
          if( m_apreferedGeometryClass == null || property.getType().equals( m_apreferedGeometryClass.getName() ) )
            validGeometryFTPList.add( property );
        }
        if( !validGeometryFTPList.isEmpty() ) // TODO ask if .size()> 1
          m_geometryProperty = (FeatureTypeProperty)validGeometryFTPList.get( 0 );
        else
          m_geometryProperty = null;
        m_valid = m_geometryProperty != null;
      }
      else
        m_valid = false;
    }
    catch( Exception e )
    {
      m_valid = false;
    }
  }

  private class NotEnoughPointsExeption extends Exception
  {}
}
