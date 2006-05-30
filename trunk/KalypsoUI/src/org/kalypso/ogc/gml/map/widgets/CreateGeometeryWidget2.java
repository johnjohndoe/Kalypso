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
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.ArrayUtils;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ui.editor.gmleditor.util.command.AddFeatureCommand;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
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
  private final List<Point> m_points = new ArrayList<Point>();

  // this is the point currently under the mouse
  private Point m_currentPoint = null;

  private Class m_apreferedGeometryClass = null;

  private IFeatureType m_featureType = null;

  private Feature m_parentFeature = null;

  private IRelationType m_linkFTP = null;

  private boolean m_valid = true;

  private CommandableWorkspace m_workspace;

  private IValuePropertyType m_geometryProperty = null;

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
  private void clear( )
  {
    m_points.clear();
    m_currentPoint = null;
  }

  /**
   * @throws GM_Exception
   * @throws NotEnoughPointsExeption
   */
  private GM_Object createGeometry( final List<Point> pixelArray ) throws GM_Exception, NotEnoughPointsExeption
  {
    final Class geoClass = m_geometryProperty.getValueClass();
    if( geoClass == GeometryUtilities.getPolygonClass() && pixelArray.size() < 3 )
      throw new NotEnoughPointsExeption();
    if( geoClass == GeometryUtilities.getLineStringClass() && pixelArray.size() < 2 )
      throw new NotEnoughPointsExeption();

    final List<GM_Position> posArray = getPositionArray( pixelArray );
    GM_Object result = null;
    if( geoClass == GeometryUtilities.getPolygonClass() )
      result = getPolygon( posArray );
    else if( geoClass == GeometryUtilities.getLineStringClass() )
      result = getLineString( posArray );
    else if( geoClass == GeometryUtilities.getPointClass() )
      result = getPoint( posArray );
    // TODO support the multis ...
    // test it
    return result;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftClicked(java.awt.Point)
   */
  @Override
  public void leftClicked( final Point p )
  {
    if( !isValid() )
      return;

    if( !m_points.isEmpty() && m_points.get( m_points.size() - 1 ).equals( p ) )
      return;
    // first test if vaild...
    final List<Point> testList = new ArrayList<Point>();
    for( final Point point : m_points )
      testList.add( point );
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
        if( m_geometryProperty.getValueClass() == GeometryUtilities.getPointClass() )
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
  @Override
  public void dragged( Point p )
  {

    if( m_points.isEmpty() || m_points.get( m_points.size() - 1 ).distance( p ) > MIN_DRAG_DISTANCE_PIXEL )
      leftClicked( p );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#dragged(java.awt.Point)
   */
  @Override
  public void moved( Point p )
  {
    if( !m_points.isEmpty() )
      m_currentPoint = p;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( Graphics g )
  {
    if( !isValid() )
      return;
    if( !m_points.isEmpty() && m_currentPoint != null )
    {
      final int[] arrayX = getXArrayPixel();
      final int[] arrayY = getYArrayPixel();
      final Class geoClass = m_geometryProperty.getValueClass();
      if( geoClass == GeometryUtilities.getPolygonClass() )
      {
        // paint polygon
        g.drawPolygon( arrayX, arrayY, arrayX.length );
        drawHandles( g, arrayX, arrayY );
      }
      else if( geoClass == GeometryUtilities.getLineStringClass() )
      {
        // paint linestring
        g.drawPolyline( arrayX, arrayY, arrayX.length );
        drawHandles( g, arrayX, arrayY );
      }
      else if( geoClass == GeometryUtilities.getPointClass() )
      {
        drawHandles( g, arrayX, arrayY );
      }
    }
  }

  /**
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
   * @return all the y poaints as array including the current point
   */
  private int[] getYArrayPixel( )
  {
    final List<Integer> yArray = new ArrayList<Integer>();
    for( int i = 0; i < m_points.size(); i++ )
    {
      Point p = m_points.get( i );
      yArray.add( new Integer( (int) p.getY() ) );
    }
    if( m_currentPoint != null )
      yArray.add( new Integer( m_currentPoint.y ) );
    return ArrayUtils.toPrimitive( yArray.toArray( new Integer[m_points.size()] ) );
  }

  /**
   * @return all the x points as array including the current point
   */
  private int[] getXArrayPixel( )
  {
    final List<Integer> xArray = new ArrayList<Integer>();
    for( int i = 0; i < m_points.size(); i++ )
    {
      final Point p = m_points.get( i );
      xArray.add( new Integer( (int) p.getX() ) );
    }
    if( m_currentPoint != null )
      xArray.add( new Integer( m_currentPoint.x ) );
    return ArrayUtils.toPrimitive( xArray.toArray( new Integer[m_points.size()] ) );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#perform()
   */
  public void perform( )
  {
    if( !isValid() )
      return;
    try
    {
      // final GM_Object geometry = createGeometry( m_points );
      if( m_validGeometryValue == null ) // nothing to perform
        return;
      final Map<IPropertyType, Object> valueMap = new HashMap<IPropertyType, Object>();
      valueMap.put( m_geometryProperty, m_validGeometryValue );
      final ICommand command = new AddFeatureCommand( m_workspace, m_featureType, m_parentFeature, m_linkFTP, 0, valueMap );
      m_workspace.postCommand( command );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    clear();
  }

  private List<GM_Position> getPositionArray( final List<Point> listPoints )
  {
    final List<GM_Position> positions = new ArrayList<GM_Position>();
    for( int i = 0; i < listPoints.size(); i++ )
    {
      final Point p = listPoints.get( i );
      int x = (int) p.getX();
      int y = (int) p.getY();
      final GM_Position pos = GeometryFactory.createGM_Position( m_projection.getSourceX( x ), m_projection.getSourceY( y ) );
      positions.add( pos );
    }
    return positions;
  }

  private GM_Surface getPolygon( final List<GM_Position> posArray ) throws GM_Exception
  {
    // close the ring
    posArray.add( posArray.get( 0 ) );
    final GM_Position[] positions = posArray.toArray( new GM_Position[posArray.size()] );
    return GeometryFactory.createGM_Surface( positions, new GM_Position[0][0], null, m_coordinatesSystem );
  }

  private GM_Curve getLineString( final List<GM_Position> posArray ) throws GM_Exception
  {
    return GeometryFactory.createGM_Curve( posArray.toArray( new GM_Position[posArray.size()] ), m_coordinatesSystem );
  }

  /**
   * @return a point
   */
  private GM_Point getPoint( final List posArray )
  {
    final GM_Position pos = (GM_Position) posArray.get( 0 );
    return GeometryFactory.createGM_Point( pos.getX(), pos.getY(), m_coordinatesSystem );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#doubleClickedLeft(java.awt.Point)
   */
  @Override
  public void doubleClickedLeft( Point p )
  {
    if( !isValid() )
      return;
    if( !m_points.isEmpty() )
    {
      final Class geoClass = m_geometryProperty.getValueClass();
      if( geoClass == GeometryUtilities.getPolygonClass() && m_points.size() >= 3 )
        perform();
      if( geoClass == GeometryUtilities.getLineStringClass() && m_points.size() >= 2 )
        perform();
    }

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#doubleClickedRight(java.awt.Point)
   */
  @Override
  public void doubleClickedRight( Point p )
  {
    // nothing
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public void activate( ICommandTarget commandPoster, MapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );
    update( mapPanel );
  }

  private boolean isValid( )
  {
    return m_valid;
  }

  private void update( MapPanel mapPanel )
  {
    try
    {
      if( mapPanel == null )
        return; // nothing to do
      final IMapModell mapModell = mapPanel.getMapModell();
      if( mapModell == null )
        return;
      final IKalypsoTheme activeTheme = mapModell.getActiveTheme();
      if( activeTheme != null && activeTheme instanceof IKalypsoFeatureTheme )
      {
        m_coordinatesSystem = mapModell.getCoordinatesSystem();
        m_projection = mapPanel.getProjection();
        final IKalypsoFeatureTheme fTheme = (IKalypsoFeatureTheme) activeTheme;
        m_workspace = fTheme.getWorkspace();
        m_featureType = fTheme.getFeatureType();
        // TODO ask for substitutions
        final FeatureList featureList = fTheme.getFeatureList();
        m_parentFeature = featureList.getParentFeature();
        m_linkFTP = featureList.getParentFeatureTypeProperty();
        // find geometryproperty
        final IValuePropertyType[] allGeomteryProperties = m_featureType.getAllGeomteryProperties();
        final List<IValuePropertyType> validGeometryFTPList = new ArrayList<IValuePropertyType>();
        for( int i = 0; i < allGeomteryProperties.length; i++ )
        {
          final IValuePropertyType property = allGeomteryProperties[i];
          if( m_apreferedGeometryClass == null || property.getValueClass() == m_apreferedGeometryClass )
            validGeometryFTPList.add( property );
        }
        if( !validGeometryFTPList.isEmpty() ) // TODO ask if .size()> 1
          m_geometryProperty = validGeometryFTPList.get( 0 );
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

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#keyReleased(java.awt.event.KeyEvent)
   */
  @Override
  public void keyReleased( KeyEvent e )
  {
    if( e.getKeyCode() == KeyEvent.VK_ESCAPE )
    {
      clear();
    }
  }

  private class NotEnoughPointsExeption extends Exception
  {
  }
}
