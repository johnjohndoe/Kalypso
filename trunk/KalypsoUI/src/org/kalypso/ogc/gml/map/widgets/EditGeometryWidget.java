/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005, 2006 by:

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
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.gml.map.widgets;

import java.awt.Graphics;
import java.awt.Point;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.command.Handle;
import org.kalypso.ogc.gml.command.JMSelector;
import org.kalypso.ogc.gml.command.ModifyFeatureGeometryCommand;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Ring;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfaceBoundary;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;

/**
 * widget to edit geometries<br>
 * works on selected theme<br>
 * <br>
 * display handles near mouse<br>
 * simply move handles with left drag<br>
 * 
 * @author doemming
 */
public class EditGeometryWidget extends AbstractWidget
{

  double m_boxRadiusVisibleHandles = 50;

  int m_boxRadiusDrawnHandle = 10;

  double m_gisRadiusTopology = 10;

  boolean m_careTopology = false;

  // list of handles
  private List<Handle> m_handles = new ArrayList<Handle>();

  // handles while editing
  private List<Handle> m_editHandles = null;

  private Point m_startPoint = null;

  private Point m_dragPoint = null;

  public EditGeometryWidget( String name, String toolTip )
  {
    super( name, toolTip );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#moved(java.awt.Point)
   */
  @Override
  public void moved( Point p )
  {
    if( p == null || isInEditingMode() )
      return;
    final GeoTransform transform = getMapPanel().getProjection();

    final IKalypsoTheme activeTheme = getActiveTheme();
    if( activeTheme == null || !(activeTheme instanceof IKalypsoFeatureTheme) )
      return;
    // GM_Envelope envelope = getMapPanel().getBoundingBox();
    double minX = transform.getSourceX( p.getX() - m_boxRadiusVisibleHandles );
    double minY = transform.getSourceY( p.getY() - m_boxRadiusVisibleHandles );
    double maxX = transform.getSourceX( p.getX() + m_boxRadiusVisibleHandles );
    double maxY = transform.getSourceY( p.getY() + m_boxRadiusVisibleHandles );
    // valid envelope with handles
    final GM_Envelope envelope = GeometryFactory.createGM_Envelope( minX, minY, maxX, maxY );
    final JMSelector selector = new JMSelector();
    // final FeatureList featureListVisible = ((IKalypsoFeatureTheme) activeTheme).getFeatureListVisible( null );
    final FeatureList featureListVisible = ((IKalypsoFeatureTheme) activeTheme).getFeatureList();
    final List<Feature> features = selector.select( envelope, featureListVisible, false );
    m_handles = createHandles( features, null, envelope );
  }

  /**
   * @return <code>true</code> if user is moving selected handles at the moment
   */
  private boolean isInEditingMode( )
  {
    return m_editHandles != null;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftPressed(java.awt.Point)
   */
  @Override
  public void leftPressed( final Point p )
  {
    // start editing mode
    m_startPoint = p;
    m_dragPoint = p;
    m_editHandles = filter( m_handles, p, null );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#dragged(java.awt.Point)
   */
  @Override
  public void dragged( Point p )
  {
    m_dragPoint = p;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#rightClicked(java.awt.Point)
   */
  @Override
  public void middleClicked( Point p )
  {
    final GeoTransform transform = getMapPanel().getProjection();
    final IKalypsoTheme activeTheme = getActiveTheme();
    if( activeTheme == null || !(activeTheme instanceof IKalypsoFeatureTheme) )
      return;
    double g1 = transform.getSourceX( p.getX() );
    double g2 = transform.getSourceX( p.getX() + m_boxRadiusDrawnHandle );
    m_gisRadiusTopology = Math.abs( g2 - g1 );
    m_careTopology = !m_careTopology;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftReleased(java.awt.Point)
   */
  @Override
  public void leftReleased( Point p )
  {
    perform();
    // end editing mode for next editing
    m_editHandles = null;
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#perform()
   */
  public void perform( )
  {
    final GeoTransform transform = getMapPanel().getProjection();
    final IKalypsoTheme activeTheme = getActiveTheme();
    if( isInEditingMode() )
    {
      if( m_startPoint != null && m_dragPoint != null )
      {
        final double gDragX = transform.getSourceX( m_dragPoint.getX() );
        final double gDragY = transform.getSourceY( m_dragPoint.getY() );

        final double gStartX = transform.getSourceX( m_startPoint.getX() );
        final double gStartY = transform.getSourceY( m_startPoint.getY() );

        final double[] translation = new double[] { gDragX - gStartX, gDragY - gStartY };
        final double[] undoTranslation = new double[] { -translation[0], -translation[1] };

        // test for validity
        for( final Handle handle : m_editHandles )
        {
          final Feature feature = handle.getFeature();
          final IValuePropertyType propertyType = handle.getPropertyType();
          final GM_Position position = handle.getPosition();
          position.translate( translation );
          final GM_Object geom = (GM_Object) feature.getProperty( propertyType );
          final Geometry geometry;
          boolean valid = false;
          try
          {
            geometry = JTSAdapter.export( geom );
            valid = geometry.isValid()&& geometry.isSimple();
          }
          catch( Exception e )
          {
            // invalid geometry do not perform
            return;
          }
          finally
          {
            position.translate( undoTranslation );
          }
          if( !valid )
            return;// invalid geometry do not perform
        }

        final IKalypsoFeatureTheme fTheme = (IKalypsoFeatureTheme) activeTheme;
        final CommandableWorkspace workspace = fTheme.getWorkspace();
        final ModifyFeatureGeometryCommand command = new ModifyFeatureGeometryCommand( workspace, m_editHandles, translation );
        try
        {
          workspace.postCommand( command );
        }
        catch( Exception e )
        {
          // TODO Auto-generated catch block
          e.printStackTrace();
        }

      }
    }
    m_editHandles = null;
  }

  /**
   * topology-mode=on : all handles relevant<br>
   * topology-mode=off : nearest handles <br>
   * 
   * @return filteres handles
   */
  private List<Handle> filter( List<Handle> handles, Point pointOfInterest, List<Handle> collector )
  {
    if( collector == null )
      collector = new ArrayList<Handle>();

    final GeoTransform transform = getMapPanel().getProjection();
    final IKalypsoTheme activeTheme = getActiveTheme();
    if( activeTheme == null || !(activeTheme instanceof IKalypsoFeatureTheme) )
      return collector;

    final double gisX = transform.getSourceX( pointOfInterest.getX() );
    final double gisY = transform.getSourceY( pointOfInterest.getY() );
    final GM_Position positionOfInterest = GeometryFactory.createGM_Position( gisX, gisY );

    final List<Handle> checkForTopology = new ArrayList<Handle>();
    // 1. select nearest handle
    Handle nearest = null;
    double minDistance = -1;
    for( final Handle handle : handles )
    {
      final GM_Position handlePosition = handle.getPosition();
      final double pX = transform.getDestX( handlePosition.getX() );
      final double pY = transform.getDestY( handlePosition.getY() );
      // check if handle enclose pointOfInterest
      if( Math.abs( pX - pointOfInterest.getX() ) <= m_boxRadiusDrawnHandle//
          && //
          Math.abs( pY - pointOfInterest.getY() ) <= m_boxRadiusDrawnHandle//
      )
      {
        // filter also for topologyCheck
        if( m_careTopology )
          checkForTopology.add( handle );
        final double distance = positionOfInterest.getDistance( handlePosition );
        if( nearest == null || distance < minDistance )
        {
          minDistance = distance;
          nearest = handle;
        }
      }
    }
    // no result ?
    if( nearest == null )
      return collector;
    if( !m_careTopology )
    {
      collector.add( nearest );
      return collector;
    }
    // 2. care for topology...
    final GM_Position nearestPosition = nearest.getPosition();
    for( final Handle handle : handles )
    {
      final GM_Position position = handle.getPosition();
      final double distance = nearestPosition.getDistance( position );
      if( distance <= m_gisRadiusTopology )
        collector.add( handle );
    }
    System.out.println( collector.size() );
    return collector;
  }

  @Override
  public void paint( Graphics g )
  {
    final GeoTransform projection = getMapPanel().getProjection();
    int mask = Handle.MASK_BOX;
    if( m_careTopology )
      mask |= Handle.MASK_TOPOLOGY;
    if( isInEditingMode() )
    {
      if( m_startPoint != null && m_dragPoint != null )
      {
        int dx = (int) (m_dragPoint.getX() - m_startPoint.getX());
        int dy = (int) (m_dragPoint.getY() - m_startPoint.getY());
        for( final Handle handle : m_editHandles )
          handle.paint( g, projection, m_boxRadiusDrawnHandle, (int) m_gisRadiusTopology, dx, dy, Handle.MASK_BOX );
      }
    }
    else
      for( final Handle handle : m_handles )
        handle.paint( g, projection, m_boxRadiusDrawnHandle, (int) m_gisRadiusTopology, mask );
  }

  private List<Handle> createHandles( final List<Feature> features, List<Handle> collector, GM_Envelope envelope )
  {
    if( collector == null )
      collector = new ArrayList<Handle>();
    for( final Feature feature : features )
      createHandles( feature, collector, envelope );
    return collector;
  }

  private List<Handle> createHandles( final Feature feature, final List<Handle> collector, GM_Envelope envelope )
  {
    final IValuePropertyType[] geometryProperties = feature.getFeatureType().getAllGeomteryProperties();
    for( int i = 0; i < geometryProperties.length; i++ )
    {
      final IValuePropertyType propType = geometryProperties[i];
      final GM_Object geometry = (GM_Object) feature.getProperty( propType );
      createHandles( feature, propType, geometry, collector, envelope );
    }
    return collector;
  }

  private void createHandles( final Feature feature, final IValuePropertyType propType, final GM_Object geometry, final List<Handle> collector, GM_Envelope envelope )
  {
    if( geometry instanceof GM_Point )
      createPointHandles( feature, propType, (GM_Point) geometry, collector, envelope );
    else if( geometry instanceof GM_Curve )
      createCurveHandles( feature, propType, (GM_Curve) geometry, collector, envelope );
    else if( geometry instanceof GM_Surface )
      createSurfaceHandles( feature, propType, (GM_Surface) geometry, collector, envelope );
  }

  private void createPointHandles( final Feature feature, final IValuePropertyType propType, final GM_Point point, final List<Handle> collector, GM_Envelope envelope )
  {
    final GM_Position position = point.getPosition();
    if( envelope.contains( position ) )
      collector.add( new Handle( feature, propType, position ) );
  }

  private void createCurveHandles( final Feature feature, final IValuePropertyType propType, final GM_Curve curve, final List<Handle> collector, GM_Envelope envelope )
  {
    final GM_Position[] positions;
    try
    {
      positions = curve.getAsLineString().getPositions();
      createHandles( feature, propType, positions, collector, envelope );
    }
    catch( GM_Exception e )
    {
      // create no handles
      e.printStackTrace();
    }

  }

  private void createSurfaceHandles( final Feature feature, final IValuePropertyType propType, final GM_Surface surface, final List<Handle> collector, GM_Envelope envelope )
  {
    final GM_SurfaceBoundary surfaceBoundary = surface.getSurfaceBoundary();
    if( surfaceBoundary == null )
      return;
    final GM_Ring exteriorRing = surfaceBoundary.getExteriorRing();
    final GM_Position[] positionsExterior = exteriorRing.getPositions();
    createHandles( feature, propType, positionsExterior, collector, envelope );
    final GM_Ring[] interiorRings = surfaceBoundary.getInteriorRings();
    if( interiorRings != null )
      for( int i = 0; i < interiorRings.length; i++ )
      {
        final GM_Ring ring = interiorRings[i];
        GM_Position[] positionsInteror = ring.getPositions();
        createHandles( feature, propType, positionsInteror, collector, envelope );
      }
  }

  private void createHandles( final Feature feature, final IValuePropertyType propType, final GM_Position[] positions, final List<Handle> collector, GM_Envelope envelope )
  {
    final Set<GM_Position> pos = new HashSet<GM_Position>();
    for( int i = 0; i < positions.length; i++ )
    {
      final GM_Position position = positions[i];
      if( envelope.contains( position ) )
      {
        if( !pos.contains( position ) )
        {
          collector.add( new Handle( feature, propType, position ) );
          pos.add( position );
        }
      }
    }
  }

}