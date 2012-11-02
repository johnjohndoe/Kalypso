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
package org.kalypso.kalypsomodel1d2d.ui.map;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gml.processes.constDelaunay.ConstraintDelaunayHelper;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.DeletePolyElementCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.element1d.Create2dElementCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.util.PointSnapper;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.utilities.tooltip.ToolTipRenderer;
import org.kalypso.ogc.gml.map.widgets.builders.IGeometryBuilder;
import org.kalypso.ogc.gml.map.widgets.builders.LineGeometryBuilder;
import org.kalypso.ogc.gml.map.widgets.builders.PolygonGeometryBuilder;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.widgets.DeprecatedMouseWidget;
import org.kalypsodeegree.graphics.displayelements.DisplayElement;
import org.kalypsodeegree.graphics.sld.LineSymbolizer;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_AbstractSurfacePatch;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree.model.geometry.GM_PolygonPatch;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Triangle;
import org.kalypsodeegree_impl.graphics.displayelements.DisplayElementFactory;
import org.kalypsodeegree_impl.graphics.sld.LineSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.Stroke_Impl;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.refinement.Refinement;

/**
 * This widget is used in order to refine an existing 2D mesh by drawing a refinement line. The user gets a preview
 * before he starts the refinement of the model.
 * <ul>
 * <li>resulting elements with 5 corners are getting split into triangles by simple polygon triangulation
 * <li>arcs cannot be split twice
 * </ul>
 * 
 * @author Thomas Jung
 */
public class RefineFEGeometryWidget extends DeprecatedMouseWidget
{
  private boolean m_modePolygon = false;

  private Point m_currentMapPoint;

  private PointSnapper m_pointSnapper;

  private IGeometryBuilder m_geometryBuilder = null;

  private IKalypsoFeatureTheme m_theme;

  private IFEDiscretisationModel1d2d m_model1d2d;

  private final ToolTipRenderer m_toolTipRenderer = new ToolTipRenderer();

  private final ToolTipRenderer m_warningRenderer = new ToolTipRenderer();

  private GM_Object[] m_objects;

  private List<IPolyElement> m_featuresToRefine;

  private boolean m_warning;

  private GM_Object m_geom;

  public RefineFEGeometryWidget( )
  {
    super( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.RefineFEGeometryWidget.0" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.RefineFEGeometryWidget.1" ) ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  @Override
  public void activate( final ICommandTarget commandPoster, final IMapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );
    reinit();
  }

  private final void reinit( )
  {
    final IMapPanel mapPanel = getMapPanel();
    final IMapModell mapModell = mapPanel.getMapModell();
    mapPanel.repaintMap();

    m_theme = UtilMap.findEditableTheme( mapPanel, IPolyElement.QNAME );
    m_model1d2d = UtilMap.findFEModelTheme( mapPanel );

    final String mode = m_modePolygon ? Messages.getString( "RefineFEGeometryWidget.0" ) : Messages.getString( "RefineFEGeometryWidget.1" ); //$NON-NLS-1$ //$NON-NLS-2$
    final String modeTooltip = String.format( Messages.getString( "RefineFEGeometryWidget.2" ), mode ); //$NON-NLS-1$
    m_toolTipRenderer.setBackgroundColor( new Color( 1f, 1f, 0.6f, 0.70f ) );
    m_toolTipRenderer.setTooltip( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.RefineFEGeometryWidget.2" ) + modeTooltip ); //$NON-NLS-1$

    m_warningRenderer.setBackgroundColor( new Color( 1f, 0.4f, 0.4f, 0.80f ) );

    if( m_modePolygon )
      m_geometryBuilder = new PolygonGeometryBuilder( 0, mapModell.getCoordinatesSystem() );
    else
      m_geometryBuilder = new LineGeometryBuilder( 0, mapModell.getCoordinatesSystem() );
    m_pointSnapper = new PointSnapper( m_model1d2d, mapPanel );

    m_objects = null;
  }

  @Override
  public void leftPressed( final Point p )
  {
    m_warning = false;

    try
    {
      if( m_geometryBuilder == null )
        return;

      final IMapPanel mapPanel = getMapPanel();
      final Object newNode = checkNewNode( p );
      if( newNode == null )
        mapPanel.setCursor( Cursor.getPredefinedCursor( Cursor.CROSSHAIR_CURSOR ) );

      if( newNode instanceof IFE1D2DNode )
      {
        final GM_Point point = ((IFE1D2DNode)newNode).getPoint();
        m_currentMapPoint = MapUtilities.retransform( getMapPanel(), point );
        m_geometryBuilder.addPoint( point );
      }
      else
      {
        m_currentMapPoint = p;
        m_geometryBuilder.addPoint( MapUtilities.transform( mapPanel, p ) );
      }
      mapPanel.setCursor( Cursor.getDefaultCursor() );

    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      KalypsoModel1D2DPlugin.getDefault().getLog().log( status );
      reinit();
    }
  }

  @Override
  public void moved( final Point p )
  {
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;

    final Object newNode = checkNewNode( p );
    if( newNode instanceof IFE1D2DNode )
    {
      final IFE1D2DNode candidateNode = (IFE1D2DNode)newNode;
      m_currentMapPoint = MapUtilities.retransform( getMapPanel(), candidateNode.getPoint() );
    }
    else
      m_currentMapPoint = p;

    if( newNode == null )
      getMapPanel().setCursor( Cursor.getPredefinedCursor( Cursor.CROSSHAIR_CURSOR ) );
    else
      getMapPanel().setCursor( Cursor.getDefaultCursor() );

    repaintMap();

  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    /* always paint a small rectangle of current position */
    if( m_currentMapPoint == null )
      return;

    final int[][] posPoints = UtilMap.getPointArrays( m_currentMapPoint );

    final int[] arrayX = posPoints[0];
    final int[] arrayY = posPoints[1];

    /* Paint as linestring. */
    g.drawPolygon( arrayX, arrayY, arrayX.length );
    UtilMap.drawHandles( g, arrayX, arrayY );

    /* paint the snap */
    if( m_pointSnapper != null )
      m_pointSnapper.paint( g );

    super.paint( g );

    final IMapPanel mapPanel = getMapPanel();
    final Rectangle bounds = mapPanel.getScreenBounds();
    final GeoTransform projection = mapPanel.getProjection();

    m_toolTipRenderer.paintToolTip( new Point( 5, bounds.height - 5 ), g, bounds );

    if( m_warning == true )
      m_warningRenderer.paintToolTip( new Point( 5, bounds.height - 80 ), g, bounds );

    if( m_geometryBuilder != null )
      m_geometryBuilder.paint( g, projection, m_currentMapPoint );
    try
    {
      if( m_objects != null )
        if( m_objects.length > 0 )
          drawRefinement( g, projection );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  @SuppressWarnings( { "unchecked", "rawtypes" } )
  private void drawRefinement( final Graphics g, final GeoTransform projection ) throws CoreException, GM_Exception
  {
    /* Paint a rect. */
    final LineSymbolizer symb = new LineSymbolizer_Impl();
    final Stroke stroke = new Stroke_Impl( new HashMap(), null, null );
    final Color color = new Color( 255, 0, 0 );
    for( final GM_Object object : m_objects )
    {
      if( object instanceof GM_Polygon )
      {
        final GM_Polygon surface = (GM_Polygon)object;
        final String crs = surface.getCoordinateSystem();

        for( final GM_AbstractSurfacePatch surfacePatch : surface )
        {
          final GM_Position[] exteriorRing = surfacePatch.getExteriorRing();
          final GM_Curve curve = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Curve( exteriorRing, crs );

          stroke.setWidth( 3 );
          stroke.setLineCap( 2 ); // round
          stroke.setStroke( color );
          symb.setStroke( stroke );

          final DisplayElement de = DisplayElementFactory.buildLineStringDisplayElement( null, curve, symb );
          de.paint( g, projection, new NullProgressMonitor() );
        }
      }
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.SnapToGeometryWidget#keyPressed(java.awt.event.KeyEvent)
   */
  @Override
  public void keyPressed( final KeyEvent e )
  {
    if( e.getKeyCode() == KeyEvent.VK_SPACE )
    {
      m_modePolygon = !m_modePolygon;
      reinit();
    }
    else if( e.getKeyCode() == KeyEvent.VK_ESCAPE )
      reinit();
    else if( e.getKeyCode() == KeyEvent.VK_BACK_SPACE )
      m_geometryBuilder.removeLastPoint();
    else if( e.getKeyCode() == KeyEvent.VK_ENTER )
      convertRefinementToModel();
    else
      super.keyPressed( e );
  }

  private void convertRefinementToModel( )
  {
    if( m_objects == null )
      return;

    // first, we re-select all features that lie on the refined GM_Object's center points.
    // This is necessary because of some additional filters used in the refinement class.

    /* calculate centroids of refinements */
    final List<GM_Point> centroidList = getCentroids( m_objects );
    /* reselect */
    final List<IPolyElement> refineList = reselectFeatures( centroidList );

    try
    {
      final CommandableWorkspace workspace = m_theme.getWorkspace();

      /* Initialize elements needed for edges and elements */
      final IFEDiscretisationModel1d2d discModel = (IFEDiscretisationModel1d2d)workspace.getRootFeature();

      // add remove element command
      final DeletePolyElementCmd deleteCommand = new DeletePolyElementCmd( discModel );
      for( final IPolyElement feature : refineList )
      {
        deleteCommand.addElementToRemove( feature );
      }
      try
      {
        workspace.postCommand( deleteCommand );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }

      /* create new elements */
      for( final GM_Object object : m_objects )
      {
        if( object instanceof GM_Polygon )
        {
          final GM_Polygon surface = (GM_Polygon)object;
          final GM_Position[] poses = surface.getSurfacePatch().getExteriorRing();
          final GM_Point[] points = new GM_Point[poses.length];
          for( int i = 0; i < poses.length - 1; i++ )
            points[i] = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Point( poses[i], surface.getCoordinateSystem() );

          final Create2dElementCommand createCommand = new Create2dElementCommand( discModel, points );
          workspace.postCommand( createCommand );
        }
      }

      reinit();
    }
    catch( final Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }

  private List<IPolyElement> reselectFeatures( final List<GM_Point> centroidList )
  {
    final List<IPolyElement> refineList = new ArrayList<>();
    for( final IPolyElement feature : m_featuresToRefine )
    {
      final GM_Polygon surface = feature.getGeometry();
      for( final GM_Point centroid : centroidList )
      {
        if( surface.intersects( centroid ) )
          refineList.add( feature );
      }
    }
    return refineList;
  }

  private List<GM_Point> getCentroids( final GM_Object[] objects )
  {
    final List<GM_Point> centroidList = new ArrayList<>();

    for( final GM_Object object : objects )
    {
      if( object instanceof GM_Polygon )
      {
        final GM_Polygon surf = (GM_Polygon)object;
        final GM_Point centroid = surf.getCentroid();

        centroidList.add( centroid );
      }
    }
    return centroidList;
  }

  @Override
  public void doubleClickedLeft( final Point p )
  {
    if( m_geometryBuilder == null )
      return;

    try
    {
      m_geom = m_geometryBuilder.finish();
      finishGeometry();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      KalypsoModel1D2DPlugin.getDefault().getLog().log( status );
      final IMapPanel mapPanel = getMapPanel();
      mapPanel.setMessage( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.RefineFEGeometryWidget.4" ) + status.getMessage() ); //$NON-NLS-1$
      reinit();
    }
  }

  protected void finishGeometry( ) throws GM_Exception
  {
    if( m_geom == null )
      return;

    m_warning = false;

    m_featuresToRefine = new ArrayList<>();

    /* select features */
    final String crs = m_geom.getCoordinateSystem();

    final List<IPolyElement> selectedFeatures = doSelect( m_geom );

    final List<GM_Polygon> surfaceList = new ArrayList<>();

    for( final IPolyElement feature : selectedFeatures )
    {
      // get the geometry
      final GM_Polygon surface = feature.getGeometry();
      surfaceList.add( surface );

      // get the selected Feature
      m_featuresToRefine.add( feature );
    }

    /* REFINEMENT */
    final GM_MultiSurface multiSurface = GeometryFactory.createGM_MultiSurface( surfaceList.toArray( new GM_Polygon[surfaceList.size()] ), crs );
    final GM_MultiSurface[] multiSurfaces = new GM_MultiSurface[] { multiSurface };

    final Refinement refinement = new Refinement();

    final GM_Object[] refinements = refinement.doRefine( multiSurfaces, m_geom );

    final List<GM_Polygon> refinementList = new ArrayList<>();

    for( final GM_Object refineGeom : refinements )
    {
      if( refineGeom instanceof GM_Polygon )
      {
        final GM_Polygon surface = (GM_Polygon)refineGeom;
        final GM_PolygonPatch surfacePatch = surface.getSurfacePatch();
        final int nodeCount = surfacePatch.getExteriorRing().length;
        final GM_Polygon newSurface = GeometryFactory.createGM_Surface( surfacePatch );
        if( nodeCount > 5 || !newSurface.getConvexHull().equals( newSurface ) )
        {
          final GM_Triangle[] triangles = ConstraintDelaunayHelper.convertToTriangles( newSurface, newSurface.getCoordinateSystem() );
          for( final GM_Triangle triangle : triangles )
            refinementList.add( GeometryFactory.createGM_Surface( triangle ) );
        }
        else
        {
          refinementList.add( newSurface );
        }
      }
    }

    if( refinementList.size() == 0 )
    {
      m_warning = true;
      m_warningRenderer.setTooltip( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.RefineFEGeometryWidget.5" ) ); //$NON-NLS-1$
    }

    // create new GM_Objects
    m_objects = refinementList.toArray( new GM_Polygon[refinementList.size()] );

    m_geometryBuilder.reset();

    getMapPanel().repaintMap();
  }

  private List<IPolyElement> doSelect( final GM_Object selectGeometry )
  {
    if( selectGeometry == null )
      return null;

    // select feature from featureList by using the selectGeometry
    final List<IPolyElement> selectedFeatures = new ArrayList<>();

    final List<IPolyElement> selectedSubList = selectFeatures( selectGeometry );
    if( selectedSubList != null )
      selectedFeatures.addAll( selectedSubList );

    return selectedFeatures;
  }

  private List<IPolyElement> selectFeatures( final GM_Object theGeom )
  {
    final List<IPolyElement> selectedFeatures = new ArrayList<>();

    // *** Why this??
    GM_Object selectGeometry = theGeom;
    try
    {
      if( theGeom instanceof GM_Curve )
      {
        final GM_Position[] positions = ((GM_Curve)theGeom).getAsLineString().getPositions();
        selectGeometry = GeometryFactory.createGM_Surface( positions, null, selectGeometry.getCoordinateSystem() );
      }
    }
    catch( final GM_Exception e )
    {
    }

    // *****

    final GM_Envelope envelope = selectGeometry.getEnvelope();
    final List<IFE1D2DElement> result = m_model1d2d.queryElements( envelope, null );

    for( final IFE1D2DElement element : result )
    {
      if( element instanceof IPolyElement )
      {
        final IPolyElement polyElement = (IPolyElement)element;
        final GM_Object geom = polyElement.getGeometry();
        if( geom != null )
        {
          final GM_Object intersection = selectGeometry.intersection( geom );
          if( intersection != null )
            selectedFeatures.add( polyElement );
        }
      }
    }

    return selectedFeatures;
  }

  private Object checkNewNode( final Point p )
  {
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return null;

    final GM_Point currentPoint = MapUtilities.transform( mapPanel, p );
    return m_pointSnapper == null ? null : m_pointSnapper.moved( currentPoint );
  }
}
