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
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gml.processes.constDelaunay.ConstraintDelaunayHelper;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.FE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.DeleteCmdFactory;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.DeletePolyElementCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.util.PointSnapper;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.utilities.tooltip.ToolTipRenderer;
import org.kalypso.ogc.gml.map.widgets.builders.IGeometryBuilder;
import org.kalypso.ogc.gml.map.widgets.builders.LineGeometryBuilder;
import org.kalypso.ogc.gml.map.widgets.builders.PolygonGeometryBuilder;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.widgets.AbstractWidget;
import org.kalypsodeegree.graphics.displayelements.DisplayElement;
import org.kalypsodeegree.graphics.sld.LineSymbolizer;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree.model.geometry.GM_Triangle;
import org.kalypsodeegree_impl.graphics.displayelements.DisplayElementFactory;
import org.kalypsodeegree_impl.graphics.sld.LineSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.Stroke_Impl;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
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
public class RefineFEGeometryWidget extends AbstractWidget
{
  private static final double SNAP_DISTANCE = 0.02;

  private boolean m_modePolygon = false;

  private Point m_currentMapPoint;

  private PointSnapper m_pointSnapper;

  private IGeometryBuilder m_geometryBuilder = null;

  private IKalypsoFeatureTheme m_theme;

  private IFEDiscretisationModel1d2d m_model1d2d;

  private final ToolTipRenderer m_toolTipRenderer = new ToolTipRenderer();

  private final ToolTipRenderer m_warningRenderer = new ToolTipRenderer();

  private FeatureList m_featureList;

  private GM_Object[] m_objects;

  private List<Feature> m_featuresToRefine;

  private boolean m_warning;

  @SuppressWarnings("rawtypes")
  private final Map<GM_Position, IFE1D2DNode> m_nodesNameConversionMap = new HashMap<GM_Position, IFE1D2DNode>();

  private GM_Object m_geom;

  public RefineFEGeometryWidget( )
  {
    super( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.RefineFEGeometryWidget.0" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.RefineFEGeometryWidget.1" ) ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
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

    final String mode = m_modePolygon ? Messages.getString("RefineFEGeometryWidget.0") : Messages.getString("RefineFEGeometryWidget.1"); //$NON-NLS-1$ //$NON-NLS-2$
    final String modeTooltip = String.format( Messages.getString("RefineFEGeometryWidget.2"), mode ); //$NON-NLS-1$
    m_toolTipRenderer.setBackgroundColor( new Color( 1f, 1f, 0.6f, 0.70f ) );
    m_toolTipRenderer.setTooltip( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.RefineFEGeometryWidget.2" ) + modeTooltip ); //$NON-NLS-1$

    m_warningRenderer.setBackgroundColor( new Color( 1f, 0.4f, 0.4f, 0.80f ) );

    if( m_modePolygon )
      m_geometryBuilder = new PolygonGeometryBuilder( 0, mapModell.getCoordinatesSystem() );
    else
      m_geometryBuilder = new LineGeometryBuilder( 0, mapModell.getCoordinatesSystem() );
    m_pointSnapper = new PointSnapper( m_model1d2d, mapPanel );

    final IKalypsoTheme activeTheme = mapModell.getActiveTheme();
    if( activeTheme instanceof IKalypsoFeatureTheme )
    {
      m_theme = (IKalypsoFeatureTheme) activeTheme;
      m_featureList = m_theme == null ? null : m_theme.getFeatureList();
    }

    m_objects = null;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftClicked(java.awt.Point)
   */
  @SuppressWarnings({ "rawtypes" })
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
        final GM_Point point = ((IFE1D2DNode) newNode).getPoint();
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

  @SuppressWarnings({ "rawtypes" })
  @Override
  public void moved( final Point p )
  {
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;

    final Object newNode = checkNewNode( p );
    if( newNode instanceof IFE1D2DNode )
    {
      final IFE1D2DNode candidateNode = (IFE1D2DNode) newNode;
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

  @SuppressWarnings({ "unchecked", "rawtypes" })
  private void drawRefinement( final Graphics g, final GeoTransform projection ) throws CoreException, GM_Exception
  {
    /* Paint a rect. */
    final LineSymbolizer symb = new LineSymbolizer_Impl();
    final Stroke stroke = new Stroke_Impl( new HashMap(), null, null );
    final Color color = new Color( 255, 0, 0 );
    for( final GM_Object object : m_objects )
    {
      if( object instanceof GM_Surface )
      {
        final GM_Surface<GM_SurfacePatch> surface = (GM_Surface) object;
        final String crs = surface.getCoordinateSystem();

        for( final GM_SurfacePatch surfacePatch : surface )
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
    else if( e.getKeyCode() == KeyEvent.VK_ENTER )
      convertRefinementToModel();
    else
      super.keyPressed( e );
  }

  @SuppressWarnings({ "unchecked" })
  private void convertRefinementToModel( )
  {
    if( m_objects == null )
      return;

    // first, we re-select all features that lie on the refined GM_Object's center points.
    // This is necessary because of some additional filters used in the refinement class.

    /* calculate centroids of refinements */
    final List<GM_Point> centroidList = getCentroids( m_objects );
    /* reselect */
    final List<Feature> refineList = reselectFeatures( centroidList );

    try
    {
      final CommandableWorkspace workspace = m_theme.getWorkspace();

      /* Initialize elements needed for edges and elements */
      final IFEDiscretisationModel1d2d discModel = (IFEDiscretisationModel1d2d) workspace.getRootFeature();

      // add remove element command
      final IDiscrModel1d2dChangeCommand deleteCmdPolyElement = DeleteCmdFactory.createDeleteCmdPoly( discModel );
      final List<Feature> elementsToRemove = new ArrayList<Feature>();
      for( final Feature feature : refineList )
      {
        if( GMLSchemaUtilities.substitutes( feature.getFeatureType(), IPolyElement.QNAME ) )
        {
          ((DeletePolyElementCmd) deleteCmdPolyElement).addElementToRemove( feature );
          elementsToRemove.add( feature );
          // final IDiscrModel1d2dChangeCommand deleteCmd = DeleteCmdFactory.createDeleteCmd( feature, discModel );
          // workspace.postCommand( deleteCmd );
        }
      }
      try
      {
        deleteCmdPolyElement.process();
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }

      discModel.getElements().removeAll( elementsToRemove );
      // workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, discModel,
      // elementsToRemove.toArray( new Feature[ elementsToRemove.size() ] ),
      // FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_DELETE ) );
      m_nodesNameConversionMap.clear();

      final List<Feature> lListAdded = new ArrayList<Feature>();
      /* create new elements */
      for( final GM_Object object : m_objects )
      {
        if( object instanceof GM_Surface )
        {
          final GM_Surface<GM_SurfacePatch> surface = (GM_Surface<GM_SurfacePatch>) object;
          lListAdded.addAll( createPolyElement( surface, discModel ) );
          // ElementGeometryHelper.createFE1D2DfromSurface( workspace, discModel, surface );
        }
      }

      if( lListAdded.size() > 0 )
      {
        FeatureStructureChangeModellEvent changeEvent = new FeatureStructureChangeModellEvent( workspace, discModel, lListAdded.toArray( new Feature[lListAdded.size()] ), FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD );
        workspace.fireModellEvent( changeEvent );
        Logger.getLogger( RefineFEGeometryWidget.class.getName() ).log( Level.INFO, "Model event fired: " + changeEvent ); //$NON-NLS-1$
      }
      reinit();
    }
    catch( final Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }

  @SuppressWarnings("rawtypes")
  private List<Feature> createPolyElement( final GM_Surface<GM_SurfacePatch> surface, final IFEDiscretisationModel1d2d discModel )
  {
    final List<Feature> lListRes = new ArrayList<Feature>();
    final List<IFE1D2DEdge> lListEdges = new ArrayList<IFE1D2DEdge>();
    for( final GM_SurfacePatch surfacePatch : surface )
    {
      final GM_Position[] poses = surfacePatch.getExteriorRing();
      final List<GM_Point> lListPoints = new ArrayList<GM_Point>();
      for( int i = 0; i < poses.length - 1; i++ )
        lListPoints.add( org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Point( poses[i], surface.getCoordinateSystem() ) );

      lListRes.addAll( createNodesAndEdges( discModel, lListEdges, lListPoints ) );

      final IPolyElement element2d = discModel.getElements().addNew( IPolyElement.QNAME, IPolyElement.class );
      lListRes.add( element2d );
      for( final IFE1D2DEdge lEdge : lListEdges )
      {
        // add edge to element and element to edge
        final String elementId = element2d.getId();
        element2d.addEdge( lEdge.getId() );
        lEdge.addContainer( elementId );
      }
    }

    return lListRes;
  }

  @SuppressWarnings("rawtypes")
  private List<Feature> createNodesAndEdges( final IFEDiscretisationModel1d2d discModel, final List<IFE1D2DEdge> lListEdges, final List<GM_Point> lListPoses )
  {
    final List<Feature> lListRes = new ArrayList<Feature>();
    IFE1D2DNode lastNode = null;
    int iCountNodes = 0;
    if( lListPoses.size() > 0 && !lListPoses.get( lListPoses.size() - 1 ).equals( lListPoses.get( 0 ) ) )
    {
      lListPoses.add( lListPoses.get( 0 ) );
    }
    for( final GM_Point lPoint : lListPoses )
    {
      IFE1D2DNode actNode = m_nodesNameConversionMap.get( lPoint.getPosition() );

      if( actNode == null )
      {
        actNode = discModel.findNode( lPoint, SNAP_DISTANCE );
      }

      if( actNode == null )
      {
        actNode = discModel.createNode( lPoint, -1, new boolean[1] );
        if( actNode == null )
        {
          return new ArrayList<Feature>();
        }
        m_nodesNameConversionMap.put( lPoint.getPosition(), actNode );
        lListRes.add( actNode );
      }

      if( iCountNodes > 0 )
      {
        final IFE1D2DEdge existingEdge = discModel.findEdge( lastNode, actNode );
        final IFE1D2DEdge edge;
        if( existingEdge != null )
        {
          edge = existingEdge;
        }
        else
        {
          edge = FE1D2DEdge.createFromModel( discModel, lastNode, actNode );
          lListRes.add( edge );
        }
        lListEdges.add( edge );
        // final String gmlID = edge.getId();
      }
      iCountNodes++;
      lastNode = actNode;
    }
    return lListRes;
  }

  @SuppressWarnings({ "unchecked", "rawtypes" })
  private List<Feature> reselectFeatures( final List<GM_Point> centroidList )
  {
    final List<Feature> refineList = new ArrayList<Feature>();
    for( final Feature feature : m_featuresToRefine )
    {
      if( GMLSchemaUtilities.substitutes( feature.getFeatureType(), IPolyElement.QNAME ) )
      {
        final GM_Object geom = (GM_Object) feature.getProperty( IFE1D2DElement.PROP_GEOMETRY );
        if( geom instanceof GM_Surface )
        {
          final GM_Surface<GM_SurfacePatch> surface = (GM_Surface) geom;

          for( final GM_Point centroid : centroidList )
          {
            if( surface.intersects( centroid ) )
              refineList.add( feature );
          }
        }
      }
    }
    return refineList;
  }

  @SuppressWarnings({ "unchecked" })
  private List<GM_Point> getCentroids( final GM_Object[] objects )
  {
    final List<GM_Point> centroidList = new ArrayList<GM_Point>();

    for( final GM_Object object : objects )
    {
      if( object instanceof GM_Surface )
      {
        final GM_Surface<GM_SurfacePatch> surf = (GM_Surface<GM_SurfacePatch>) object;
        final GM_Point centroid = surf.getCentroid();

        centroidList.add( centroid );
      }
    }
    return centroidList;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#doubleClickedLeft(java.awt.Point)
   */
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

  @SuppressWarnings({ "unchecked", "rawtypes" })
  protected void finishGeometry( ) throws GM_Exception
  {
    if( m_geom == null )
      return;

    m_warning = false;

    m_featuresToRefine = new ArrayList<Feature>();

    if( m_featureList == null )
      return;

    /* select features */
    final String crs = m_geom.getCoordinateSystem();

    final List<Feature> selectedFeatures = doSelect( m_geom, m_featureList );

    final List<GM_Surface> surfaceList = new ArrayList<GM_Surface>();

    for( final Feature feature : selectedFeatures )
    {
      if( GMLSchemaUtilities.substitutes( feature.getFeatureType(), IPolyElement.QNAME ) )
      {
        // get the geometry
        final GM_Object selectedGeom = (GM_Object) feature.getProperty( IFE1D2DElement.PROP_GEOMETRY );
        if( selectedGeom instanceof GM_Surface )
        {
          final GM_Surface<GM_SurfacePatch> surface = (GM_Surface) selectedGeom;

          surfaceList.add( surface );
        }

        // get the selected Feature
        m_featuresToRefine.add( feature );
      }
    }

    /* REFINEMENT */
    final GM_MultiSurface multiSurface = GeometryFactory.createGM_MultiSurface( surfaceList.toArray( new GM_Surface[surfaceList.size()] ), crs );
    final GM_MultiSurface[] multiSurfaces = new GM_MultiSurface[] { multiSurface };

    final Refinement refinement = new Refinement();

    final GM_Object[] refinements = refinement.doRefine( multiSurfaces, m_geom );

    final List<GM_Surface> refinementList = new ArrayList<GM_Surface>();

    for( final GM_Object refineGeom : refinements )
    {
      if( refineGeom instanceof GM_Surface )
      {
        final GM_Surface<GM_SurfacePatch> surface = (GM_Surface) refineGeom;
        for( final GM_SurfacePatch surfacePatch : surface )
        {
          final int nodeCount = surfacePatch.getExteriorRing().length;
          final GM_Surface newSurface = GeometryFactory.createGM_Surface( surfacePatch );
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
    }

    if( refinementList.size() == 0 )
    {
      m_warning = true;
      m_warningRenderer.setTooltip( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.RefineFEGeometryWidget.5" ) ); //$NON-NLS-1$
    }

    // create new GM_Objects
    m_objects = refinementList.toArray( new GM_Surface[refinementList.size()] );

    m_geometryBuilder.reset();

    getMapPanel().repaintMap();
  }

  private static List<Feature> doSelect( final GM_Object selectGeometry, final FeatureList featureList )
  {
    if( selectGeometry == null )
      return null;
    // select feature from featureList by using the selectGeometry
    if( featureList == null )
      return null;

    final List<Feature> selectedFeatures = new ArrayList<Feature>();

    final List<Feature> selectedSubList = selectFeatures( featureList, selectGeometry );
    if( selectedSubList != null )
      selectedFeatures.addAll( selectedSubList );

    return selectedFeatures;
  }

  @SuppressWarnings({ "unchecked", "rawtypes" })
  private static List<Feature> selectFeatures( final FeatureList featureList, final GM_Object theGeom )
  {
    final List<Feature> selectedFeatures = new ArrayList<Feature>();
    GM_Object selectGeometry = theGeom;
    try
    {
      if( theGeom instanceof GM_Curve )
      {
        final GM_Position[] positions = ((GM_Curve) theGeom).getAsLineString().getPositions();
        selectGeometry = GeometryFactory.createGM_Surface( positions, null, selectGeometry.getCoordinateSystem() );
      }
    }
    catch( final GM_Exception e )
    {
    }
    final GM_Envelope envelope = selectGeometry.getEnvelope();
    final GMLWorkspace workspace = featureList.getParentFeature().getWorkspace();
    final List result = featureList.query( envelope, null );

    for( final Object object : result )
    {
      final Feature feature = FeatureHelper.getFeature( workspace, object );

      if( GMLSchemaUtilities.substitutes( feature.getFeatureType(), IPolyElement.QNAME ) )
      {
        final GM_Object geom = (GM_Object) feature.getProperty( IFE1D2DElement.PROP_GEOMETRY );
        if( geom != null )
        {
          final GM_Object intersection = selectGeometry.intersection( geom );
          if( intersection != null )
          {
            selectedFeatures.add( feature );
          }
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
    final IFE1D2DNode< ? > snapNode = m_pointSnapper == null ? null : m_pointSnapper.moved( currentPoint );
    final Object newNode = snapNode;

    return newNode;
  }
}
