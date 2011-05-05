/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
import org.eclipse.jface.window.DefaultToolTip;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gml.processes.constDelaunay.ConstraintDelaunayHelper;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.FE1D2DDiscretisationModel;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.FE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.util.PointSnapper;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.utilities.tooltip.ToolTipRenderer;
import org.kalypso.ogc.gml.map.widgets.builders.PolygonGeometryBuilder;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.widgets.AbstractWidget;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;
import org.kalypsodeegree.graphics.displayelements.DisplayElement;
import org.kalypsodeegree.graphics.sld.LineSymbolizer;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree.model.geometry.GM_Triangle;
import org.kalypsodeegree_impl.graphics.displayelements.DisplayElementFactory;
import org.kalypsodeegree_impl.graphics.sld.LineSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.Stroke_Impl;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * This widget is used to triangulate a boundary with breaklines. The user gets a preview before he starts the
 * refinement of the model.
 * 
 * @author Stefan Kurzbach
 */
public class TriangulateGeometryWidget extends AbstractWidget implements IWidgetWithOptions
{
  private static final double SNAP_DISTANCE = 0.02;

  private boolean m_modePolygon = true;

  private Point m_currentMapPoint;

  private PointSnapper m_pointSnapper;

  private PolygonGeometryBuilder m_geometryBuilder = null;

  private IKalypsoFeatureTheme m_theme;

  private IFEDiscretisationModel1d2d m_model1d2d;

  private final ToolTipRenderer m_toolTipRenderer = new ToolTipRenderer();

  private final ToolTipRenderer m_warningRenderer = new ToolTipRenderer();

  private boolean m_warning;

  private Composite m_composite;

  protected double m_maxArea = -1;

  protected double m_minAngle = 22;

  protected boolean m_noSteiner = true;

  private GM_Surface<GM_SurfacePatch> m_boundaryGeom;

  private FeatureList m_featureList;

  private GM_Surface<GM_SurfacePatch>[] m_objects;

  @SuppressWarnings("rawtypes")
  private Map<GM_Position, IFE1D2DNode> m_nodesNameConversionMap = new HashMap<GM_Position, IFE1D2DNode>();

  public TriangulateGeometryWidget( )
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

    final String modeTooltip = "\n    '<Space>': Change mode " + (m_modePolygon ? "(Polygon)" : "(Line)");
    m_toolTipRenderer.setBackgroundColor( new Color( 1f, 1f, 0.6f, 0.70f ) );
    m_toolTipRenderer.setTooltip( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.RefineFEGeometryWidget.2" ) + modeTooltip ); //$NON-NLS-1$

    m_warningRenderer.setBackgroundColor( new Color( 1f, 0.4f, 0.4f, 0.80f ) );

    if( m_modePolygon )
      m_geometryBuilder = new PolygonGeometryBuilder( 0, mapModell.getCoordinatesSystem() );
    // else
    // m_geometryBuilder = new LineGeometryBuilder( 0, mapModell.getCoordinatesSystem() );
    m_pointSnapper = new PointSnapper( m_model1d2d, mapPanel );

    final IKalypsoTheme activeTheme = mapModell.getActiveTheme();
    if( activeTheme instanceof IKalypsoFeatureTheme )
    {
      m_theme = (IKalypsoFeatureTheme) activeTheme;
      m_featureList = m_theme == null ? null : m_theme.getFeatureList();
    }

    m_nodesNameConversionMap.clear();
    m_objects = null;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftClicked(java.awt.Point)
   */
  @SuppressWarnings({ "rawtypes", "unchecked" })
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

      final GM_Point thePoint;
      if( newNode instanceof IFE1D2DNode )
      {
        thePoint = ((IFE1D2DNode) newNode).getPoint();
        m_currentMapPoint = MapUtilities.retransform( getMapPanel(), thePoint );
      }
      else
      {
        m_currentMapPoint = p;
        thePoint = MapUtilities.transform( mapPanel, m_currentMapPoint );
      }

      m_geometryBuilder.addPoint( thePoint );
      final GM_Surface<GM_SurfacePatch> finish = (GM_Surface<GM_SurfacePatch>) m_geometryBuilder.finish();
      if( finish != null )
      {
        if( GeometryUtilities.isSelfIntersecting( finish.get( 0 ).getExteriorRing() ) )
        {
          m_warning = true;
          m_warningRenderer.setTooltip( "Boundary is self-intersecting." );
          return;
        }

        final List<Feature> possiblyIntersecting = m_featureList.query( finish.getEnvelope(), null );
        for( final Feature feature : possiblyIntersecting )
        {
          final GM_Object geom = feature.getDefaultGeometryPropertyValue();
          if( geom.intersects( finish ) )
          {
            final GM_Object intersection = geom.intersection( finish );
            if( intersection instanceof GM_Surface )
            {
              m_warning = true;
              m_warningRenderer.setTooltip( "Boundary intersects existing elements." );
              return;
            }
          }
        }
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

  @SuppressWarnings({ "rawtypes" })
  private void drawRefinement( final Graphics g, final GeoTransform projection ) throws CoreException, GM_Exception
  {
    /* Paint a rect. */
    final LineSymbolizer symb = new LineSymbolizer_Impl();
    final Stroke stroke = new Stroke_Impl( new HashMap(), null, null );
    final Color color = new Color( 255, 0, 0 );
    for( final GM_Surface<GM_SurfacePatch> surface : m_objects )
    {
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
    {
      m_geometryBuilder.removeLastPoint();
      m_warning = false;
    }
    else if( e.getKeyCode() == KeyEvent.VK_ENTER )
      convertRefinementToModel();
    else
      super.keyPressed( e );
  }

  @SuppressWarnings({ "unchecked", "rawtypes", "deprecation" })
  private void convertRefinementToModel( )
  {
    if( m_objects == null )
      return;

    try
    {
      final CommandableWorkspace workspace = m_theme.getWorkspace();

      /* Initialize elements needed for edges and elements */
      final IFEDiscretisationModel1d2d discModel = new FE1D2DDiscretisationModel( workspace.getRootFeature() );

      List<Feature> lListAdded = new ArrayList<Feature>();
      /* create new elements */
      for( final GM_Surface surface : m_objects )
      {
        lListAdded.addAll( createPolyElement( surface, discModel ) );
      }

      if( lListAdded.size() > 0 )
      {
        FeatureStructureChangeModellEvent changeEvent = new FeatureStructureChangeModellEvent( workspace, discModel.getFeature(), lListAdded.toArray( new Feature[lListAdded.size()] ), FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD );
        workspace.fireModellEvent( changeEvent );
        Logger.getLogger( TriangulateGeometryWidget.class.getName() ).log( Level.INFO, "Model event fired: " + changeEvent ); //$NON-NLS-1$
      }
      reinit();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

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
      lListRes.add( element2d.getFeature() );
      for( final IFE1D2DEdge lEdge : lListEdges )
      {
        // add edge to element and element to edge
        final String elementId = element2d.getGmlID();
        element2d.addEdge( lEdge.getGmlID() );
        lEdge.addContainer( elementId );
      }
    }

    return lListRes;
  }

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
        lListRes.add( actNode.getFeature() );
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
          lListRes.add( edge.getFeature() );
        }
        lListEdges.add( edge );
        // final String gmlID = edge.getGmlID();
      }
      iCountNodes++;
      lastNode = actNode;
    }
    return lListRes;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#doubleClickedLeft(java.awt.Point)
   */
  @SuppressWarnings("unchecked")
  @Override
  public void doubleClickedLeft( final Point p )
  {
    if( m_geometryBuilder == null )
      return;

    try
    {
      m_boundaryGeom = (GM_Surface<GM_SurfacePatch>) m_geometryBuilder.finish();
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
    if( m_boundaryGeom == null )
      return;

    if( m_warning )
      return;

    if( m_featureList == null )
      return;

    final List<GM_Surface> refinementList = new ArrayList<GM_Surface>( 1000 );

    final List<String> args = new ArrayList<String>();
    if( m_maxArea > 0 )
    {
      args.add( "-a" + m_maxArea );
    }
    if( m_minAngle > 0 )
    {
      args.add( "-q" + m_minAngle );
    }
    if( m_noSteiner )
    {
      args.add( "-Y" );
    }
    final GM_Triangle[] triangles = ConstraintDelaunayHelper.convertToTriangles( m_boundaryGeom, m_boundaryGeom.getCoordinateSystem(), args.toArray( new String[args.size()] ) );

    for( final GM_Triangle triangle : triangles )
      refinementList.add( GeometryFactory.createGM_Surface( triangle ) );

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

  @SuppressWarnings({ "rawtypes" })
  private Object checkNewNode( final Point p )
  {
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return null;

    final GM_Point currentPoint = MapUtilities.transform( mapPanel, p );
    final IFE1D2DNode snapNode = m_pointSnapper == null ? null : m_pointSnapper.moved( currentPoint );
    final Object newNode = snapNode;

    return newNode;
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#createControl(org.eclipse.swt.widgets.Composite,
   *      org.eclipse.ui.forms.widgets.FormToolkit)
   */
  @Override
  public Control createControl( final Composite parent, final FormToolkit toolkit )
  {
    m_composite = toolkit.createComposite( parent, SWT.FILL );
    m_composite.setLayout( new FillLayout() );

    final GridLayout gridLayout = new GridLayout();
    gridLayout.numColumns = 2;

    final Section section = toolkit.createSection( m_composite, Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
    final Composite sectionComposite = toolkit.createComposite( section, SWT.NONE );
    section.setClient( sectionComposite );
    section.setText( "Triangulierungsoptionen" );
    section.setDescription( "Diese Optionen haben nur einen Effekt, wenn triangle.exe im \"bin\"-Verzeichnis von Kalypso liegt." );
    section.setExpanded( true );
    sectionComposite.setLayout( gridLayout );

    final FocusListener focusListener = new FocusListener()
    {
      @Override
      public void focusLost( FocusEvent arg0 )
      {
        try
        {
          finishGeometry();
        }
        catch( final GM_Exception e1 )
        {
          e1.printStackTrace();
        }
      }

      @Override
      public void focusGained( FocusEvent arg0 )
      {
      }
    };

    toolkit.createLabel( sectionComposite, "Maximale Fläche" );
    final Text maxArea = toolkit.createText( sectionComposite, "", SWT.SINGLE | SWT.BORDER ); //$NON-NLS-1$
    final DefaultToolTip toolTip = new DefaultToolTip( maxArea );
    maxArea.setLayoutData( new GridData( SWT.FILL, SWT.BEGINNING, true, false ) );
    maxArea.addModifyListener( new ModifyListener()
    {
      @Override
      public void modifyText( final ModifyEvent e )
      {
        toolTip.hide();

        final String string = maxArea.getText();
        if( string.isEmpty() )
        {
          m_maxArea = -1;
          return;
        }

        String message = null;
        try
        {
          m_maxArea = Double.parseDouble( string );
          if( m_maxArea < 0 )
          {
            message = "Fläche muss positiv sein";
          }
        }
        catch( Exception ex )
        {
          m_maxArea = -1;
          message = "Eingabe ist keine Zahl";
        }
        if( message != null )
        {
          toolTip.setText( message );
          toolTip.show( maxArea.getCaretLocation() );
        }
      }
    } );
    maxArea.addFocusListener( focusListener );

    toolkit.createLabel( sectionComposite, "Minimaler Winkel" );
    final Text minAngle = toolkit.createText( sectionComposite, "22", SWT.SINGLE | SWT.BORDER ); //$NON-NLS-1$
    final DefaultToolTip toolTip2 = new DefaultToolTip( minAngle );
    minAngle.setLayoutData( new GridData( SWT.FILL, SWT.BEGINNING, true, false ) );
    minAngle.addModifyListener( new ModifyListener()
    {
      @Override
      public void modifyText( final ModifyEvent e )
      {
        toolTip2.hide();

        final String string = minAngle.getText();
        if( string.isEmpty() )
        {
          m_minAngle = -1;
          return;
        }

        String message = null;
        try
        {
          m_minAngle = Double.parseDouble( string );
          if( m_minAngle < 0 )
          {
            message = "Winkel muss positiv sein";
          }
          else if( m_minAngle > 32 )
          {
            message = "Winkel sollte höchstens 32° betragen";
          }
        }
        catch( Exception ex )
        {
          m_minAngle = -1;
          message = "Eingabe ist keine Zahl";
        }
        if( message != null )
        {
          toolTip.setText( message );
          toolTip.show( minAngle.getCaretLocation() );
        }
      }
    } );
    minAngle.addFocusListener( focusListener );

    final Button noSteinerButton = toolkit.createButton( sectionComposite, "Einfügen von Knoten auf dem Rand verbieten", SWT.CHECK );
    noSteinerButton.setSelection( true );
    noSteinerButton.setLayoutData( new GridData( SWT.FILL, SWT.BEGINNING, true, false, 2, 1 ) );
    noSteinerButton.addSelectionListener( new SelectionListener()
    {
      @Override
      public void widgetSelected( SelectionEvent arg0 )
      {
        widgetDefaultSelected( arg0 );
      }

      @Override
      public void widgetDefaultSelected( SelectionEvent arg0 )
      {
        m_noSteiner = noSteinerButton.getSelection();
        try
        {
          finishGeometry();
        }
        catch( final GM_Exception e1 )
        {
          e1.printStackTrace();
        }
      }
    } );

    return m_composite;
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#disposeControl()
   */
  @Override
  public void disposeControl( )
  {
    if( m_composite != null && !m_composite.isDisposed() )
    {
      m_composite.dispose();
    }
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#getPartName()
   */
  @Override
  public String getPartName( )
  {
    return "Refine Elements";
  }

}
