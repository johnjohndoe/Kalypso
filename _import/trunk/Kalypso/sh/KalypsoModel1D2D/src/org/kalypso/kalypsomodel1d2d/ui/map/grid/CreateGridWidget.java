package org.kalypso.kalypsomodel1d2d.ui.map.grid;

import java.awt.Cursor;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.KeyEvent;

import org.apache.commons.lang.ObjectUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsomodel1d2d.ui.map.util.PointSnapper;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.widgets.AbstractWidget;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.widgets.IWidget;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * Provides the mechanism to create automaticaly fem element within a grid
 * 
 * @author Patrice Congo
 */
public class CreateGridWidget extends AbstractWidget implements IWidgetWithOptions
{
  private Point m_currentPoint = null;

  // private LineGeometryBuilder m_builder;//s[3] = new Linenull;
  GridPointCollector gridPointCollector = new GridPointCollector();

  private boolean isActivated = false;

  private final int m_radius = 20;

  private final IWidget m_delegateWidget = null;

  private PointSnapper m_pointSnapper;

  private IMapModell m_mapModell;

  private IKalypsoFeatureTheme m_nodeTheme;

  private IKalypsoFeatureTheme m_polyElementTheme;

  private IKalypsoFeatureTheme m_edgeTheme;

  private IKalypsoFeatureTheme m_continuityLineTheme;

  private IKalypsoFeatureTheme m_flowRelationsTheme;

  private IFEDiscretisationModel1d2d m_discModel;

  @SuppressWarnings("unchecked")
  private IFE1D2DNode m_snapNode;

  public CreateGridWidget( )
  {
    super( Messages.getString( "CreateGridWidget.0" ), Messages.getString( "CreateGridWidget.1" ) ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public void activate( final ICommandTarget commandPoster, final MapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );

    // find the right themes to edit i.e. the discretisation model
    if( isActivated == false )
    {
      reinit();
      isActivated = true;
    }

    m_mapModell = mapPanel.getMapModell();
    m_nodeTheme = UtilMap.findEditableTheme( m_mapModell, Kalypso1D2DSchemaConstants.WB1D2D_F_NODE );
    gridPointCollector.setNodeTheme( m_nodeTheme );
    m_polyElementTheme = UtilMap.findEditableTheme( m_mapModell, IPolyElement.QNAME );
    m_edgeTheme = UtilMap.findEditableTheme( m_mapModell, IFE1D2DEdge.QNAME );
    m_continuityLineTheme = UtilMap.findEditableTheme( m_mapModell, IFELine.QNAME );
    m_flowRelationsTheme = UtilMap.findEditableTheme( m_mapModell, IBoundaryCondition.QNAME );

    m_discModel = UtilMap.findFEModelTheme( m_mapModell );
    m_pointSnapper = new PointSnapper( m_discModel, mapPanel );

  }

  private final void reinit( )
  {
    CS_CoordinateSystem targetCrs = getMapPanel().getMapModell().getCoordinatesSystem();
    if( targetCrs == null )
      targetCrs = KalypsoCorePlugin.getDefault().getCoordinatesSystem();

    gridPointCollector.reset( targetCrs );
  }

  @SuppressWarnings("unchecked")
  private Object checkNewNode( final Point p )
  {
    final MapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return null;

    final GM_Point currentPoint = MapUtilities.transform( mapPanel, p );

    if( m_snappingActive )
      m_snapNode = m_pointSnapper == null ? null : m_pointSnapper.moved( currentPoint );

    final Object newNode = m_snapNode == null ? currentPoint : m_snapNode;

    return newNode;
  }

  @SuppressWarnings("unchecked")
  @Override
  public void moved( final Point p )
  {
    Object newNode = checkNewNode( p );

    if( newNode instanceof IFE1D2DNode )
      m_currentPoint = MapUtilities.retransform( getMapPanel(), ((IFE1D2DNode) newNode).getPoint() );
    else
      m_currentPoint = p;

    if( gridPointCollector.getHasAllSides() )
    {
      // gridPointCollector.selectPoint( lowerCorner, upperCorner );
      final int centerX = (int) p.getX();
      final int centerY = (int) p.getY();

      final int halfWidth = m_radius / 2;
      final Point lower = new Point( centerX - halfWidth, centerY - halfWidth );

      final MapPanel mapPanel = getMapPanel();
      final GM_Point gmLower = MapUtilities.transform( mapPanel, lower );
      final GM_Point selectedPoint = gridPointCollector.getSelectedPoint();
      gridPointCollector.selectPoint( gmLower, MapUtilities.calculateWorldDistance( mapPanel, gmLower, m_radius * 2 ) );
      final GM_Point newSelectedPoint = gridPointCollector.getSelectedPoint();
      if( !ObjectUtils.equals( selectedPoint, newSelectedPoint ) )
      {
        // TODO: check if this repaint is necessary for the widget
        final MapPanel panel = getMapPanel();
        if( panel != null )
          panel.repaint();
      }
    }
    // TODO: check if this repaint is really necessary
    final MapPanel panel = getMapPanel();
    if( panel != null )
    {
      panel.repaint();

      checkGrid( panel );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftClicked(java.awt.Point)
   */
  @SuppressWarnings("unchecked")
  @Override
  public void leftClicked( final Point p )
  {
    final Object newNode = checkNewNode( p );
    if( newNode instanceof IFE1D2DNode )
      m_currentPoint = MapUtilities.retransform( getMapPanel(), ((IFE1D2DNode) newNode).getPoint() );
    else
      m_currentPoint = p;

    if( newNode == null )
      getMapPanel().setCursor( Cursor.getPredefinedCursor( Cursor.CROSSHAIR_CURSOR ) );
    else
      getMapPanel().setCursor( Cursor.getDefaultCursor() );

    if( p == null )
      return;

    // TODO: check if this repaint is really necessary
    final MapPanel panel = getMapPanel();
    if( panel != null )
    {
      panel.repaint();

      checkGrid( panel );
    }

    try
    {
      final GM_Point currentPos = MapUtilities.transform( getMapPanel(), m_currentPoint );

      gridPointCollector.addPoint( currentPos );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      KalypsoModel1D2DPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#doubleClickedLeft(java.awt.Point)
   */
  @Override
  public void doubleClickedLeft( final Point p )
  {
    gridPointCollector.finishSide();
  }

  private Point draggedPoint;

  private boolean m_snappingActive;

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#dragged(java.awt.Point)
   */
  @SuppressWarnings("unchecked")
  @Override
  public void dragged( final Point p )
  {
    Object newNode = checkNewNode( p );

    if( newNode instanceof IFE1D2DNode )
      m_currentPoint = MapUtilities.retransform( getMapPanel(), ((IFE1D2DNode) newNode).getPoint() );
    else
      m_currentPoint = p;

    try
    {
      if( gridPointCollector.getHasAllSides() )
      {
        if( isSamePoint( m_currentPoint, gridPointCollector.getSelectedPoint(), m_radius * 2, getMapPanel().getProjection() ) || true )// TODO
        {
          final GM_Point nextPoint = MapUtilities.transform( getMapPanel(), m_currentPoint );
          gridPointCollector.changeSelectedPoint( nextPoint );
          getMapPanel().repaint();
        }
      }
      else
      {
        draggedPoint = p;
        final GM_Point lastSaved = gridPointCollector.getLastPoint();
        if( !isSamePoint( p, lastSaved, m_radius, getMapPanel().getProjection() ) )
        {
          m_currentPoint = p;
          // TODO: check if this repaint is really necessary
          final MapPanel panel = getMapPanel();
          if( panel != null )
            panel.repaint();

          return;
        }
        final GM_Point currentPos = MapUtilities.transform( getMapPanel(), p );
        gridPointCollector.replaceLastPoint( currentPos );
        m_currentPoint = p;
      }

    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    // TODO: check if this repaint is really necessary
    final MapPanel panel = getMapPanel();
    if( panel != null )
    {
      panel.repaint();

      checkGrid( panel );
    }
  }

  private void checkGrid( final MapPanel panel )
  {
    final IStatus status = gridPointCollector.isValid();
    if( status.isOK() )
      panel.setMessage( "" );
    else
      panel.setMessage( status.getMessage() );
  }

  public static final boolean isSamePoint( final Point ref, final GM_Point toCompare, final int m_radius, final GeoTransform transform )
  {
    Assert.throwIAEOnNull( transform, Messages.getString( "CreateGridWidget.6" ) ); //$NON-NLS-1$
    if( ref == null || toCompare == null )
      return false;
    else
    {
      final int x = (int) transform.getDestX( toCompare.getX() );
      final int y = (int) transform.getDestY( toCompare.getY() );
      return (x > (ref.getX() - m_radius)) && (x > (ref.getY() - m_radius)) && (y < (ref.getX() + m_radius)) && (y < (ref.getY() + m_radius));
    }

  }

  public static final boolean isSamePoint( final Point ref, final Point toCompare, final int m_radius )
  {
    if( ref == null )
      return false;
    else
    {
      return (toCompare.getX() > (ref.getX() - m_radius)) && (toCompare.getY() > (ref.getY() - m_radius)) && (toCompare.getX() < (ref.getX() + m_radius))
          && (toCompare.getY() < (ref.getY() + m_radius));
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    final Point currentPoint = m_currentPoint;

    if( currentPoint != null )
    {
      gridPointCollector.paint( g, getMapPanel().getProjection(), currentPoint );

      // TODO check what it is doing
      LinePointCollectorConfig currentLPCConfig = gridPointCollector.getCurrentLPCConfig();
      if( currentLPCConfig == null )
        currentLPCConfig = gridPointCollector.getSideconfigsAsArray()[0];

      /* paint the snap */
      if( m_pointSnapper != null )
        m_pointSnapper.paint( g );
    }
  }

  public static final char ESC = 0X01B;

  /**
   * @param e
   * @see org.kalypso.ogc.gml.widgets.IWidget#keyPressed(java.awt.event.KeyEvent)
   */
  @Override
  public void keyPressed( final KeyEvent e )
  {

    final MapPanel mapPanel = getMapPanel();

    if( e.getKeyCode() == KeyEvent.VK_SHIFT )
      m_snappingActive = true;

    if( m_delegateWidget != null )
      m_delegateWidget.keyPressed( e );

    mapPanel.repaint();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#keyTyped(java.awt.event.KeyEvent)
   */
  @Override
  public void keyTyped( final KeyEvent e )
  {
    final char typed = e.getKeyChar();
    final MapPanel mapPanel = getMapPanel();

    if( e.isActionKey() )
    {
      System.out.println( "e:" + e ); //$NON-NLS-1$
    }
    if( typed == ESC )
    {
      if( e.isShiftDown() )
      {
        reinit();
        mapPanel.repaint();
      }
      else
      {
        gridPointCollector.clearCurrent();
        mapPanel.repaint();
      }

    }
    else if( typed == '\b' )
    {

      if( e.isShiftDown() )// e.getModifiers()==InputEvent.SHIFT_MASK)
      {
        gridPointCollector.gotoPreviousSide();
        mapPanel.repaint();
      }
      else
      {
        gridPointCollector.removeLastPoint();
        mapPanel.repaint();
      }
    }
    else if( typed == '\t' )
    {
      System.out.println( "Selected" ); //$NON-NLS-1$
    }
    else if( typed == 't' )
    {
      convertToModell();
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#rightClicked(java.awt.Point)
   */
  @Override
  public void rightClicked( final Point p )
  {

  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#middleClicked(java.awt.Point)
   */
  @Override
  public void middleClicked( final Point p )
  {
    gridPointCollector.selectNext();
    final MapPanel mapPanel = getMapPanel();
    mapPanel.repaint();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#finish()
   */
  @Override
  public void finish( )
  {
    System.out.println( "FINISH" ); //$NON-NLS-1$
    super.finish();
    // isActivated=false;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#setSelection(org.eclipse.jface.viewers.ISelection)
   */
  @Override
  public void setSelection( final ISelection selection )
  {
    System.out.println( "Sel=" + selection ); //$NON-NLS-1$
  }

  private final GridWidgetFace gridWidgetFace = new GridWidgetFace( this );

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#createControl(org.eclipse.swt.widgets.Composite)
   */
  public Control createControl( final Composite parent, final FormToolkit toolkit )
  {
    final Control control = gridWidgetFace.createControl( parent, toolkit, this );
    gridWidgetFace.setInput( gridPointCollector );
    return control;
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#disposeControl()
   */
  public void disposeControl( )
  {
    gridWidgetFace.disposeControl();
  }

  public void convertToModell( )
  {
    final MapPanel mapPanel = getMapPanel();
    final IMapModell mapModel = getMapPanel().getMapModell();
    final IFEDiscretisationModel1d2d model1d2d = UtilMap.findFEModelTheme( mapModel );
    final IKalypsoFeatureTheme theme = UtilMap.findEditableTheme( mapModel, Kalypso1D2DSchemaConstants.WB1D2D_F_NODE );

    final CommandableWorkspace workspace = theme.getWorkspace();
    IStatus status = gridPointCollector.getAddToModelCommand( mapPanel, model1d2d, workspace );
    // TODO: handle status
    reinit();
  }
}
