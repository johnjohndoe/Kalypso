package org.kalypso.kalypsomodel1d2d.ui.map.grid;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.KeyEvent;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.util.PointSnapper;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.utilities.tooltip.ToolTipRenderer;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.widgets.AbstractWidget;
import org.kalypso.ogc.gml.widgets.IWidget;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * Provides the mechanism to create automaticaly fem element within a grid
 * 
 * @author Patrice Congo
 */
public class CreateGridWidget extends AbstractWidget implements IWidgetWithOptions
{
  private final GridWidgetFace m_gridWidgetFace = new GridWidgetFace( this );

  // private GridWidgetFace m_gridWidgetFace;

  private Point m_currentPoint = null;

  final GridPointCollector m_gridPointCollector = new GridPointCollector();

  // GridPointCollector m_gridPointCollector ;

  private boolean isActivated = false;

  private final int m_radius = 10;

  public static final char ESC = 0X01B;

  private final IWidget m_delegateWidget = null;

  private PointSnapper m_pointSnapper;

  private IKalypsoFeatureTheme m_nodeTheme;

  private IFEDiscretisationModel1d2d m_discModel;

  private boolean m_snappingActive = true;

  private IFE1D2DNode m_snapNode;

  private final ToolTipRenderer m_toolTipRenderer = new ToolTipRenderer();

  private final ToolTipRenderer m_warningRenderer = new ToolTipRenderer();

  private boolean m_warning;

  public CreateGridWidget( )
  {
    super( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.grid.CreateGridWidget.0" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.grid.CreateGridWidget.1" ) ); //$NON-NLS-1$ //$NON-NLS-2$
    // m_gridWidgetFace = new GridWidgetFace( this );
    // m_gridPointCollector = new GridPointCollector();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public void activate( final ICommandTarget commandPoster, final IMapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );

    m_toolTipRenderer.setBackgroundColor( new Color( 1f, 1f, 0.6f, 0.70f ) );
    m_warningRenderer.setBackgroundColor( new Color( 1f, 0.4f, 0.4f, 0.80f ) );

    // m_gridWidgetFace = new GridWidgetFace( this );
    // m_gridPointCollector = new GridPointCollector();
    // find the right themes to edit i.e. the discretisation model
    if( isActivated == false )
    {
      reinit();
      isActivated = true;
    }

    m_nodeTheme = UtilMap.findEditableTheme( mapPanel, Kalypso1D2DSchemaConstants.WB1D2D_F_NODE );
    m_gridPointCollector.setNodeTheme( m_nodeTheme );

    m_discModel = UtilMap.findFEModelTheme( mapPanel );
    m_pointSnapper = new PointSnapper( m_discModel, mapPanel );

  }

  private final void reinit( )
  {
    m_warning = false;

    String targetCrs = getMapPanel().getMapModell().getCoordinatesSystem();
    if( targetCrs == null )
      targetCrs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

    m_gridPointCollector.reset( targetCrs );
  }

  private Object checkNewNode( final Point p )
  {
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return null;

    final GM_Point currentPoint = MapUtilities.transform( mapPanel, p );

    if( m_snappingActive )
      m_snapNode = m_pointSnapper == null ? null : m_pointSnapper.moved( currentPoint );

    final Object newNode = m_snapNode == null ? currentPoint : m_snapNode;

    return newNode;
  }

  @Override
  public void moved( final Point p )
  {
    final Object newNode = checkNewNode( p );

    if( newNode instanceof IFE1D2DNode )
      m_currentPoint = MapUtilities.retransform( getMapPanel(), ((IFE1D2DNode) newNode).getPoint() );
    else
      m_currentPoint = p;

    if( m_gridPointCollector.getHasAllSides() )
    {
      final IMapPanel mapPanel = getMapPanel();
      final GM_Point point = MapUtilities.transform( mapPanel, p );
      m_gridPointCollector.selectPoint( point, MapUtilities.calculateWorldDistance( mapPanel, point, m_radius ) );
    }
    final IMapPanel panel = getMapPanel();
    if( panel != null )
      panel.repaintMap();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftClicked(java.awt.Point)
   */
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

    final IMapPanel panel = getMapPanel();
    if( panel != null )
    {
      panel.repaintMap();

      try
      {
        GM_Point currentPos;
        if( newNode instanceof IFE1D2DNode )
          currentPos = ((IFE1D2DNode) newNode).getPoint();
        else
          currentPos = MapUtilities.transform( panel, m_currentPoint );
        m_gridPointCollector.addPoint( currentPos );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
        KalypsoModel1D2DPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
      }
    }
    checkGrid();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#doubleClickedLeft(java.awt.Point)
   */
  @Override
  public void doubleClickedLeft( final Point p )
  {
    m_gridPointCollector.finishSide();

    checkGrid();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#dragged(java.awt.Point)
   */
  @Override
  public void dragged( final Point p )
  {
    final Object newNode = checkNewNode( p );

    if( newNode instanceof IFE1D2DNode )
      m_currentPoint = MapUtilities.retransform( getMapPanel(), ((IFE1D2DNode) newNode).getPoint() );
    else
      m_currentPoint = p;

    try
    {
      if( m_gridPointCollector.getHasAllSides() )
      {
        if( isSamePoint( m_currentPoint, m_gridPointCollector.getSelectedPoint(), m_radius * 2, getMapPanel().getProjection() ) || true )
        {
          GM_Point point;
          if( newNode instanceof IFE1D2DNode )
            point = ((IFE1D2DNode) newNode).getPoint();
          else
            point = MapUtilities.transform( getMapPanel(), m_currentPoint );

          m_gridPointCollector.changeSelectedPoint( point );
          getMapPanel().repaintMap();
        }
      }
      else
      {
        final GM_Point lastSaved = m_gridPointCollector.getLastPoint();
        if( !isSamePoint( p, lastSaved, m_radius, getMapPanel().getProjection() ) )
        {
          m_currentPoint = p;
          // TODO: check if this repaint is really necessary
          final IMapPanel panel = getMapPanel();
          if( panel != null )
            panel.repaintMap();

          return;
        }
        final GM_Point currentPos = MapUtilities.transform( getMapPanel(), p );
        m_gridPointCollector.replaceLastPoint( currentPos );
        m_currentPoint = p;
      }

      checkGrid();

    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    // TODO: check if this repaint is really necessary
    final IMapPanel panel = getMapPanel();
    if( panel != null )
    {
      panel.repaintMap();
      // checkGrid();
    }
  }

  private void checkGrid( )
  {
    final IStatus status = m_gridPointCollector.isValid();
    if( status.isOK() )
      m_warning = false;
    else
    {
      m_warning = true;
      m_warningRenderer.setTooltip( status.getMessage() );
    }

  }

  public static final boolean isSamePoint( final Point ref, final GM_Point toCompare, final int m_radius, final GeoTransform transform )
  {
    Assert.throwIAEOnNull( transform, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.grid.CreateGridWidget.6" ) ); //$NON-NLS-1$
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
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;

    final Point currentPoint = m_currentPoint;

    if( currentPoint != null )
    {
      m_gridPointCollector.paint( g, getMapPanel().getProjection(), currentPoint );

      // TODO check what it is doing
      LinePointCollectorConfig currentLPCConfig = m_gridPointCollector.getCurrentLPCConfig();
      if( currentLPCConfig == null )
        currentLPCConfig = m_gridPointCollector.getSideconfigsAsArray()[0];

      /* paint the snap */
      if( m_pointSnapper != null )
        m_pointSnapper.paint( g );
    }

    final Rectangle bounds = mapPanel.getScreenBounds();
    if( m_warning == true )
      m_warningRenderer.paintToolTip( new Point( 5, bounds.height - 5 ), g, bounds );

  }

  /**
   * @param e
   * @see org.kalypso.ogc.gml.widgets.IWidget#keyPressed(java.awt.event.KeyEvent)
   */
  @Override
  public void keyPressed( final KeyEvent e )
  {
    final IMapPanel mapPanel = getMapPanel();

    if( e.getKeyCode() == KeyEvent.VK_SHIFT )
      m_snappingActive = false;

    if( m_delegateWidget != null )
      m_delegateWidget.keyPressed( e );

    mapPanel.repaintMap();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#keyReleased(java.awt.event.KeyEvent)
   */
  @Override
  public void keyReleased( final KeyEvent e )
  {
    super.keyReleased( e );

    if( e.getKeyCode() == KeyEvent.VK_SHIFT )
      m_snappingActive = true;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#keyTyped(java.awt.event.KeyEvent)
   */
  @Override
  public void keyTyped( final KeyEvent e )
  {
    final char typed = e.getKeyChar();
    final IMapPanel mapPanel = getMapPanel();

    if( typed == ESC )
    {
      if( e.isShiftDown() )
      {
        reinit();
        mapPanel.repaintMap();
      }
      else
      {
        m_gridPointCollector.clearCurrent();
        mapPanel.repaintMap();
      }
    }
    else if( typed == '\b' )
    {
      if( e.isShiftDown() )
      {
        m_gridPointCollector.gotoPreviousSide();
        mapPanel.repaintMap();
      }
      else
      {
        m_gridPointCollector.removeLastPoint();
        mapPanel.repaintMap();
      }
    }
    else if( typed == 't' )
      convertToModell();
    else if( typed == 'n' )
      m_gridPointCollector.selectNext();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#middleClicked(java.awt.Point)
   */
  @Override
  public void rightClicked( final Point p )
  {
    m_gridPointCollector.selectNext();
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel != null )
      mapPanel.repaintMap();
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public Control createControl( final Composite parent, final FormToolkit toolkit )
  {
    final Control control = m_gridWidgetFace.createControl( parent, toolkit, this );
    m_gridWidgetFace.setInput( m_gridPointCollector );
    return control;
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#disposeControl()
   */
  @Override
  public void disposeControl( )
  {
    m_gridWidgetFace.disposeControl();
  }

  public void convertToModell( )
  {
    final IMapPanel mapPanel = getMapPanel();
    final IFEDiscretisationModel1d2d model1d2d = UtilMap.findFEModelTheme( mapPanel );
    final IKalypsoFeatureTheme theme = UtilMap.findEditableTheme( mapPanel, Kalypso1D2DSchemaConstants.WB1D2D_F_NODE );

    final CommandableWorkspace workspace = theme.getWorkspace();
    final ICoreRunnableWithProgress operation = new ICoreRunnableWithProgress()
    {
      @Override
      public IStatus execute( final IProgressMonitor monitor )
      {
        final IStatus status = m_gridPointCollector.getAddToModelCommand( mapPanel, model1d2d, workspace );
        return status;
      }
    };
    final IStatus status = ProgressUtilities.busyCursorWhile( operation, null );
    if( status.equals( Status.OK_STATUS ) )
    {
      try
      {
        workspace.postCommand( new EmptyCommand( "set dirty command ", false ) ); //$NON-NLS-1$
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
    }
    reinit();
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#getPartName()
   */
  @Override
  public String getPartName( )
  {
    return null;
  }
}