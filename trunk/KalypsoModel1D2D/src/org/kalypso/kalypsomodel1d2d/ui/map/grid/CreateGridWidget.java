package org.kalypso.kalypsomodel1d2d.ui.map.grid;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.quadmesh.ImportQuadMeshWorker;
import org.kalypso.kalypsomodel1d2d.ui.map.quadmesh.QuadMesh;
import org.kalypso.kalypsomodel1d2d.ui.map.util.PointSnapper;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.utilities.tooltip.ToolTipRenderer;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.widgets.AbstractWidget;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * TODO/FIXME: we should separate editing the four lines and editing the grid afterwards. Interaction between those two
 * is too buggy anyways. So we should split up into two widgets, one for entering the four lines; afterwards one for
 * editing the grid.
 * Provides the mechanism to create automatically fem element within a grid
 * 
 * @author Patrice Congo
 */
public class CreateGridWidget extends AbstractWidget implements IWidgetWithOptions
{
  private GridPointCollector m_gridPointCollector;

  private GridWidgetFace m_gridWidgetFace;

  private final ToolTipRenderer m_warningRenderer = new ToolTipRenderer();

  private final int m_radius = 10;

  private boolean isActivated = false;

  private PointSnapper m_pointSnapper;

  private GM_Point m_currentPoint;

  public CreateGridWidget( )
  {
    super( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.grid.CreateGridWidget.0" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.grid.CreateGridWidget.1" ) ); //$NON-NLS-1$ //$NON-NLS-2$

    m_warningRenderer.setBackgroundColor( new Color( 1f, 0.4f, 0.4f, 0.80f ) );
  }

  @Override
  public void activate( final ICommandTarget commandPoster, final IMapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );

    if( isActivated == false )
    {
      // find the right themes to edit i.e. the discretisation model
      final IFEDiscretisationModel1d2d discModel = UtilMap.findFEModelTheme( mapPanel );

      m_gridPointCollector = new GridPointCollector( discModel, KalypsoDeegreePlugin.getDefault().getCoordinateSystem() );
      m_gridWidgetFace = new GridWidgetFace( this, m_gridPointCollector );

      m_pointSnapper = new PointSnapper( discModel, mapPanel );

      reinit();
      isActivated = true;
    }
  }

  final void reinit( )
  {
    String targetCrs = getMapPanel().getMapModell().getCoordinatesSystem();
    if( targetCrs == null )
      targetCrs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

    if( m_gridPointCollector != null )
      m_gridPointCollector.reset( targetCrs );
  }

  private GM_Point snapToNode( final Point p, final boolean snappingActive )
  {
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null || m_pointSnapper == null )
      return null;

    final GM_Point currentPoint = MapUtilities.transform( mapPanel, p );

    m_pointSnapper.activate( snappingActive );
    final IFE1D2DNode snapNode = m_pointSnapper.moved( currentPoint );

    if( snapNode == null )
      return currentPoint;

    return snapNode.getPoint();
  }

  @Override
  public void mouseMoved( final MouseEvent event )
  {
    if( event.getButton() != 0 )
      return;

    final Point p = event.getPoint();
    final boolean snappingActive = !event.isShiftDown();

    /* update current location */
    final GM_Point currentPoint = snapToNode( p, snappingActive );

    m_currentPoint = currentPoint;

    // TODO: makes no sense
    if( m_gridPointCollector != null && m_gridPointCollector.getHasAllSides() )
    {
      final IMapPanel mapPanel = getMapPanel();
      final GM_Point point = MapUtilities.transform( mapPanel, p );
      final double screenRadius = MapUtilities.calculateWorldDistance( mapPanel, point, m_radius );
      m_gridPointCollector.selectPoint( point, screenRadius );
    }

    repaintMap();
  }

  @Override
  public void mouseClicked( final MouseEvent event )
  {
    final IMapPanel panel = getMapPanel();
    if( panel == null )
      return;

    if( (event.getButton() == MouseEvent.BUTTON1) )
    {
      if( m_gridPointCollector.getHasAllSides() )
      {
        /* nothing to do, grid is finished */
      }
      /* double click */
      else if( event.getClickCount() > 1 )
      {
        m_gridPointCollector.finishSide();
      }
      else
      {
        final boolean snappingActive = !event.isShiftDown();

        final Point point = event.getPoint();

        final GM_Point snapToNode = snapToNode( point, snappingActive );
        m_currentPoint = snapToNode;

        if( snapToNode == null )
          panel.setCursor( Cursor.getPredefinedCursor( Cursor.CROSSHAIR_CURSOR ) );
        else
          panel.setCursor( Cursor.getDefaultCursor() );

        m_gridPointCollector.addPoint( snapToNode );
      }
    }
    else if( (event.getButton() == MouseEvent.BUTTON3) )
    {
      m_gridPointCollector.selectNext();
    }
    else
      return;

    event.consume();

    panel.repaintMap();

    checkGrid();
  }

  @Override
  public void mouseDragged( final MouseEvent event )
  {
    if( (event.getModifiersEx() & MouseEvent.BUTTON1_DOWN_MASK) == 0 )
      return;

    final IMapPanel panel = getMapPanel();
    if( panel == null )
      return;

    event.consume();

    final boolean snappingActive = !event.isShiftDown();

    m_currentPoint = snapToNode( event.getPoint(), snappingActive );
    final Point currentPos = MapUtilities.retransform( panel, snapToNode( event.getPoint(), snappingActive ) );

    if( m_gridPointCollector.getHasAllSides() )
    {
      final GM_Point oldSelectedPoint = m_gridPointCollector.getSelectedPoint();
      if( !isSamePoint( currentPos, oldSelectedPoint, m_radius, getMapPanel().getProjection() ) )
      {
        m_gridPointCollector.changeSelectedPoint( snapToNode( event.getPoint(), snappingActive ) );
      }
    }
    else
    {
      final GM_Point lastSaved = m_gridPointCollector.getLastPoint();
      if( isSamePoint( currentPos, lastSaved, m_radius, getMapPanel().getProjection() ) )
      {
        final GM_Point currentPosi = MapUtilities.transform( getMapPanel(), event.getPoint() );
        m_gridPointCollector.replaceLastPoint( currentPosi );
      }
    }

    checkGrid();

    repaintMap();
  }

  private void checkGrid( )
  {
    final IStatus status = m_gridPointCollector.isValid();

    if( status.isOK() )
      m_warningRenderer.setTooltip( null );
    else
      m_warningRenderer.setTooltip( status.getMessage() );
  }

  private static final boolean isSamePoint( final Point ref, final GM_Point toCompare, final int radius, final GeoTransform transform )
  {
    Assert.throwIAEOnNull( transform, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.grid.CreateGridWidget.6" ) ); //$NON-NLS-1$

    if( ref == null || toCompare == null )
      return false;
    else
    {
      final int x = (int)transform.getDestX( toCompare.getX() );
      final int y = (int)transform.getDestY( toCompare.getY() );

      final double distX = Math.abs( x - ref.getX() );
      final double distY = Math.abs( y - ref.getY() );

      return distX < radius && distY < radius;
    }
  }

  @Override
  public void paint( final Graphics g )
  {
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;

    if( m_currentPoint != null )
    {
      m_gridPointCollector.paint( g, mapPanel.getProjection(), m_currentPoint );

      final Point currentPos = MapUtilities.retransform( mapPanel, m_currentPoint );

      final int[] arrayX = new int[] { currentPos.x };
      final int[] arrayY = new int[] { currentPos.y };
      UtilMap.drawHandles( g, arrayX, arrayY );
    }

    /* paint the snap */
    if( m_pointSnapper != null )
      m_pointSnapper.paint( g );

    final Rectangle bounds = mapPanel.getScreenBounds();
    m_warningRenderer.paintToolTip( new Point( 5, bounds.height - 5 ), g, bounds );
  }

  @Override
  public void keyTyped( final KeyEvent e )
  {
    final int typed = e.getKeyChar();

    switch( typed )
    {
      case KeyEvent.VK_ESCAPE:
        if( e.isShiftDown() )
          reinit();
        else
          m_gridPointCollector.clearCurrent();
        break;

      case KeyEvent.VK_BACK_SPACE:
        if( e.isShiftDown() )
          m_gridPointCollector.gotoPreviousSide();
        else
          m_gridPointCollector.removeLastPoint();
        break;

      case 't':
      case 'T':
      case KeyEvent.VK_ENTER:
        convertToModell();
        break;

      case 'n':
      case 'N':
      case KeyEvent.VK_SPACE:
        m_gridPointCollector.selectNext();
        break;

      default:
        break;
    }

    repaintMap();
  }

  @Override
  public Control createControl( final Composite parent, final FormToolkit toolkit )
  {
    return m_gridWidgetFace.createControl( parent, toolkit );
  }

  @Override
  public void disposeControl( )
  {
    m_gridWidgetFace.disposeControl();
  }

  void convertToModell( )
  {
    final IMapPanel mapPanel = getMapPanel();
    final IKalypsoFeatureTheme theme = UtilMap.findEditableTheme( mapPanel, Kalypso1D2DSchemaConstants.WB1D2D_F_NODE );

    final CommandableWorkspace workspace = theme.getWorkspace();

    final QuadMesh tempGrid = m_gridPointCollector.getTempGrid();
    if( tempGrid == null )
      return;

    final ICoreRunnableWithProgress operation = new ImportQuadMeshWorker( workspace, tempGrid );

    final Display display = PlatformUI.getWorkbench().getDisplay();
    final Runnable runnable = new Runnable()
    {
      @Override
      public void run( )
      {
        final IStatus status = ProgressUtilities.busyCursorWhile( operation, null );
        reinit();
        if( !status.isOK() )
        {
          StatusDialog.open( display.getActiveShell(), status, getName() );
        }
      }
    };
    display.asyncExec( runnable );
  }

  @Override
  public String getPartName( )
  {
    return null;
  }
}