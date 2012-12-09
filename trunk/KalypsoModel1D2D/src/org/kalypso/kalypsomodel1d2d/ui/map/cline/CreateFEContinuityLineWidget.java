package org.kalypso.kalypsomodel1d2d.ui.map.cline;

import java.awt.Cursor;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.IJobChangeListener;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.progress.IProgressConstants2;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.jobs.MutexRule;
import org.kalypso.contribs.eclipse.swt.awt.SWT_AWT_Utilities;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.CreateContinuityLine1DCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.CreateContinuityLine2DCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.util.PointSnapper;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoLayerModell;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.utilities.tooltip.ToolTipRenderer;
import org.kalypso.ogc.gml.map.widgets.advanced.utils.SLDPainter2;
import org.kalypso.ogc.gml.map.widgets.builders.LineGeometryBuilder;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.widgets.AbstractWidget;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * @author Gernot Belger
 * @author Patrice Congo
 * @author Dejan Antanaskovic
 */
public class CreateFEContinuityLineWidget extends AbstractWidget
{
  private final ToolTipRenderer m_toolTipRenderer = ToolTipRenderer.createStandardTooltip();

  private final ToolTipRenderer m_warningRenderer = ToolTipRenderer.createErrorTooltip();

  private final List<IFE1D2DNode> m_nodeList = new ArrayList<>();

  private final ISchedulingRule m_previewJobRule = new MutexRule();

  private final IJobChangeListener m_previewJobListener = new JobChangeAdapter()
  {
    @Override
    public void done( final IJobChangeEvent event )
    {
      handlePreviewCalculated( event.getJob() );
    }
  };

  private final SLDPainter2 m_previewPainter = new SLDPainter2( getClass().getResource( "continuityLinePreview.sld" ) ); //$NON-NLS-1$

  private Job m_previewJob = null;

  private IFEDiscretisationModel1d2d m_discModel = null;

  /* The current node of the disc-model under the cursor. */
  private IFE1D2DNode m_currentNode = null;

  private Point m_currentMapPoint = null;

  private PointSnapper m_pointSnapper = null;

  private GM_Curve m_preview = null;

  public CreateFEContinuityLineWidget( )
  {
    super( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.cline.CreateFEContinuityLineWidget.0" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.cline.CreateFEContinuityLineWidget.1" ) ); //$NON-NLS-1$ //$NON-NLS-2$

    m_toolTipRenderer.setTooltip( "Edit new boundary / transition line.\r\n    '<Click>': Add node to line\r\n    '<Double-Click>': Finish line\r\n    '<ESC>': Cancel current edit\r\n    '<DEL>': Remove last point" );
  }

  @Override
  public void activate( final ICommandTarget commandPoster, final IMapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );

    if( mapPanel.getMapModell() == null )
      return;

    m_discModel = UtilMap.findFEModelTheme( mapPanel );
    m_pointSnapper = new PointSnapper( m_discModel, mapPanel );

    reinit();
  }

  private void reinit( )
  {
    if( m_previewJob != null )
      m_previewJob.cancel();

    m_currentNode = null;
    m_nodeList.clear();
    m_preview = null;

    repaintMap();
  }

  private IFE1D2DNode[] getNodes( )
  {
    return m_nodeList.toArray( new IFE1D2DNode[m_nodeList.size()] );
  }

  @Override
  public void keyTyped( final KeyEvent e )
  {
    final char keyChar = e.getKeyChar();

    if( KeyEvent.VK_ESCAPE == keyChar )
    {
      reinit();
    }
    else if( KeyEvent.VK_BACK_SPACE == keyChar || KeyEvent.VK_DELETE == keyChar )
    {
      if( m_nodeList.size() > 1 )
        m_nodeList.remove( m_nodeList.size() - 1 );
      else
        reinit();
    }

    repaintMap();
  }

  @Override
  public void mouseMoved( final MouseEvent event )
  {
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;

    final Point p = event.getPoint();

    /* try to snap to node */
    final GM_Point currentPoint = MapUtilities.transform( mapPanel, p );
    final IFE1D2DNode snapNode = m_pointSnapper == null ? null : m_pointSnapper.moved( currentPoint );

    /* reset */
    m_currentNode = null;
    m_warningRenderer.setTooltip( null );

    if( snapNode != null )
    {
      if( isValidSnapNode( mapPanel, snapNode ) )
      {
        m_currentNode = snapNode;
        recalculatePreview( getNodes(), snapNode );
      }
      else
        recalculatePreview( getNodes(), null );
    }

    if( m_currentNode == null )
      m_currentMapPoint = p;
    else
      m_currentMapPoint = MapUtilities.retransform( getMapPanel(), snapNode.getPoint() );

    if( snapNode == null )
      mapPanel.setCursor( Cursor.getPredefinedCursor( Cursor.CROSSHAIR_CURSOR ) );
    else
      mapPanel.setCursor( Cursor.getDefaultCursor() );

    repaintMap();
  }

  private void recalculatePreview( final IFE1D2DNode[] nodes, final IFE1D2DNode snapNode )
  {
    if( m_previewJob != null )
      m_previewJob.cancel();

    final IFE1D2DNode[] allNodes = snapNode == null ? nodes : ArrayUtils.add( nodes, snapNode );

    final Job job = new ContinuityLinePreviewJob( allNodes );

    job.setUser( false );
    job.setSystem( true );
    job.setProperty( IProgressConstants2.NO_IMMEDIATE_ERROR_PROMPT_PROPERTY, Boolean.TRUE );
    job.setRule( m_previewJobRule );

    job.addJobChangeListener( m_previewJobListener );

    m_previewJob = job;

    job.schedule();
  }

  private boolean isValidSnapNode( final IMapPanel mapPanel, final IFE1D2DNode snapNode )
  {
    final ContinuityLineEditValidator validator = new ContinuityLineEditValidator( m_discModel, mapPanel, getNodes(), snapNode );
    final String warning = validator.execute();
    m_warningRenderer.setTooltip( warning );

    return warning == null;
  }

  @Override
  public void mousePressed( final MouseEvent event )
  {
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;

    if( event.getButton() != MouseEvent.BUTTON1 )
      return;

    if( event.getClickCount() == 1 )
      clickedLeft();
  }

  @Override
  public void mouseClicked( final MouseEvent event )
  {
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null || m_pointSnapper == null )
      return;

    if( event.getButton() != MouseEvent.BUTTON1 )
      return;

    if( event.getClickCount() == 2 )
      doubleClickedLeft();

    repaintMap();
  }

  private void clickedLeft( )
  {
    if( m_currentNode == null )
      return;

    if( ContinuityLineEditValidator.is2dNode( m_currentNode ) )
    {
      final IFE1D2DNode lastNode = m_nodeList.isEmpty() ? null : m_nodeList.get( m_nodeList.size() - 1 );
      /* special handling for last node: is not warned, but should still not be added (allows double click on last point to finish) */
      if( m_currentMapPoint != lastNode )
        m_nodeList.add( m_currentNode );
    }
    else
    {
      // REMARK: no check needed, illegal situations already prevented by validation
      createBoundaryLine1D( m_currentNode );
    }
  }

  private void doubleClickedLeft( )
  {
    // REMARK: if double click on last point, ignore problem
    if( m_currentNode == null )
      return;

    createBoundaryLine2D( getNodes() );
  }

  private void createBoundaryLine1D( final IFE1D2DNode node )
  {
    try
    {
      final IKalypsoTheme theme = UtilMap.findEditableTheme( getMapPanel(), Kalypso1D2DSchemaConstants.WB1D2D_F_NODE );
      final CommandableWorkspace workspace = ((IKalypsoFeatureTheme)theme).getWorkspace();

      final CreateContinuityLine1DCommand command = new CreateContinuityLine1DCommand( m_discModel, node );
      workspace.postCommand( command );
    }
    catch( final CoreException e )
    {
      handleError( e.getStatus() );
    }
    catch( final Exception e )
    {
      handleError( e );
    }
    finally
    {
      reinit();
    }
  }

  private void createBoundaryLine2D( final IFE1D2DNode[] nodes )
  {
    try
    {
      // TODO: validation!
      // TODO: check if there is already a boundary line

      final IKalypsoTheme theme = UtilMap.findEditableTheme( getMapPanel(), Kalypso1D2DSchemaConstants.WB1D2D_F_NODE );
      final CommandableWorkspace workspace = ((IKalypsoFeatureTheme)theme).getWorkspace();

      final CreateContinuityLine2DCommand command = new CreateContinuityLine2DCommand( m_discModel, nodes );
      workspace.postCommand( command );
    }
    catch( final CoreException e )
    {
      handleError( e.getStatus() );
    }
    catch( final Exception e )
    {
      handleError( e );
    }
    finally
    {
      reinit();
    }
  }

  private void handleError( final Exception e )
  {
    final String message = Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.cline.CreateFEContinuityLineWidget.2" ); //$NON-NLS-1$
    final IStatus status = new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, message, e );

    handleError( status );
  }

  private void handleError( final IStatus status )
  {
    KalypsoModel1D2DPlugin.getDefault().getLog().log( status );

    final Shell shell = PlatformUI.getWorkbench().getWorkbenchWindows()[0].getActivePage().getActivePart().getSite().getShell();
    final StatusDialog statusDialog = new StatusDialog( shell, status, getName() );
    SWT_AWT_Utilities.openSwtWindow( statusDialog );
  }

  @Override
  public void paint( final Graphics g )
  {
    final IMapPanel mapPanel = getMapPanel();
    final GeoTransform projection = mapPanel.getProjection();

    final Rectangle bounds = mapPanel.getScreenBounds();
    m_toolTipRenderer.paintToolTip( new Point( 5, bounds.height - 5 ), g, bounds );

    /* always paint a small rectangle of current position */
    if( m_currentMapPoint == null )
      return;

    /* Paint preview if exists */
    m_previewPainter.paint( g, projection, m_preview );

    /* Paint handle of mouse position */
    final int[][] posPoints = UtilMap.getPointArrays( m_currentMapPoint );

    final int[] arrayX = posPoints[0];
    final int[] arrayY = posPoints[1];

    UtilMap.drawHandles( g, arrayX, arrayY );

    final IFE1D2DNode[] nodes = getNodes();
    if( nodes.length != 0 )
    {
      try
      {
        final GM_Point currentPoint = MapUtilities.transform( mapPanel, m_currentMapPoint );

        final LineGeometryBuilder geometryBuilder = createLineBuilder( mapPanel, nodes, currentPoint );

        geometryBuilder.paint( g, projection, null );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
    }

    /* paint the snap */
    if( m_pointSnapper != null )
      m_pointSnapper.paint( g );

    if( m_currentMapPoint != null )
    {
      final Point warningPosition = new Point( m_currentMapPoint.x + 10, m_currentMapPoint.y - 10 );
      m_warningRenderer.paintToolTip( warningPosition, g, bounds );
    }
  }

  static LineGeometryBuilder createLineBuilder( final IMapPanel mapPanel, final IFE1D2DNode[] nodes, final GM_Point currentPoint ) throws Exception
  {
    final IKalypsoLayerModell modell = mapPanel.getMapModell();

    final LineGeometryBuilder geometryBuilder = new LineGeometryBuilder( 0, modell.getCoordinatesSystem() );
    for( final IFE1D2DNode node : nodes )
      geometryBuilder.addPoint( node.getPoint() );

    geometryBuilder.addPoint( currentPoint );

    return geometryBuilder;
  }

  protected void handlePreviewCalculated( final Job job )
  {
    final ContinuityLinePreviewJob previewJob = (ContinuityLinePreviewJob)job;
    m_preview = previewJob.getContinuityLine();
    repaintMap();
  }
}