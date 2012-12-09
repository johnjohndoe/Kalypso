package org.kalypso.kalypsomodel1d2d.ui.map.cline;

import java.awt.Cursor;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchPartSite;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.progress.IWorkbenchSiteProgressService;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
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

  private final ToolTipRenderer m_liveWarningRenderer = ToolTipRenderer.createErrorTooltip();

  private final ToolTipRenderer m_contilineWarningRenderer = ToolTipRenderer.createErrorTooltip();

  private final List<ContinuityEdge> m_continuityEdges = new ArrayList<>();

  private final SLDPainter2 m_previewPainter = new SLDPainter2( getClass().getResource( "continuityLinePreview.sld" ) ); //$NON-NLS-1$

  private final SLDPainter2 m_previewWarningPainter = new SLDPainter2( getClass().getResource( "continuityLinePreviewWarned.sld" ) ); //$NON-NLS-1$

  private IFEDiscretisationModel1d2d m_discModel = null;

  /* The current node of the disc-model under the cursor. */
  private ContinuityEdge m_currentEdge = null;

  private Point m_currentMapPoint = null;

  private PointSnapper m_pointSnapper = null;

//  private WeightedGraph<IFE1D2DNode, IFE1D2DEdge> m_discGraph;

  private IStatus m_contiLineStatus = Status.OK_STATUS;

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

//    /* initialize graph in separate thread */
//    final DiscretisationGraphOperation graphOp = new DiscretisationGraphOperation( m_discModel );
//
//    final IWorkbenchPartSite site = PlatformUI.getWorkbench().getWorkbenchWindows()[0].getActivePage().getActivePart().getSite();
//    final IWorkbenchSiteProgressService service = (IWorkbenchSiteProgressService)site.getService( IWorkbenchSiteProgressService.class );
//    final IStatus status = RunnableContextHelper.execute( service, true, false, graphOp );
//    if( !status.isOK() )
//      handleError( status );
//
//    m_discGraph = graphOp.getGraph();

    reinit();
  }

  private void reinit( )
  {
    if( m_currentEdge != null )
    {
      m_currentEdge.dispose();
      m_currentEdge = null;
    }

    m_contiLineStatus = Status.OK_STATUS;

    for( final ContinuityEdge edge : m_continuityEdges )
      edge.dispose();
    m_continuityEdges.clear();

    repaintMap();
  }

  private IFE1D2DNode[] getClickedNodes( )
  {
    final Collection<IFE1D2DNode> nodes = new ArrayList<>();

    for( final ContinuityEdge edge : m_continuityEdges )
    {
      // REMARK: start is null for first edge
      final IFE1D2DNode startNode = edge.getStartNode();
      if( startNode != null )
        nodes.add( startNode );

      nodes.add( edge.getEndNode() );
    }

    return nodes.toArray( new IFE1D2DNode[nodes.size()] );
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
      if( m_continuityEdges.size() > 1 )
      {
        final ContinuityEdge edge = m_continuityEdges.remove( m_continuityEdges.size() - 1 );
        edge.dispose();
        previewChanged();
      }
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
    if( m_currentEdge != null )
      m_currentEdge.dispose();
    m_currentEdge = null;
    m_liveWarningRenderer.setTooltip( null );

    if( snapNode != null )
    {
      if( isValidSnapNode( mapPanel, snapNode ) )
      {
        final IFE1D2DNode lastNode = m_continuityEdges.isEmpty() ? null : m_continuityEdges.get( m_continuityEdges.size() - 1 ).getEndNode();
        m_currentEdge = new ContinuityEdge( this, lastNode, snapNode );
      }
    }

    if( m_currentEdge == null )
      m_currentMapPoint = p;
    else
      m_currentMapPoint = MapUtilities.retransform( getMapPanel(), snapNode.getPoint() );

    if( snapNode == null )
      mapPanel.setCursor( Cursor.getPredefinedCursor( Cursor.CROSSHAIR_CURSOR ) );
    else
      mapPanel.setCursor( Cursor.getDefaultCursor() );

    repaintMap();
  }

  private boolean isValidSnapNode( final IMapPanel mapPanel, final IFE1D2DNode snapNode )
  {
    final ContinuityLineEditValidator validator = new ContinuityLineEditValidator( m_discModel, mapPanel, getClickedNodes(), snapNode );
    final String warning = validator.execute();
    m_liveWarningRenderer.setTooltip( warning );

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
    if( m_currentEdge == null )
      return;

    if( !m_contiLineStatus.isOK() )
      return;

    final IFE1D2DNode currentNode = m_currentEdge.getEndNode();
    if( ContinuityLineEditValidator.is2dNode( currentNode ) )
    {
      final IFE1D2DNode lastNode = m_continuityEdges.isEmpty() ? null : m_continuityEdges.get( m_continuityEdges.size() - 1 ).getEndNode();

      /* special handling for last node: is not warned, but should still not be added (allows double click on last point to finish) */
      if( currentNode != lastNode )
        m_continuityEdges.add( m_currentEdge );
    }
    else
    {
      // REMARK: no check needed, illegal situations already prevented by validation
      createBoundaryLine1D( currentNode );
    }
  }

  private void doubleClickedLeft( )
  {
    // REMARK: if double click on last point, ignore problem
    if( m_currentEdge == null )
      return;

    if( !m_contiLineStatus.isOK() )
      return;

    createBoundaryLine2D();
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

  private void createBoundaryLine2D( )
  {
    final Runnable op = new Runnable()
    {
      @Override
      public void run( )
      {
        doCreateBoundaryLine2D();
      }
    };

    final Shell shell = PlatformUI.getWorkbench().getWorkbenchWindows()[0].getActivePage().getActivePart().getSite().getShell();
    shell.getDisplay().asyncExec( op );
  }

  protected void doCreateBoundaryLine2D( )
  {
    try
    {
      final ContinuityEdge[] edges = m_continuityEdges.toArray( new ContinuityEdge[m_continuityEdges.size()] );

      final ContinuityLineBuilderOperation operation = new ContinuityLineBuilderOperation( m_discModel, getMapPanel(), edges );

      final IWorkbenchPartSite site = PlatformUI.getWorkbench().getWorkbenchWindows()[0].getActivePage().getActivePart().getSite();
      final IWorkbenchSiteProgressService service = (IWorkbenchSiteProgressService)site.getService( IWorkbenchSiteProgressService.class );

      // REMARK: needs to be called in swt thread
      final IStatus status = RunnableContextHelper.execute( service, true, false, operation );
      if( !status.isOK() )
      {
        handleError( status );
        return;
      }

      reinit();

      final IFE1D2DNode[] nodes = operation.getContinuityLine();

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
    for( final ContinuityEdge edge : m_continuityEdges )
      paintEdge( g, projection, edge );
    paintEdge( g, projection, m_currentEdge );

    /* Paint handle of mouse position */
    final int[][] posPoints = UtilMap.getPointArrays( m_currentMapPoint );

    final int[] arrayX = posPoints[0];
    final int[] arrayY = posPoints[1];

    UtilMap.drawHandles( g, arrayX, arrayY );

    final IFE1D2DNode[] nodes = getClickedNodes();
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

    final Point warningPosition = new Point( m_currentMapPoint.x + 10, m_currentMapPoint.y - 10 );
    if( !m_contiLineStatus.isOK() )
    {
      /* problems with conti line preceed over other warnings */
      m_contilineWarningRenderer.setTooltip( "Resulting continuity line is invalid" );
      m_contilineWarningRenderer.paintToolTip( warningPosition, g, bounds );
    }
    else if( m_currentMapPoint != null )
    {
      m_liveWarningRenderer.paintToolTip( warningPosition, g, bounds );
    }
  }

  private void paintEdge( final Graphics g, final GeoTransform projection, final ContinuityEdge edge )
  {
    if( edge == null )
      return;

    final GM_Curve curve = edge.getPathAsGeometry();
    if( curve == null )
      return;

    if( m_contiLineStatus.isOK() )
      m_previewPainter.paint( g, projection, curve );
    else
      m_previewWarningPainter.paint( g, projection, curve );
  }

  static LineGeometryBuilder createLineBuilder( final IMapPanel mapPanel, final IFE1D2DNode[] nodes, final GM_Point currentPoint ) throws Exception
  {
    final IKalypsoLayerModell modell = mapPanel.getMapModell();

    final LineGeometryBuilder geometryBuilder = new LineGeometryBuilder( 0, modell.getCoordinatesSystem() );
    for( final IFE1D2DNode node : nodes )
      geometryBuilder.addPoint( node.getPoint() );

    if( currentPoint != null )
      geometryBuilder.addPoint( currentPoint );

    return geometryBuilder;
  }

  void previewChanged( )
  {
    /* revalidate existing path */
    final ContinuityLineBuilder builder = new ContinuityLineBuilder();

    for( final ContinuityEdge edge : m_continuityEdges )
    {
      final IFE1D2DNode[] path = edge.getPath();
      builder.addPath( path );
    }

    if( m_currentEdge != null )
      builder.addPath( m_currentEdge.getPath() );

    m_contiLineStatus = builder.validate( m_discModel, getMapPanel() );

    repaintMap();
  }
}