package org.kalypso.kalypsomodel1d2d.ui.map.cline;

import java.awt.Cursor;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.swt.awt.SWT_AWT_Utilities;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
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
import org.kalypso.ogc.gml.map.widgets.builders.LineGeometryBuilder;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.widgets.AbstractWidget;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.LineString;

/**
 * @author Gernot Belger
 * @author Patrice Congo
 * @author Dejan Antanaskovic
 */
public class CreateFEContinuityLineWidget extends AbstractWidget
{
  private final ToolTipRenderer m_toolTipRenderer = ToolTipRenderer.createStandardTooltip();

  private final ToolTipRenderer m_warningRenderer = ToolTipRenderer.createErrorTooltip();

  private IFEDiscretisationModel1d2d m_discModel = null;

  /* The current node of the disc-model under the cursor. */
  private IFE1D2DNode m_currentNode = null;

  private Point m_currentMapPoint;

  private PointSnapper m_pointSnapper;

  private final List<IFE1D2DNode> m_nodeList = new ArrayList<>();

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
    m_currentNode = null;
    m_nodeList.clear();
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

    if( snapNode != null && isValidSnapNode( mapPanel, snapNode ) )
    {
      if( m_nodeList.size() == 0 )
        m_currentNode = snapNode;
      else if( is2dNode( snapNode ) )
      {
        /* as soon as we have nodes, it is a 2d conti line, so we can only select 2d elements */
        m_currentNode = snapNode;
      }
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

  private boolean isValidSnapNode( final IMapPanel mapPanel, final IFE1D2DNode snapNode )
  {
    final String warning = validateSnapeNode( mapPanel, snapNode );
    m_warningRenderer.setTooltip( warning );

    return warning == null;
  }

  private String validateSnapeNode( final IMapPanel mapPanel, final IFE1D2DNode snapNode )
  {
    /* 1d-nodes: only on end of line */
    if( m_nodeList.size() == 0 && !is2dNode( snapNode ) )
    {
      final IFE1D2DElement[] elements = snapNode.getAdjacentElements();
      if( elements.length == 0 )
        return "Cannot add continuity line to single 1D node that is not attached to the net";

      if( elements.length > 1 )
        return "1D-continuity line must be at end of 1D-reach";
    }

    /* check for any conti line on node */
    final IFELine touchedLine = m_discModel.findContinuityLine( snapNode.getPoint(), IFEDiscretisationModel1d2d.CLUSTER_TOLERANCE );
    if( touchedLine != null )
      return "Node is already part of a continuity line";

    if( m_nodeList.size() > 0 )
    {
      if( !is2dNode( snapNode ) )
        return "2D-continuity line cannot touch 1D-node";

      /* duplicate point */
      for( final IFE1D2DNode node : m_nodeList )
      {
        if( node == snapNode )
          return "Node already contained in line";
      }

      try
      {
        /* build geometry */
        final LineGeometryBuilder lineBuilder = createLineBuilder( mapPanel );
        final GM_Curve curve = (GM_Curve)lineBuilder.finish();

        /* new node on line */
        if( snapNode.getPoint().distance( curve ) < IFEDiscretisationModel1d2d.CLUSTER_TOLERANCE )
          return "Line is self-intersecting";

        /* self intersection */
        final LineString line = (LineString)JTSAdapter.export( curve );
        if( !line.isSimple() )
          return "Line is self-intersecting";

        /* intersection with other conti lines */
        // REMARK: we assume that we have not too many conti lines and just do a linear search here
        final IFELine[] contiLines = m_discModel.getContinuityLines();
        for( final IFELine contiLine : contiLines )
        {
          final GM_Curve geometry = contiLine.getGeometry();
          if( curve.intersects( geometry ) )
            return "Line intersect an existing continuity line";
        }
      }
      catch( final Exception e )
      {
        return e.getLocalizedMessage();
      }
    }

    return null;
  }

  private boolean is2dNode( final IFE1D2DNode node )
  {
    final IFE1D2DElement[] elements = node.getAdjacentElements();
    for( final IFE1D2DElement element : elements )
    {
      if( !(element instanceof IPolyElement) )
        return false;
    }

    return true;
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
    if( mapPanel == null )
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

    if( is2dNode( m_currentNode ) )
      m_nodeList.add( m_currentNode );
    else
    {
      // REMARK: no check needed, illegal situations already prevented by validation
      createBoundaryLine1D( m_currentNode );
    }
  }

  private void doubleClickedLeft( )
  {
    if( m_currentNode == null )
      return;

    createBoundaryLine2D( m_nodeList.toArray( new IFE1D2DNode[m_nodeList.size()] ) );
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

    final Rectangle bounds = mapPanel.getScreenBounds();
    m_toolTipRenderer.paintToolTip( new Point( 5, bounds.height - 5 ), g, bounds );

    /* always paint a small rectangle of current position */
    if( m_currentMapPoint == null )
      return;

    /* Paint handle of mouse position */
    final int[][] posPoints = UtilMap.getPointArrays( m_currentMapPoint );

    final int[] arrayX = posPoints[0];
    final int[] arrayY = posPoints[1];

    UtilMap.drawHandles( g, arrayX, arrayY );

    if( !m_nodeList.isEmpty() )
    {
      try
      {
        final LineGeometryBuilder geometryBuilder = createLineBuilder( mapPanel );

        final GeoTransform projection = mapPanel.getProjection();
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

  private LineGeometryBuilder createLineBuilder( final IMapPanel mapPanel ) throws Exception
  {
    final IKalypsoLayerModell modell = mapPanel.getMapModell();

    final LineGeometryBuilder geometryBuilder = new LineGeometryBuilder( 0, modell.getCoordinatesSystem() );
    for( int i = 0; i < m_nodeList.size(); i++ )
      geometryBuilder.addPoint( m_nodeList.get( i ).getPoint() );

    final GM_Point currentPoint = MapUtilities.transform( getMapPanel(), m_currentMapPoint );

    geometryBuilder.addPoint( currentPoint );

    return geometryBuilder;
  }
}