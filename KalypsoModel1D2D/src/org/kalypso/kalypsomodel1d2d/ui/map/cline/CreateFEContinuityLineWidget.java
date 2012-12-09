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
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * @author Gernot Belger
 * @author Patrice Congo
 * @author Dejan Antanaskovic
 */
public class CreateFEContinuityLineWidget extends AbstractWidget
{
  // FIXME: tooltips
  private final ToolTipRenderer m_toolTipRenderer = new ToolTipRenderer();

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

    m_currentNode = null;

    if( snapNode != null )
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
    else if( checkFor1DContiLine( m_currentNode ) )
      createBoundaryLine1D( m_currentNode );
    else
      reinit();
  }

  private void doubleClickedLeft( )
  {
    if( m_currentNode == null )
      return;

    createBoundaryLine2D( m_nodeList.toArray( new IFE1D2DNode[m_nodeList.size()] ) );
  }

  private boolean checkFor1DContiLine( final IFE1D2DNode node )
  {
    if( node == null )
      return false;

    // TODO: would be nice to show the validations during mouse move

    /* check for end of reach */
    final IFE1D2DElement[] elements = node.getAdjacentElements();
    if( elements.length == 0 )
    {
      final IStatus status = new Status( IStatus.INFO, KalypsoModel1D2DPlugin.PLUGIN_ID, "This is a single 1D node that is not attached to the net. Continuity line not added." );
      handleError( status );
      return false;
    }
    else if( elements.length > 1 )
    {
      final IStatus status = new Status( IStatus.INFO, KalypsoModel1D2DPlugin.PLUGIN_ID, "This node is not at the end of the 1D-reach. Continuity line not added." );
      handleError( status );
      return false;
    }

    /* check for existance of another conti line */
    final IFELine contiLine = m_discModel.findContinuityLine( node.getPoint(), 0.1 );
    if( contiLine == null )
      return true;

    final IStatus status = new Status( IStatus.INFO, KalypsoModel1D2DPlugin.PLUGIN_ID, "There already is a continuity line at the end of this 1D reach. Continuity line not added." );
    handleError( status );

    return false;
    // return contiLine == null || contiLine instanceof IContinuityLine2D;
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
      final IKalypsoLayerModell modell = mapPanel.getMapModell();

      final LineGeometryBuilder geometryBuilder = new LineGeometryBuilder( 0, modell.getCoordinatesSystem() );
      try
      {
        for( int i = 0; i < m_nodeList.size(); i++ )
          geometryBuilder.addPoint( m_nodeList.get( i ).getPoint() );

        final GeoTransform projection = mapPanel.getProjection();
        geometryBuilder.paint( g, projection, m_currentMapPoint );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
    }

    /* paint the snap */
    if( m_pointSnapper != null )
      m_pointSnapper.paint( g );
  }
}