package org.kalypso.kalypsomodel1d2d.ui.map.cline;

import java.awt.Cursor;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.DiscretisationModelUtils;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IContinuityLine1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IContinuityLine2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.CreateContinuityLineCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.util.PointSnapper;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.widgets.builders.LineGeometryBuilder;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.widgets.AbstractWidget;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * @author Gernot Belger
 * @author Patrice Congo
 * @author Dejan Antanaskovic
 */
public class CreateFEContinuityLineWidget extends AbstractWidget
{

  private IFEDiscretisationModel1d2d m_discModel = null;

  /* The current node of the disc-model under the cursor. */
  private IFE1D2DNode m_currentNode = null;

  private Point m_currentMapPoint;

  private PointSnapper m_pointSnapper;

  private List<IFE1D2DNode> m_nodeList = null;

  private QName m_lineType = null;

  public CreateFEContinuityLineWidget( )
  {
    super( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.cline.CreateFEContinuityLineWidget.0" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.cline.CreateFEContinuityLineWidget.1" ) ); //$NON-NLS-1$ //$NON-NLS-2$
    m_nodeList = new ArrayList<IFE1D2DNode>();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public void activate( final ICommandTarget commandPoster, final IMapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );
    if( getMapPanel().getMapModell() == null )
      return;

    m_discModel = UtilMap.findFEModelTheme( getMapPanel() );
    m_pointSnapper = new PointSnapper( m_discModel, mapPanel );
    reinit();
  }

  private void reinit( )
  {
    m_currentNode = null;
    m_lineType = null;
    m_nodeList.clear();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#keyTyped(java.awt.event.KeyEvent)
   */
  @Override
  public void keyTyped( final KeyEvent e )
  {
    if( KeyEvent.VK_ESCAPE == e.getKeyChar() )
    {
      reinit();
      getMapPanel().repaintMap();
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
      final IFE1D2DNode candidateNode = (IFE1D2DNode) newNode;
      m_currentMapPoint = MapUtilities.retransform( getMapPanel(), candidateNode.getPoint() );

      if( m_lineType == null )
        m_currentNode = (IFE1D2DNode) newNode;
      else
      {
        if( m_lineType.equals( IContinuityLine1D.QNAME ) )
        {
          if( DiscretisationModelUtils.is1DNode( candidateNode ) )
            m_currentNode = candidateNode;
          else
            m_currentNode = null;
        }
        else
        {
          if( DiscretisationModelUtils.is1DNode( candidateNode ) )
            m_currentNode = null;
          else
            m_currentNode = candidateNode;
        }
      }
    }
    else
      m_currentMapPoint = p;

    if( newNode == null )
      getMapPanel().setCursor( Cursor.getPredefinedCursor( Cursor.CROSSHAIR_CURSOR ) );
    else
      getMapPanel().setCursor( Cursor.getDefaultCursor() );

    repaintMap();

    // if no line is started before, allowed selection is element 1D or element 2D
    // if 2D line is started: if selection is 2d node, add it; if selection is 1d node, ignore it
    // opposite for 1d line

  }

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
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftClicked(java.awt.Point)
   */
  @Override
  public void leftClicked( final Point p )
  {
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;

    if( m_currentNode == null )
      return;

    m_nodeList.add( m_currentNode );

    if( DiscretisationModelUtils.is1DNode( m_currentNode ) )
    {
      m_lineType = IContinuityLine1D.QNAME;

      // TODO: check if there is already a 1d boundary line
      if( !checkFor1DContiLine() )
      {
        createBoundaryLine();
        m_nodeList.clear();

      }
      else
        reinit();
    }
    else
      m_lineType = IContinuityLine2D.QNAME;

    getMapPanel().repaintMap();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#doubleClickedLeft(java.awt.Point)
   */
  @Override
  public void doubleClickedLeft( final Point p )
  {
    if( m_currentNode == null )
    {
      // reinit();
      return;
    }
    else
    {
      if( m_nodeList.size() == 1 )
      {
        m_lineType = IContinuityLine1D.QNAME;

        // TODO: check if there is already a 1d boundary line

        if( !checkFor1DContiLine() )
        {
          createBoundaryLine();
          m_nodeList.clear();
        }
        else
          reinit();
      }
    }
    createBoundaryLine();
  }

  private boolean checkFor1DContiLine( )
  {
    final IFE1D2DNode node = m_nodeList.get( 0 );

    if( node == null )
      return false;

    final IFELine contiLine = m_discModel.findContinuityLine( node.getPoint(), 0.1 );
    if( contiLine == null || contiLine instanceof IContinuityLine2D )
      return false;
    else
      return true;
  }

  private void createBoundaryLine( )
  {
    try
    {
      if( m_nodeList.isEmpty() )
        return;

      // TODO: check if there is already a boundary line

      final IKalypsoTheme theme = UtilMap.findEditableTheme( getMapPanel(), Kalypso1D2DSchemaConstants.WB1D2D_F_NODE );
      final CommandableWorkspace workspace = ((IKalypsoFeatureTheme) theme).getWorkspace();

      final CreateContinuityLineCommand command = new CreateContinuityLineCommand( m_discModel, m_nodeList, m_lineType );
      workspace.postCommand( command );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      KalypsoModel1D2DPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );

      final IStatus status = StatusUtilities.statusFromThrowable( e );

      final Shell shell = PlatformUI.getWorkbench().getWorkbenchWindows()[0].getActivePage().getActivePart().getSite().getShell();
      shell.getDisplay().asyncExec( new Runnable()
      {
        @Override
        public void run( )
        {
          ErrorDialog.openError( shell, getName(), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.cline.CreateFEContinuityLineWidget.2" ), status ); //$NON-NLS-1$
        }
      } );
    }
    finally
    {
      reinit();
    }
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

    if( !m_nodeList.isEmpty() )
    {
      final LineGeometryBuilder geometryBuilder = new LineGeometryBuilder( 0, getMapPanel().getMapModell().getCoordinatesSystem() );
      try
      {
        for( int i = 0; i < m_nodeList.size(); i++ )
          geometryBuilder.addPoint( m_nodeList.get( i ).getPoint() );
        geometryBuilder.paint( g, getMapPanel().getProjection(), m_currentMapPoint );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
    }
  }

}
