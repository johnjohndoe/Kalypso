package org.kalypso.kalypsomodel1d2d.ui.map.cline;

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
import org.kalypso.kalypsomodel1d2d.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.DiscretisationModelUtils;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IContinuityLine1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IContinuityLine2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.CreateContinuityLineCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.widgets.AbstractWidget;
import org.kalypso.ogc.gml.map.widgets.builders.LineGeometryBuilder;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * @author Gernot Belger
 * @author Patrice Congo
 * @author Dejan Antanaskovic
 */
public class CreateFEContinuityLineWidget extends AbstractWidget
{
  private final int m_grabRadius = 10;

  private IFEDiscretisationModel1d2d m_discModel = null;

  /* The current node of the disc-model under the cursor. */
  private IFE1D2DNode m_currentNode = null;

  private Point m_currentPoint = new Point();

  private List<IFE1D2DNode> m_nodeList = null;

  private QName m_lineType = null;

  public CreateFEContinuityLineWidget( )
  {
    super( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.CreateFEContlineWidget.0" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.CreateFEContlineWidget.1" ) ); //$NON-NLS-1$ $NON-NLS-2$
    m_nodeList = new ArrayList<IFE1D2DNode>();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public void activate( final ICommandTarget commandPoster, final MapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );
    if( getMapPanel().getMapModell() == null )
      return;
    reinit();
  }

  private void reinit( )
  {
    m_currentNode = null;
    m_lineType = null;
    m_nodeList.clear();
  }

  private IFEDiscretisationModel1d2d getDiscretisationModel( )
  {
    if( m_discModel == null )
      m_discModel = UtilMap.findFEModelTheme( getMapPanel().getMapModell() );
    return m_discModel;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#keyTyped(java.awt.event.KeyEvent)
   */
  @Override
  public void keyTyped( KeyEvent e )
  {
    if( KeyEvent.VK_ESCAPE == e.getKeyChar() )
    {
      reinit();
      getMapPanel().repaint();
    }
  }

  @Override
  public void moved( final Point p )
  {
    final GM_Point currentPos = MapUtilities.transform( getMapPanel(), p );
    final double grabDistance = MapUtilities.calculateWorldDistance( getMapPanel(), currentPos, m_grabRadius );

    final IFE1D2DNode candidateNode = getDiscretisationModel().findNode( currentPos, grabDistance );

    // TODO check if node is already part of some boundary line; if so, ignore it

    // if no line is started before, allowed selection is element 1D or element 2D
    // if 2D line is started: if selection is 2d node, add it; if selection is 1d node, ignore it
    // oposite for 1d line

    if( candidateNode == null )
      m_currentNode = null;
    else
    {
      if( m_lineType == null )
        m_currentNode = candidateNode;
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
    setCurrentPoint( currentPos );
  }

  private void setCurrentPoint( final GM_Point currentPos )
  {
    if( m_currentNode == null )
      m_currentPoint = MapUtilities.retransform( getMapPanel(), currentPos );
    else
      m_currentPoint = MapUtilities.retransform( getMapPanel(), m_currentNode.getPoint() );
    getMapPanel().repaint();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftClicked(java.awt.Point)
   */
  @Override
  public void leftClicked( final Point p )
  {
    if( m_currentNode == null )
      return;
    if( DiscretisationModelUtils.is1DNode( m_currentNode ) )
    {
      m_lineType = IContinuityLine1D.QNAME;
      m_nodeList.clear();
    }
    else
      m_lineType = IContinuityLine2D.QNAME;

    m_nodeList.add( m_currentNode );
    getMapPanel().repaint();
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
    try
    {
      if( m_nodeList.isEmpty() )
        return;
      final IKalypsoTheme theme = UtilMap.findEditableTheme( getMapPanel().getMapModell(), Kalypso1D2DSchemaConstants.WB1D2D_F_NODE );
      final CommandableWorkspace workspace = ((IKalypsoFeatureTheme) theme).getWorkspace();

      final CreateContinuityLineCommand command = new CreateContinuityLineCommand( getDiscretisationModel(), m_nodeList, m_lineType );
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
        public void run( )
        {
          ErrorDialog.openError( shell, getName(), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.CreateFEContlineWidget.2" ), status ); //$NON-NLS-1$
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
  public void paint( final Graphics graphics )
  {
    if( m_currentNode != null )
      graphics.drawRect( (int) m_currentPoint.getX() - m_grabRadius, (int) m_currentPoint.getY() - m_grabRadius, m_grabRadius * 2, m_grabRadius * 2 );
    if( !m_nodeList.isEmpty() )
    {
      final LineGeometryBuilder geometryBuilder = new LineGeometryBuilder( 0, getMapPanel().getMapModell().getCoordinatesSystem() );
      try
      {
        for( int i = 0; i < m_nodeList.size(); i++ )
          geometryBuilder.addPoint( m_nodeList.get( i ).getPoint() );
        geometryBuilder.paint( graphics, getMapPanel().getProjection(), m_currentPoint );
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
    }
  }
}
