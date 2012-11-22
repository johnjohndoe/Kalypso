package org.kalypso.kalypsomodel1d2d.ui.map;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.KeyEvent;
import java.util.List;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.element1d.Create2dElementCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.util.Add2DElementsCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.util.PointSnapper;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.utilities.tooltip.ToolTipRenderer;
import org.kalypso.ogc.gml.widgets.DeprecatedMouseWidget;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_PolygonPatch;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

import com.google.common.collect.ImmutableList;

/**
 * @author Gernot Belger
 * @author Thomas Jung
 */
public class CreateFE2DElementWidget extends DeprecatedMouseWidget
{
  private ElementGeometryBuilder m_builder = null;

  private IKalypsoFeatureTheme m_nodeTheme;

  private PointSnapper m_pointSnapper;

  private Point m_currentMapPoint;

  private final ToolTipRenderer m_toolTipRenderer = new ToolTipRenderer();

  private final ToolTipRenderer m_warningRenderer = new ToolTipRenderer();

  private boolean m_warning;

  public CreateFE2DElementWidget( )
  {
    super( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.CreateFE2DElementWidget.0" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.CreateFE2DElementWidget.1" ) ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  @Override
  public void activate( final ICommandTarget commandPoster, final IMapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );

    m_toolTipRenderer.setBackgroundColor( new Color( 1f, 1f, 0.6f, 0.70f ) );
    m_warningRenderer.setBackgroundColor( new Color( 1f, 0.4f, 0.4f, 0.80f ) );

    final IFEDiscretisationModel1d2d discModel = UtilMap.findFEModelTheme( mapPanel );
    // we must have a discretization model theme
    m_nodeTheme = UtilMap.findEditableTheme( mapPanel, IFE1D2DNode.FEATURE_1D2DNODE );
    if( m_nodeTheme == null )
      m_nodeTheme = UtilMap.findEditableTheme( mapPanel, IFE1D2DEdge.QNAME );
    if( m_nodeTheme == null )
      m_nodeTheme = UtilMap.findEditableTheme( mapPanel, IPolyElement.QNAME );
    m_pointSnapper = new PointSnapper( discModel, mapPanel );

    reinit();
  }

  @Override
  public void finish( )
  {
    super.finish();

    getMapPanel().setMessage( "" ); //$NON-NLS-1$
    getMapPanel().setCursor( Cursor.getDefaultCursor() );
  }

  protected void reinit( )
  {
    m_builder = null;

    if( m_nodeTheme != null )
      m_builder = new ElementGeometryBuilder( 4, m_nodeTheme );
  }

  @Override
  public void paint( final Graphics g )
  {
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;

    if( m_currentMapPoint != null )
    {
      if( m_builder != null )
      {
        m_builder.paint( g, getMapPanel().getProjection(), m_currentMapPoint );

        if( m_pointSnapper != null )
          m_pointSnapper.paint( g );
      }
    }

    super.paint( g );

    final Rectangle bounds = mapPanel.getScreenBounds();
    m_toolTipRenderer.setTooltip( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.CreateFE2DElementWidget.3" ) ); //$NON-NLS-1$
    m_toolTipRenderer.paintToolTip( new Point( 5, bounds.height - 5 ), g, bounds );

    if( m_warning == true )
      m_warningRenderer.paintToolTip( new Point( 5, bounds.height - 80 ), g, bounds );

    if( m_pointSnapper != null )
    {
      final IFE1D2DNode snapNode = m_pointSnapper.getSnapNode();
      if( snapNode == null )
        return;

      final double z = snapNode.getPoint().getZ();
      if( !Double.isNaN( z ) )
      {
        final String format = Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.CreateFE2DElementWidget.4", z ); //$NON-NLS-1$
        getMapPanel().setMessage( format );
      }
    }

  }

  @Override
  public void keyPressed( final KeyEvent e )
  {
    if( e.getKeyCode() == KeyEvent.VK_SHIFT )
      m_pointSnapper.activate( false );
  }

  @Override
  public void keyReleased( final KeyEvent e )
  {
    if( e.getKeyCode() == KeyEvent.VK_SHIFT )
      m_pointSnapper.activate( true );
  }

  @Override
  public void keyTyped( final KeyEvent e )
  {
    super.keyTyped( e );
    if( KeyEvent.VK_ESCAPE == e.getKeyChar() )
    {
      reinit();
      repaintMap();
    }
    else if( KeyEvent.VK_BACK_SPACE == e.getKeyChar() || KeyEvent.VK_DELETE == e.getKeyChar() )
    {
      m_builder.removeLast();
      repaintMap();
    }
  }

  @Override
  public void moved( final Point p )
  {
    final Object newNode = checkNewNode( p );
    if( newNode instanceof IFE1D2DNode )
      m_currentMapPoint = MapUtilities.retransform( getMapPanel(), ((IFE1D2DNode)newNode).getPoint() );
    else
      m_currentMapPoint = p;

    if( newNode == null )
      getMapPanel().setCursor( Cursor.getPredefinedCursor( Cursor.CROSSHAIR_CURSOR ) );
    else
      getMapPanel().setCursor( Cursor.getDefaultCursor() );

    repaintMap();
  }

  @Override
  public void leftPressed( final Point p )
  {
    final Object newNode = checkNewNode( p );

    if( newNode == null )
      return;

    try
    {
      final Create2dElementCommand command;
      if( newNode instanceof GM_Point )
      {
        command = m_builder.addNode( (GM_Point)newNode );
      }
      else
      {
        command = m_builder.addNode( ((IFE1D2DNode)newNode).getPoint() );
      }

      if( command != null )
        createElement();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      KalypsoModel1D2DPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
    }
    finally
    {
      repaintMap();
    }
  }

  @Override
  public void doubleClickedLeft( final Point p )
  {
    // Check again, else we would need a state flag, whether leftClicked was OK
    if( checkNewNode( p ) == null )
      return;

    if( m_builder.getNumberOfNodes() < 3 )
      return;
    createElement();
  }

  private void createElement( )
  {
    try
    {
      final List<GM_Point> nodes = m_builder.getNodes();
      final GM_Position[] ring = ElementGeometryHelper.ringPositionsFromNodes( nodes.toArray( new GM_Point[nodes.size()] ) );
      final GM_PolygonPatch patch = GeometryFactory.createGM_PolygonPatch( ring, null, nodes.get( 0 ).getCoordinateSystem() );
      final Add2DElementsCommand command = new Add2DElementsCommand( m_nodeTheme.getWorkspace(), ImmutableList.of( patch ), null );
      m_nodeTheme.getWorkspace().postCommand( command );
      final IStatus status = command.getStatus();
      if( status.isOK() )
        m_warning = false;
      else
      {
        m_warning = true;
        m_warningRenderer.setTooltip( StatusUtilities.messageFromStatus( status ) );
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      KalypsoModel1D2DPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
    }
    finally
    {
      reinit();
      repaintMap();
    }
  }

  private Object checkNewNode( final Point p )
  {
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null || m_builder == null )
      return null;

    final GM_Point currentPoint = MapUtilities.transform( mapPanel, p );
    final IFE1D2DNode snapNode = m_pointSnapper == null ? null : m_pointSnapper.moved( currentPoint );
    final Object newNode = snapNode == null ? currentPoint : snapNode;

    if( newNode == null )
      return null;

    IStatus status;
    if( newNode instanceof GM_Point )
      status = m_builder.checkNewNode( (GM_Point)newNode );
    else
      status = m_builder.checkNewNode( ((IFE1D2DNode)newNode).getPoint() );

    if( status.isOK() )
      m_warning = false;
    else
    {
      if( status.getMessage().equals( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.CreateFE2DElementWidget.5" ) ) ) //$NON-NLS-1$
      {
        // TODO: delete element!

      }
      m_warning = true;
      m_warningRenderer.setTooltip( status.getMessage() );
    }

    if( status.isOK() )
      return newNode;

    return null;
  }

}
