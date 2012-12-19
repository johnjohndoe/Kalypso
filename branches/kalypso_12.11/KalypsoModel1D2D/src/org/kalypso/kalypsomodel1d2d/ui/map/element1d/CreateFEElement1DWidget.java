package org.kalypso.kalypsomodel1d2d.ui.map.element1d;

import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.utilities.tooltip.ToolTipRenderer;
import org.kalypso.ogc.gml.map.widgets.builders.LineGeometryBuilder;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.widgets.AbstractWidget;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * Widget for creating 1d2d element
 *
 * @author Patrice Congo
 */
public class CreateFEElement1DWidget extends AbstractWidget
{
  private final int m_grabRadius = 20;

  private LineGeometryBuilder m_lineBuilder = null;

  private IKalypsoFeatureTheme m_theme;

  private IFEDiscretisationModel1d2d m_model1d2d;

  private GM_Point m_currentPos = null;

  private final ToolTipRenderer m_toolTipRenderer = new ToolTipRenderer();

  private IFE1D2DNode m_node;

  public CreateFEElement1DWidget( )
  {
    super( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.element1d.CreateFEElement1DWidget.0" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.element1d.CreateFEElement1DWidget.1" ) ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  @Override
  public void activate( final ICommandTarget commandPoster, final IMapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );
    reinit();
  }

  private final void reinit( )
  {
    final IMapPanel mapPanel = getMapPanel();

    mapPanel.setMessage( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.element1d.CreateFEElement1DWidget.2" ) ); //$NON-NLS-1$

    final IMapModell mapModell = mapPanel.getMapModell();
    m_theme = UtilMap.findEditableTheme( mapPanel, IElement1D.QNAME );
    m_model1d2d = UtilMap.findFEModelTheme( mapPanel );
    m_lineBuilder = new LineGeometryBuilder( 0, mapModell.getCoordinatesSystem() );
  }

  @Override
  public void mouseMoved( final MouseEvent event )
  {
    m_currentPos = MapUtilities.transform( getMapPanel(), event.getPoint() );

    /* find node */
    m_node = null;

    final double grabDistance = MapUtilities.calculateWorldDistance( getMapPanel(), m_currentPos, m_grabRadius * 2 );
    if( m_model1d2d == null )
      return;

    m_node = m_model1d2d.findNode( m_currentPos, grabDistance );

    final IMapPanel panel = getMapPanel();
    if( panel != null )
      panel.repaintMap();
  }

  @Override
  public void mouseClicked( final MouseEvent event )
  {
    if( event.getButton() != MouseEvent.BUTTON1 )
      return;

    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;

    event.consume();

    try
    {
      if( event.getClickCount() > 1 )
        finishLine();
      else
      {
        /* If we have a node, take this position, else take the current one */
        final Point point = event.getPoint();
        final GM_Point currentPos = m_node == null ? MapUtilities.transform( mapPanel, point ) : m_node.getPoint();

        mapPanel.setMessage( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.element1d.CreateFEElement1DWidget.3" ) ); //$NON-NLS-1$

        m_lineBuilder.addPoint( currentPos );
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      final IStatus status = StatusUtilities.statusFromThrowable( e );
      KalypsoModel1D2DPlugin.getDefault().getLog().log( status );
      mapPanel.setMessage( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.element1d.CreateFEElement1DWidget.4" ) + status.getMessage() ); //$NON-NLS-1$

      reinit();
    }
  }

  private void finishLine( ) throws Exception
  {
    if( m_lineBuilder == null )
      return;

    final GM_Curve curve = (GM_Curve) m_lineBuilder.finish();

    final FeatureList featureList = m_theme.getFeatureList();

    /* Initialize elements needed for edges and elements */
    final IFEDiscretisationModel1d2d discModel = (IFEDiscretisationModel1d2d) featureList.getOwner();

    final Create1DElementCommand command = new Create1DElementCommand( discModel, curve );
    m_theme.getWorkspace().postCommand( command );

    reinit();
  }

  @Override
  public void paint( final Graphics g )
  {
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;

    super.paint( g );

    if( m_currentPos != null )
    {
      final Point currentPoint = MapUtilities.retransform( getMapPanel(), m_currentPos );
      if( m_lineBuilder != null )
        m_lineBuilder.paint( g, getMapPanel().getProjection(), currentPoint );
    }

    if( m_node != null )
    {
      final int smallRect = 10;
      final Point nodePoint = MapUtilities.retransform( getMapPanel(), m_node.getPoint() );
      g.drawRect( (int) nodePoint.getX() - smallRect, (int) nodePoint.getY() - smallRect, smallRect * 2, smallRect * 2 );
    }

    final Rectangle bounds = mapPanel.getScreenBounds();

    m_toolTipRenderer.setTooltip( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.element1d.CreateFEElement1DWidget.7" ) ); //$NON-NLS-1$
    m_toolTipRenderer.paintToolTip( new Point( 5, bounds.height - 5 ), g, bounds );
  }

  @Override
  public void keyPressed( final KeyEvent e )
  {
    final int keyCode = e.getKeyCode();
    switch( keyCode )
    {
      case KeyEvent.VK_ESCAPE:
        reinit();
        getMapPanel().repaintMap();
        break;

      case KeyEvent.VK_BACK_SPACE:
        m_lineBuilder.removeLastPoint();
        getMapPanel().repaintMap();
        break;

      default:
        break;
    }
  }
}
