package org.kalypso.kalypsomodel1d2d.ui.map;

import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.KeyEvent;

import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.widgets.AbstractWidget;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * @author Gernot Belger
 */
public class CreateFE2DElementWidget extends AbstractWidget
{
  private Point m_currentPoint = null;

  private ElementGeometryBuilder m_builder = null;

  private IKalypsoFeatureTheme m_nodeTheme;

  private IFEDiscretisationModel1d2d m_discModel = null;

  private final int m_radius = 20;

  public CreateFE2DElementWidget( )
  {
    super( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.CreateFE2DElementWidget.0" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.CreateFE2DElementWidget.1" ) ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public void activate( final ICommandTarget commandPoster, final MapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );

    // find the right themes to edit i.e. the discretisation model

    reinit();
  }

  private final void reinit( )
  {
    m_builder = null;
    m_discModel = null;

    // we must have the node and the element theme, one of them must be active
    // First node theme gets it

    final IMapModell mapModell = getMapPanel().getMapModell();
    m_nodeTheme = UtilMap.findEditableTheme( mapModell, Kalypso1D2DSchemaConstants.WB1D2D_F_NODE );
    if( m_nodeTheme != null )
      m_builder = new ElementGeometryBuilder( 4, m_nodeTheme );

    m_discModel = UtilMap.findFEModelTheme( mapModell );
  }

  @Override
  public void moved( final Point p )
  {
    m_currentPoint = p;
    final MapPanel panel = getMapPanel();
    if( panel != null )
      panel.repaint();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftClicked(java.awt.Point)
   */
  @Override
  public void leftClicked( final Point p )
  {
    try
    {
      /* snap to next node */
      // TODO: exclude already found nodes?
      final MapPanel mapPanel = getMapPanel();
      final GM_Point currentPosition = MapUtilities.transform( mapPanel, p );
      final double snapDistance = MapUtilities.calculateWorldDistance( mapPanel, currentPosition, m_radius );
      final IFE1D2DNode snapNode = m_discModel.findNode( currentPosition, snapDistance );

      final ICommand command;
      if( snapNode != null )
        command = m_builder.addNode( snapNode );
      else
        command = m_builder.addNode( currentPosition );

      if( command != null )
      {
        m_nodeTheme.getWorkspace().postCommand( command );

        reinit();
      }

      mapPanel.repaint();
    }
    catch( final Exception e )
    {
      KalypsoModel1D2DPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
    }
  }

  /**
   * TODO: change to right-clicked: BUT!: at the moment the xontext menu is opened, so the framework must know wether
   * this widget is editing something at the moment or not
   * 
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#doubleClickedLeft(java.awt.Point)
   */
  @Override
  public void doubleClickedLeft( final Point p )
  {
    try
    {
      final ICommand command = m_builder.finish();
      if( command != null )
      {
        m_nodeTheme.getWorkspace().postCommand( command );
        m_builder = new ElementGeometryBuilder( 4, m_nodeTheme );
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      KalypsoModel1D2DPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    final Point currentPoint = m_currentPoint;

    if( currentPoint != null )
    {
      if( m_builder != null )
        m_builder.paint( g, getMapPanel().getProjection(), currentPoint );
      g.drawRect( (int) currentPoint.getX() - 10, (int) currentPoint.getY() - 10, 20, 20 );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#keyTyped(java.awt.event.KeyEvent)
   */
  @Override
  public void keyTyped( final KeyEvent e )
  {
    if( KeyEvent.VK_ESCAPE == e.getKeyChar()/* Code() */)
    {
      this.reinit();
      getMapPanel().repaint();
    }
    // super.keyTyped(e);

  }
}
