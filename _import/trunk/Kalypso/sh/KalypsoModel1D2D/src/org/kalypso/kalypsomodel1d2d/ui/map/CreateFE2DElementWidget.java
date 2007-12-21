package org.kalypso.kalypsomodel1d2d.ui.map;

import java.awt.Cursor;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.KeyEvent;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.util.PointSnapper;
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
  private ElementGeometryBuilder m_builder = null;

  private IKalypsoFeatureTheme m_nodeTheme;

  private PointSnapper m_pointSnapper;

  private Point m_currentMapPoint;

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

    final IMapModell mapModell = mapPanel.getMapModell();
    final IFEDiscretisationModel1d2d discModel = UtilMap.findFEModelTheme( mapModell );
    // we must have the node theme. First node theme gets it
    m_nodeTheme = UtilMap.findEditableTheme( mapModell, Kalypso1D2DSchemaConstants.WB1D2D_F_NODE );
    m_pointSnapper = new PointSnapper( discModel, mapPanel );

    reinit();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#finish()
   */
  @Override
  public void finish( )
  {
    super.finish();

    getMapPanel().setMessage( "" );
    getMapPanel().setCursor( Cursor.getDefaultCursor() );
  }

  protected void reinit( )
  {
    m_builder = null;

    if( m_nodeTheme != null )
      m_builder = new ElementGeometryBuilder( 4, m_nodeTheme );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
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
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#keyPressed(java.awt.event.KeyEvent)
   */
  @Override
  public void keyPressed( final KeyEvent e )
  {
    if( e.getKeyCode() == KeyEvent.VK_SHIFT )
      m_pointSnapper.activate( false );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#keyReleased(java.awt.event.KeyEvent)
   */
  @Override
  public void keyReleased( final KeyEvent e )
  {
    if( e.getKeyCode() == KeyEvent.VK_SHIFT )
      m_pointSnapper.activate( true );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#keyTyped(java.awt.event.KeyEvent)
   */
  @Override
  public void keyTyped( final KeyEvent e )
  {
    super.keyTyped( e );
    if( KeyEvent.VK_ESCAPE == e.getKeyChar() )
    {
      reinit();
      mapRepaint();
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.EditGeometryWidget#moved(java.awt.Point)
   */
  @Override
  public void moved( final Point p )
  {
    final Object newNode = checkNewNode( p );
    if( newNode instanceof IFE1D2DNode )
      m_currentMapPoint = MapUtilities.retransform( getMapPanel(), ((IFE1D2DNode) newNode).getPoint() );
    else
      m_currentMapPoint = p;

    if( newNode == null )
      getMapPanel().setCursor( Cursor.getPredefinedCursor( Cursor.CROSSHAIR_CURSOR ) );
    else
      getMapPanel().setCursor( Cursor.getDefaultCursor() );

    mapRepaint();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftClicked(java.awt.Point)
   */
  @Override
  public void leftClicked( final Point p )
  {
    final Object newNode = checkNewNode( p );

    if( newNode == null )
      return;

    try
    {
      final ICommand command = m_builder.addNode( newNode );

      if( command != null )
      {
        m_nodeTheme.getWorkspace().postCommand( command );
        reinit();
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      KalypsoModel1D2DPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
    }
    finally
    {
      mapRepaint();
    }
  }

  /**
   * TODO: change to right-clicked: BUT!: at the moment the context menu is opened, so the framework must know whether
   * this widget is editing something at the moment or not
   * 
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#doubleClickedLeft(java.awt.Point)
   */
  @Override
  public void doubleClickedLeft( final Point p )
  {
    // Check again, else we would need a state flag, whether leftClicked was OK
    if( checkNewNode( p ) == null )
      return;

    if( m_builder.getNumberOfNodes() < 3 )
      return;
    try
    {
      final ICommand command = m_builder.finish();
      if( command != null )
        m_nodeTheme.getWorkspace().postCommand( command );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      KalypsoModel1D2DPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
    }
    finally
    {
      reinit();
      mapRepaint();
    }
  }

  private Object checkNewNode( final Point p )
  {
    final MapPanel mapPanel = getMapPanel();
    if( mapPanel == null || m_builder == null )
      return null;

    final GM_Point currentPoint = MapUtilities.transform( mapPanel, p );
    final IFE1D2DNode snapNode = m_pointSnapper == null ? null : m_pointSnapper.moved( currentPoint );
    final Object newNode = snapNode == null ? currentPoint : snapNode;

    final IStatus status = m_builder.checkNewNode( newNode );
    if( status.isOK() )
      mapPanel.setMessage( "" );
    else
      mapPanel.setMessage( status.getMessage() );

    if( status.isOK() )
      return newNode;

    return null;
  }

}
