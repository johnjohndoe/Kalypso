/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.gml.widgets;

import java.awt.Graphics;
import java.awt.Point;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.events.MouseWheelListener;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypso.ogc.gml.selection.IFeatureSelectionListener;

/**
 * Der Controller für die MapView
 * 
 * @author vdoemming
 */
public class SwtWidgetManager implements IWidgetManager, MouseListener, MouseMoveListener, MouseWheelListener, KeyListener
{
  private final Set<IWidgetChangeListener> m_widgetChangeListener = new HashSet<IWidgetChangeListener>();

  private final IFeatureSelectionListener m_featureSelectionListener = new IFeatureSelectionListener()
  {
    public void selectionChanged( final IFeatureSelection selection )
    {
      onSelectionChanged( selection );
    }
  };

  private final IMapPanel m_mapPanel;

  private final ICommandTarget m_commandTarget;

  private IWidget m_actualWidget = null;

  private boolean m_mouseDown;

  public SwtWidgetManager( final ICommandTarget commandTarget, final IMapPanel mapPanel )
  {
    m_mapPanel = mapPanel;
    m_commandTarget = commandTarget;

    m_mapPanel.getSelectionManager().addSelectionListener( m_featureSelectionListener );
  }

  public void dispose( )
  {
    setActualWidget( null );

    m_mapPanel.getSelectionManager().removeSelectionListener( m_featureSelectionListener );
  }

  public ICommandTarget getCommandTarget( )
  {
    return m_commandTarget;
  }

  public void paintWidget( final Graphics g )
  {
    final IWidget actualWidget = getActualWidget();
    if( actualWidget != null )
      actualWidget.paint( g );
  }

  public IWidget getActualWidget( )
  {
    return m_actualWidget;
  }

  public void setActualWidget( final IWidget newWidget )
  {
    if( m_actualWidget != null )
      m_actualWidget.finish();

    m_actualWidget = newWidget;

    if( m_actualWidget != null )
    {
      m_actualWidget.activate( m_commandTarget, m_mapPanel );
      m_actualWidget.setSelection( m_mapPanel.getSelectionManager() );
    }

    fireWidgetChangeEvent( newWidget );

    if( m_mapPanel != null )
      m_mapPanel.repaintMap();
  }

  /**
   * Adds a listener to this manager.
   * <p>
   * Has no effect, if the same listener was already registered.
   */
  public void addWidgetChangeListener( final IWidgetChangeListener listener )
  {
    m_widgetChangeListener.add( listener );
  }

  /**
   * Removes a listener from this manager.
   * <p>
   * Has no effect, if this listener was not added to this manager before.
   */
  public void removeWidgetChangeListener( final IWidgetChangeListener listener )
  {
    m_widgetChangeListener.remove( listener );
  }

  private void fireWidgetChangeEvent( final IWidget newWidget )
  {
    final IWidgetChangeListener[] listener = m_widgetChangeListener.toArray( new IWidgetChangeListener[m_widgetChangeListener.size()] );
    for( final IWidgetChangeListener element : listener )
      element.widgetChanged( newWidget );
  }

  protected void onSelectionChanged( final IFeatureSelection selection )
  {
    if( m_actualWidget != null )
      m_actualWidget.setSelection( selection );
  }


  // MouseAdapter

  // FIXME: not supported by SWT

// /**
// * @see java.awt.event.KeyListener#keyTyped(java.awt.event.KeyEvent)
// */
// public void keyTyped( final KeyEvent e )
// {
// final IWidget widget = getActualWidget();
// if( widget != null )
// widget.keyTyped( e );
// }


  // /////////// SWT /////////////

  /**
   * @see org.eclipse.swt.events.MouseListener#mouseDoubleClick(org.eclipse.swt.events.MouseEvent)
   */
  @Override
  public void mouseDoubleClick( final org.eclipse.swt.events.MouseEvent e )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.eclipse.swt.events.MouseListener#mouseDown(org.eclipse.swt.events.MouseEvent)
   */
  @Override
  public void mouseDown( final org.eclipse.swt.events.MouseEvent e )
  {
    m_mouseDown = true;

    // MOUSE PRESSED
    final IWidget actualWidget = getActualWidget();
    if( actualWidget == null )
      return;

    // TODO: popup trigger not supported in SWT; we must rewrite the popup code
    // (was never good anyway)
    // if( e.isPopupTrigger() )
    // actualWidget.clickPopup( point );
    // else

    final Point point = new Point( e.x, e.y );

    switch( e.button )
    {
      case 1:
        actualWidget.leftPressed( point );
        break;

      case 2:
        actualWidget.middlePressed( point );
        break;

      case 3:
        actualWidget.rightPressed( point );
        break;

      default:
        break;
    }
  }

  /**
   * @see org.eclipse.swt.events.MouseListener#mouseUp(org.eclipse.swt.events.MouseEvent)
   */
  @Override
  public void mouseUp( final org.eclipse.swt.events.MouseEvent e )
  {
    final boolean mouseWasDown = m_mouseDown;

    m_mouseDown = false;

    // MOUSE RELEASED
    // MOUSE_CLICKED
    final IWidget actualWidget = getActualWidget();
    if( getActualWidget() == null )
      return;

    final Point point = new Point( e.x, e.y );

    // REMARK: pop-up trigger not supported in SWT; we should rewrite the popup code, it was never good anyway
    if( mouseWasDown && e.count == 1 && e.button == 3 )
      actualWidget.clickPopup( point );

    switch( e.button )
    {
      case 1:
        actualWidget.leftReleased( point );
        break;

      case 2:
        actualWidget.middleReleased( point );
        break;

      case 3:
        actualWidget.rightReleased( point );
        break;

      default:
        break;
    }

    // 'Click' holds only, if mouse was pressed in the same composite
    if( !mouseWasDown )
      return;

    switch( e.button )
    {
      case 1:
        if( e.count == 1 )
          actualWidget.leftClicked( point );
        else if( e.count == 2 )
          actualWidget.doubleClickedLeft( point );
        break;

      case 2:
        actualWidget.middleClicked( point );
        break;

      case 3:
        if( e.count == 1 )
          actualWidget.rightClicked( point );
        else if( e.count == 2 )
          actualWidget.doubleClickedRight( point );
        break;

      default:
        break;
    }
  }

  /**
   * @see org.eclipse.swt.events.MouseMoveListener#mouseMove(org.eclipse.swt.events.MouseEvent)
   */
  @Override
  public void mouseMove( final org.eclipse.swt.events.MouseEvent e )
  {
    // MOUSE MOVED
    final IWidget actualWidget = getActualWidget();
    if( actualWidget == null )
      return;

    final Point point = new Point( e.x, e.y );

    actualWidget.moved( point );

    m_mapPanel.fireMouseMouveEvent( e.x, e.y );

    // MOUSE DRAGGED
    if( m_mouseDown )
      actualWidget.dragged( point );
  }

  /**
   * @see org.eclipse.swt.events.MouseWheelListener#mouseScrolled(org.eclipse.swt.events.MouseEvent)
   */
  @Override
  public void mouseScrolled( final org.eclipse.swt.events.MouseEvent e )
  {
    // What to do...
    // Zoom in/out ...?
  }

  /**
   * @see org.eclipse.swt.events.KeyListener#keyPressed(org.eclipse.swt.events.KeyEvent)
   */
  @Override
  public void keyPressed( final org.eclipse.swt.events.KeyEvent e )
  {
    // KEY_PRESSED
    final IWidget widget = getActualWidget();
    if( widget == null )
      return;

// KeyEvent evt = new KeyEvent(null, -1, -1, e.stateMask,e.keyCode, e.character );
// widget.keyPressed( evt );
  }

  /**
   * @see org.eclipse.swt.events.KeyListener#keyReleased(org.eclipse.swt.events.KeyEvent)
   */
  @Override
  public void keyReleased( final org.eclipse.swt.events.KeyEvent e )
  {
    final IWidget widget = getActualWidget();
    if( widget == null )
      return;

// KeyEvent evt = new KeyEvent(null, -1, -1, e.stateMask,e.keyCode, e.character );
// widget.keyReleased( e );

  }
}