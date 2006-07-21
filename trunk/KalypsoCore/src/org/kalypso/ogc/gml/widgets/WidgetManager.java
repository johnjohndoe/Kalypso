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
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.util.ArrayList;
import java.util.List;

import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.ogc.gml.map.MapPanel;

/**
 * Der Controller fuer die MapView
 * 
 * @author vdoemming
 */
public class WidgetManager implements MouseListener, MouseMotionListener, KeyListener
{
  private IWidget m_actualWidget = null;

  private final MapPanel m_MapPanel;

  private final ICommandTarget m_commandTarget;

  private Point m_lastDragged = null;

  private static final double MINIMUM_MOUSE_DISTANCE = 5;

  private Point m_lastMoved = null;

  private final List<IWidgetChangeListener> m_widgetChangeListener = new ArrayList<IWidgetChangeListener>();

  public WidgetManager( final ICommandTarget commandTarget, final MapPanel mapPanel )
  {
    m_MapPanel = mapPanel;
    m_commandTarget = commandTarget;
  }

  // MouseAdapter
  public void mouseClicked( MouseEvent e )
  {
    final IWidget actualWidget = getActualWidget();
    if( actualWidget == null )
      return;

    if( e.isPopupTrigger() )
      actualWidget.clickPopup( e.getPoint() );
    else
      switch( e.getButton() )
      {
        case MouseEvent.BUTTON1:
        {
          if( e.getClickCount() == 1 )
            actualWidget.leftClicked( e.getPoint() );
          else if( e.getClickCount() == 2 )
            actualWidget.doubleClickedLeft( e.getPoint() );
        }
          break;

        case MouseEvent.BUTTON2:
          actualWidget.middleClicked( e.getPoint() );
          break;

        case MouseEvent.BUTTON3:
        {
          if( e.getClickCount() == 1 )
            actualWidget.rightClicked( e.getPoint() );
          else if( e.getClickCount() == 2 )
            actualWidget.doubleClickedRight( e.getPoint() );
        }
          break;

        default:
          break;
      }
  }

  public void mouseMoved( MouseEvent e )
  {
    if( m_lastMoved == null || m_lastMoved.distance( e.getPoint() ) > MINIMUM_MOUSE_DISTANCE )
      if( getActualWidget() != null )
      {
        m_lastMoved = e.getPoint();
        getActualWidget().moved( m_lastMoved );

        m_MapPanel.repaint();
      }
  }

  // MouseMotionAdapter:
  public void mouseDragged( MouseEvent e )
  {
    if( m_lastDragged == null || m_lastDragged.distance( e.getPoint() ) > MINIMUM_MOUSE_DISTANCE )

      if( getActualWidget() != null )
      {
        m_lastDragged = e.getPoint();
        getActualWidget().dragged( m_lastDragged );
        m_MapPanel.repaint();
      }

  }

  public void mouseEntered( MouseEvent e )
  {
    //
  }

  public void mouseExited( MouseEvent e )
  {
    //
  }

  public void mousePressed( MouseEvent e )
  {
    final IWidget actualWidget = getActualWidget();
    if( actualWidget == null )
      return;
    if( e.isPopupTrigger() )
      actualWidget.clickPopup( e.getPoint() );
    else
      switch( e.getButton() )
      {
        case MouseEvent.BUTTON1:
          actualWidget.leftPressed( e.getPoint() );
          break;

        case MouseEvent.BUTTON2:
          actualWidget.middlePressed( e.getPoint() );
          break;

        case MouseEvent.BUTTON3:
          actualWidget.rightPressed( e.getPoint() );
          break;

        default:
          break;
      }
  }

  public void mouseReleased( MouseEvent e )
  {
    final IWidget actualWidget = getActualWidget();
    if( getActualWidget() == null )
      return;

    if( e.isPopupTrigger() )
      actualWidget.clickPopup( e.getPoint() );
    else
      switch( e.getButton() )
      {
        case MouseEvent.BUTTON1: // Left
          actualWidget.leftReleased( e.getPoint() );
          break;

        case MouseEvent.BUTTON2:
          actualWidget.middleReleased( e.getPoint() );
          break;

        case MouseEvent.BUTTON3: // Right
          actualWidget.perform();
          break;

        default:
          break;
      }
  }

  public void paintWidget( Graphics g )
  {
    if( getActualWidget() != null )
      getActualWidget().paint( g );
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
      m_actualWidget.activate( m_commandTarget, m_MapPanel );

    fireWidgetChangeEvent( newWidget );
  }

  public void addWidgetChangeListener( final IWidgetChangeListener listener )
  {
    m_widgetChangeListener.add( listener );
  }

  public void removeWidgetChangeListener( final IWidgetChangeListener listener )
  {
    m_widgetChangeListener.remove( listener );
  }

  private void fireWidgetChangeEvent( final IWidget newWidget )
  {
    final IWidgetChangeListener[] listener = m_widgetChangeListener.toArray( new IWidgetChangeListener[m_widgetChangeListener.size()] );
    for( int i = 0; i < listener.length; i++ )
      listener[i].widgetChanged( newWidget );
  }

  /**
   * @see java.awt.event.KeyListener#keyTyped(java.awt.event.KeyEvent)
   */
  public void keyTyped( KeyEvent e )
  {
    final IWidget widget = getActualWidget();
    if( widget != null )
      widget.keyTyped( e );
  }

  /**
   * @see java.awt.event.KeyListener#keyPressed(java.awt.event.KeyEvent)
   */
  public void keyPressed( KeyEvent e )
  {
    final IWidget widget = getActualWidget();
    if( widget != null )
      widget.keyPressed( e );
  }

  /**
   * @see java.awt.event.KeyListener#keyReleased(java.awt.event.KeyEvent)
   */
  public void keyReleased( KeyEvent e )
  {
    final IWidget widget = getActualWidget();
    if( widget != null )
      widget.keyReleased( e );
  }

}