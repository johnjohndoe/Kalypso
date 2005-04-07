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
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.util.command.ICommandTarget;

/**
 * Der Controller fuer die MapView
 * 
 * @author vdoemming
 */
public class WidgetManager implements MouseListener, MouseMotionListener
{
  private IWidget myNormalWidget = null;

  private final MapPanel myMapPanel;

  private final ICommandTarget m_commandTarget;

  private Point m_lastDragged = null;

  private static final double MINIMUM_MOUSE_DISTANCE = 5;

  private Point m_lastMoved = null;

  private final List m_widgetChangeListener = new ArrayList();

  public WidgetManager( final ICommandTarget commandTarget, final MapPanel mapPanel )
  {
    myMapPanel = mapPanel;
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
        actualWidget.leftClicked( e.getPoint() );
        break;

      case MouseEvent.BUTTON2:
        actualWidget.middleClicked( e.getPoint() );
        break;

      case MouseEvent.BUTTON3:
        actualWidget.rightClicked( e.getPoint() );
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

        myMapPanel.repaint();
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
        myMapPanel.repaint();
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

      case MouseEvent.BUTTON3: //Right
        actualWidget.perform();

        //       getActualWidget().rightReleased(e.getPoint());
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

  public IWidget getActualWidget()
  {
    return myNormalWidget;
  }

  public void changeWidget( final IWidget newWidget )
  {
    if( newWidget == null )
    {
      myNormalWidget = null;
      fireWidgetChangeEvent( newWidget );
      return;
    }

    if( myNormalWidget != null )// && normalWidget != newWidget )
      myNormalWidget.finish();

    myNormalWidget = newWidget;
    myNormalWidget.activate( m_commandTarget, myMapPanel );
    fireWidgetChangeEvent( newWidget );
  }

//  private void stopTemporaryWidget()
//  {
//    if( getActualWidget() != null )
//      changeWidget( getActualWidget() );
//  }

  public void add( IWidgetChangeListener listener )
  {
    m_widgetChangeListener.add( listener );
  }

  public void remove( IWidgetChangeListener listener )
  {
    m_widgetChangeListener.remove( listener );
  }

  private void fireWidgetChangeEvent( IWidget newWidget )
  {
    synchronized( m_widgetChangeListener )
    {
      for( Iterator iter = m_widgetChangeListener.iterator(); iter.hasNext(); )
      {
        IWidgetChangeListener listener = (IWidgetChangeListener)iter.next();
        listener.widgetChanged( newWidget );
      }
    }
  }
}