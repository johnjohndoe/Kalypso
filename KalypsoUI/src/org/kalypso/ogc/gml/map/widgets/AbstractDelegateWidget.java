/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 * 
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 * 
 *  ---------------------------------------------------------------------------*/
package org.kalypso.ogc.gml.map.widgets;

import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.KeyEvent;

import org.eclipse.jface.viewers.ISelection;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.widgets.IWidget;

/**
 * @author Thomas Jung
 */
public class AbstractDelegateWidget extends AbstractWidget
{
  private IWidget m_delegate = null;

  public AbstractDelegateWidget( final String name, final String tooltip, final IWidget delegate )
  {
    super( name, tooltip );

    m_delegate = delegate;
  }

  public IWidget getDelegate( )
  {
    return m_delegate;
  }

  public void setDelegate( final IWidget delegate )
  {
    m_delegate = delegate;
  }

  // TODO: check for null in all methods

  @Override
  public void activate( final ICommandTarget commandPoster, final IMapPanel mapPanel )
  {
    m_delegate.activate( commandPoster, mapPanel );

    super.activate( getCommandTarget(), mapPanel );
  }

  @Override
  public boolean canBeActivated( final ISelection selection, final IMapPanel mapPanel )
  {
    return m_delegate.canBeActivated( selection, mapPanel );
  }

  @Override
  public void clickPopup( final Point p )
  {
    m_delegate.clickPopup( p );
  }

  @Override
  public void doubleClickedLeft( final Point p )
  {
    m_delegate.doubleClickedLeft( p );
  }

  @Override
  public void doubleClickedRight( final Point p )
  {
    m_delegate.doubleClickedRight( p );
  }

  @Override
  public void dragged( final Point p )
  {
    m_delegate.dragged( p );
  }

  @Override
  public void finish( )
  {
    m_delegate.finish();
    super.finish();
  }

  @Override
  public String getName( )
  {
    return m_delegate.getName();
  }

  @Override
  public String getToolTip( )
  {
    return m_delegate.getToolTip();
  }

  @Override
  public void keyPressed( final KeyEvent e )
  {
    m_delegate.keyPressed( e );
  }

  @Override
  public void keyReleased( final KeyEvent e )
  {
    m_delegate.keyReleased( e );
  }

  @Override
  public void keyTyped( final KeyEvent e )
  {
    m_delegate.keyTyped( e );
  }

  @Override
  public void leftClicked( final Point p )
  {
    m_delegate.leftClicked( p );
  }

  @Override
  public void leftPressed( final Point p )
  {
    m_delegate.leftPressed( p );
  }

  @Override
  public void leftReleased( final Point p )
  {
    m_delegate.leftReleased( p );
  }

  @Override
  public void middleClicked( final Point p )
  {
    m_delegate.middleClicked( p );
  }

  @Override
  public void middlePressed( final Point p )
  {
    m_delegate.middlePressed( p );
  }

  @Override
  public void middleReleased( final Point p )
  {
    m_delegate.middleReleased( p );
  }

  @Override
  public void moved( final Point p )
  {
    m_delegate.moved( p );
  }

  @Override
  public void paint( final Graphics g )
  {
    m_delegate.paint( g );
  }

  @Override
  public void rightClicked( final Point p )
  {
    m_delegate.rightClicked( p );
  }

  @Override
  public void rightPressed( final Point p )
  {
    m_delegate.rightPressed( p );
  }

  @Override
  public void rightReleased( final Point p )
  {
    m_delegate.rightReleased( p );
  }

  @Override
  public void setSelection( final ISelection selection )
  {
    m_delegate.setSelection( selection );
  }

}
