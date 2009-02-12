/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.contribs.eclipse.swt.events;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Menu;

/**
 * AWT Mouseadapter that wraps Events to show SWT ContextMenu
 * 
 * @author doemming
 */
public class SWTAWT_ContextMenuMouseAdapter extends MouseAdapter
{
  final Menu m_mapMenu;

  final Composite m_swtComposite;

  /**
   * @param mapMenu
   *          menu to show
   * @param swtComposite
   *          parent composite that embeddes the awt thing
   */
  public SWTAWT_ContextMenuMouseAdapter( final Composite swtComposite, final Menu mapMenu )
  {
    super();
    m_swtComposite = swtComposite;
    m_mapMenu = mapMenu;
  }

  private void contextMenuAboutToShow( final MouseEvent e )
  {
    if( e.isPopupTrigger() && m_swtComposite != null && !m_swtComposite.isDisposed() )
    {
      m_mapMenu.getDisplay().asyncExec( new Runnable()
      {
        public void run()
        {
          final Event event = new Event();
          final Point point = m_swtComposite.toDisplay( e.getX(), e.getY() );
          event.x = point.x;
          event.y = point.y;
          m_swtComposite.notifyListeners( SWT.MenuDetect, event );

          if( event.doit )
          {
            if( m_mapMenu != null && !m_mapMenu.isDisposed() )
            {
              m_mapMenu.setLocation( event.x, event.y );
              m_mapMenu.setVisible( true );
            }
          }
        }
      } );
    }
  }

  public void mouseClicked( MouseEvent e )
  {
    contextMenuAboutToShow( e );
  }

  public void mousePressed( MouseEvent e )
  {
    contextMenuAboutToShow( e );
  }

  public void mouseReleased( final MouseEvent e )
  {
    contextMenuAboutToShow( e );
  }

}
