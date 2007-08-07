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
package org.kalypso.ui.views.map;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.ui.IPageListener;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IWindowListener;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;

/**
 * @author kuch
 */
public class MapCoordinateWindowListener implements IWindowListener
{
  Map<IWorkbenchWindow, IPageListener> m_pageListeners = new HashMap<IWorkbenchWindow, IPageListener>();

  Map<IWorkbenchPage, IPartListener> m_partListeners = new HashMap<IWorkbenchPage, IPartListener>();

  protected final MapCoordinateStatusLineItem m_mapitem;

  public class MapCoordinaltePartListener implements IPartListener
  {
    /**
     * @see org.eclipse.ui.IPartListener#partActivated(org.eclipse.ui.IWorkbenchPart)
     */
    public void partActivated( final IWorkbenchPart part )
    {
      if( part instanceof MapView )
        m_mapitem.setEnabled( ((MapView) part).getMapPanel(), true );
    }

    /**
     * @see org.eclipse.ui.IPartListener#partBroughtToTop(org.eclipse.ui.IWorkbenchPart)
     */
    public void partBroughtToTop( final IWorkbenchPart part )
    {
    }

    /**
     * @see org.eclipse.ui.IPartListener#partClosed(org.eclipse.ui.IWorkbenchPart)
     */
    public void partClosed( final IWorkbenchPart part )
    {
      if( part instanceof MapView )
        m_mapitem.setEnabled( ((MapView) part).getMapPanel(), false );
    }

    /**
     * @see org.eclipse.ui.IPartListener#partDeactivated(org.eclipse.ui.IWorkbenchPart)
     */
    public void partDeactivated( final IWorkbenchPart part )
    {
    }

    /**
     * @see org.eclipse.ui.IPartListener#partOpened(org.eclipse.ui.IWorkbenchPart)
     */
    public void partOpened( final IWorkbenchPart part )
    {
    }

  }

  public MapCoordinateWindowListener( final MapCoordinateStatusLineItem mapitem )
  {
    m_mapitem = mapitem;
  }

  /**
   * @see org.eclipse.ui.IWindowListener#windowActivated(org.eclipse.ui.IWorkbenchWindow)
   */
  public void windowActivated( final IWorkbenchWindow window )
  {
  }

  /**
   * @see org.eclipse.ui.IWindowListener#windowClosed(org.eclipse.ui.IWorkbenchWindow)
   */
  public void windowClosed( final IWorkbenchWindow window )
  {
    final IPageListener listener = m_pageListeners.remove( window );

    if( listener != null )
      window.removePageListener( listener );
  }

  /**
   * @see org.eclipse.ui.IWindowListener#windowDeactivated(org.eclipse.ui.IWorkbenchWindow)
   */
  public void windowDeactivated( final IWorkbenchWindow window )
  {
  }

  /**
   * @see org.eclipse.ui.IWindowListener#windowOpened(org.eclipse.ui.IWorkbenchWindow)
   */
  public void windowOpened( final IWorkbenchWindow window )
  {
    final IPageListener listener = m_pageListeners.get( window );
    if( listener == null )
    {
      final IPageListener pageListener = new IPageListener()
      {

        public void pageActivated( IWorkbenchPage page )
        {
        }

        public void pageClosed( IWorkbenchPage page )
        {
          IPartListener pl = m_partListeners.remove( page );

          if( pl != null )
            page.removePartListener( pl );
        }

        public void pageOpened( IWorkbenchPage page )
        {
          IPartListener partListener = m_partListeners.get( page );
          if( partListener == null )
          {
            partListener = new MapCoordinaltePartListener();
            page.addPartListener( partListener );
            m_partListeners.put( page, partListener );
          }
        }
      };
      window.addPageListener( pageListener );
      m_pageListeners.put( window, pageListener );
    }

    /* register new part listener for active page */
    final IWorkbenchPage page = window.getActivePage();

    final MapCoordinaltePartListener partListener = new MapCoordinaltePartListener();
    page.addPartListener( partListener );
    m_partListeners.put( page, partListener );
  }
}
