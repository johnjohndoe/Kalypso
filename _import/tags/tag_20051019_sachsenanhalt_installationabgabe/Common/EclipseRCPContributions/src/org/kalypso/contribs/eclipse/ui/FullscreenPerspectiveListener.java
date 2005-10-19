/*--------------- Kalypso-Header ------------------------------------------

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

 --------------------------------------------------------------------------*/

package org.kalypso.contribs.eclipse.ui;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.jface.action.CoolBarManager;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.IContributionManager;
import org.eclipse.ui.IPerspectiveDescriptor;
import org.eclipse.ui.IPerspectiveListener;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.internal.WorkbenchWindow;

/**
 * This Listeners provides full-screen mode for a single perspective.
 * <p>What is does is this: if the 'full-screen' perspective is shown, this listeners hides menu and toolbars of the main window.</p>
 * <p>If another perspective is shown, it recreates these items</p>
 * 
 * @author belger
 */
public class FullscreenPerspectiveListener implements IPerspectiveListener
{
  private final boolean m_hideToolbars;
  private final boolean m_hideMenu;
  private final IPerspectiveDescriptor m_descriptor;

  /** Controbution-Manager -> Items */
  private Map m_itemMap = new HashMap();

  public FullscreenPerspectiveListener( final IPerspectiveDescriptor descriptor, final boolean hideMenu,
      final boolean hideToolbars )
  {
    m_descriptor = descriptor;
    m_hideMenu = hideMenu;
    m_hideToolbars = hideToolbars;
  }

  /**
   * @see org.eclipse.ui.IPerspectiveListener#perspectiveActivated(org.eclipse.ui.IWorkbenchPage,
   *      org.eclipse.ui.IPerspectiveDescriptor)
   */
  public void perspectiveActivated( final IWorkbenchPage page, final IPerspectiveDescriptor perspective )
  {
    if( m_descriptor.equals( perspective ) )
      hideBars( page );
    else
      showBars( page );
  }

  private void showBars( final IWorkbenchPage page )
  {
    for( final Iterator mapIt = m_itemMap.entrySet().iterator(); mapIt.hasNext(); )
    {
      final Map.Entry entry = (Entry)mapIt.next();
      // prepare for exception
      mapIt.remove();
      final IContributionManager manager = (IContributionManager)entry.getKey();
      final IContributionItem[] items = (IContributionItem[])entry.getValue();

      for( int i = 0; i < items.length; i++ )
        manager.add( items[i] );

      manager.update( true );
    }

    // should be empty
    m_itemMap.clear();
    
    // extra refresh for the coolbar manager, if not, we get layout problems
    final WorkbenchWindow window = (WorkbenchWindow)page.getWorkbenchWindow();
    final CoolBarManager coolBarManager = window.getCoolBarManager();
    if( coolBarManager != null )
      coolBarManager.update( true );
  }

  private void hideBars( final IWorkbenchPage page )
  {
    final WorkbenchWindow window = (WorkbenchWindow)page.getWorkbenchWindow();

    if( m_hideMenu )
      clearManager( window.getMenuManager() );

    if( m_hideToolbars )
    {
      clearManager( window.getToolBarManager() );
      clearManager( window.getCoolBarManager() );
    }
  }

  private void clearManager( final IContributionManager manager )
  {
    if( manager != null )
    {
      m_itemMap.put( manager, manager.getItems() );
      manager.removeAll();
      manager.update( true );
    }
  }

  /**
   * @see org.eclipse.ui.IPerspectiveListener#perspectiveChanged(org.eclipse.ui.IWorkbenchPage,
   *      org.eclipse.ui.IPerspectiveDescriptor, java.lang.String)
   */
  public void perspectiveChanged( final IWorkbenchPage page, final IPerspectiveDescriptor perspective,
      final String changeId )
  {}
}
