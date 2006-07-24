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
package org.kalypso.ui.editor.mapeditor;

import java.awt.Frame;

import org.eclipse.jface.action.GroupMarker;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.IWorkbenchPartSite;
import org.kalypso.contribs.eclipse.swt.events.SWTAWT_ContextMenuMouseAdapter;
import org.kalypso.ogc.gml.map.MapPanel;

/**
 * Helper class for {@link org.eclipse.ui.IWorkbenchPart}s, which show a map.
 * 
 * @author Gernot Belger
 */
public class MapPartHelper
{
  private MapPartHelper( )
  {
    // will not be instantiated
  }

  public static Control createMapPanelPartControl( final Composite parent, final MapPanel mapPanel, final IWorkbenchPartSite site )
  {
    final Composite composite = new Composite( parent, SWT.RIGHT | SWT.EMBEDDED );
    // create MapPanel
    final Frame virtualFrame = SWT_AWT.new_Frame( composite );
    virtualFrame.setVisible( true );
    mapPanel.setVisible( true );
    virtualFrame.add( mapPanel );

    // create Context Menu
    final MenuManager menuManager = new MenuManager();
    menuManager.setRemoveAllWhenShown( true );
    menuManager.addMenuListener( new IMenuListener()
    {
      public void menuAboutToShow( IMenuManager manager )
      {
        manager.add( new GroupMarker( IWorkbenchActionConstants.MB_ADDITIONS ) );
        manager.add( new Separator() );
      }
    } );
    final Menu mapMenu = menuManager.createContextMenu( composite );
    composite.setMenu( mapMenu );
    // register it
    site.registerContextMenu( menuManager, mapPanel );
    site.setSelectionProvider( mapPanel );

    mapPanel.addMouseListener( new SWTAWT_ContextMenuMouseAdapter( composite, mapMenu ) );

    return composite;
  }

}
