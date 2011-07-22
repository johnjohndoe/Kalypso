/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.wspm.tuhh.ui.light;

import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.IWorkbenchPartSite;
import org.eclipse.ui.menus.IMenuService;
import org.kalypso.ui.editor.gmleditor.part.GmvViewPart;

/**
 * Shows the local data of the PDB: a single fixed wspm project.
 * 
 * @author Gernot Belger
 */
public class WspmGmvViewPart extends GmvViewPart
{
  public static final String ID = "org.kalypso.model.wspm.tuhh.ui.light.WspmGmvViewPart"; //$NON-NLS-1$

  private MenuManager m_menuManager;

  /**
   * Made public in order to be called by PDB.
   * 
   * @see org.eclipse.ui.part.WorkbenchPart#setPartName(java.lang.String)
   */
  @Override
  public void setPartName( final String partName )
  {
    super.setPartName( partName );
  }

  @Override
  public void dispose( )
  {
    final IWorkbenchPartSite site = getSite();
    final IMenuService service = (IMenuService) site.getService( IMenuService.class );
    service.releaseContributions( m_menuManager );

    super.dispose();
  }

  @Override
  protected void registerContextMenu( final MenuManager menuManager )
  {
    m_menuManager = menuManager;

    final IWorkbenchPartSite site = getSite();
    final IMenuService service = (IMenuService) site.getService( IMenuService.class );

    /* Hide any popup stuff */
    menuManager.setRemoveAllWhenShown( false );

    // add additions seperator: if not, eclipse whines
    menuManager.add( new Separator( IWorkbenchActionConstants.MB_ADDITIONS ) );

    final String uri = String.format( "popup:%s", ID );
    service.populateContributionManager( menuManager, uri );
  }

  @Override
  protected void handleMenuAboutToShow( final IMenuManager manager )
  {
    // No 'New' menu
    // super.handleMenuAboutToShow( manager );
  }
}