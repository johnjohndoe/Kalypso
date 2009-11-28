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
package org.kalypso.model.product.application;

import org.eclipse.jface.action.GroupMarker;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.actions.ContributionItemFactory;
import org.eclipse.ui.application.ActionBarAdvisor;
import org.eclipse.ui.application.IActionBarConfigurer;
import org.eclipse.ui.internal.ide.IDEWorkbenchMessages;

/**
 * @author Gernot Belger
 */
@SuppressWarnings("restriction")
public class KalypsoModelActionBarAdvisor extends ActionBarAdvisor
{
  private final IWorkbenchWindow m_window;

  public KalypsoModelActionBarAdvisor( final IActionBarConfigurer configurer )
  {
    super( configurer );
    m_window = configurer.getWindowConfigurer().getWindow();
  }

  /**
   * Returns the window to which this action builder is contributing.
   */
  private IWorkbenchWindow getWindow( )
  {
    return m_window;
  }

  @Override
  protected void makeActions( final IWorkbenchWindow window )
  {
    register( ActionFactory.PREFERENCES.create( window ) );
    register( ActionFactory.INTRO.create( window ) );
    register( ActionFactory.HELP_CONTENTS.create( window ) );
    register( ActionFactory.HELP_SEARCH.create( window ) );
    register( ActionFactory.ABOUT.create( window ) );
    register( ActionFactory.RESET_PERSPECTIVE.create( window ) );
  }

  @Override
  protected void fillMenuBar( final IMenuManager menuBar )
  {
    menuBar.add( createWindowMenu() );
    menuBar.add( createHelpMenu() );
  }

  /**
   * Creates and returns the Window menu.
   */
  private MenuManager createWindowMenu( )
  {
    final MenuManager menu = new MenuManager( IDEWorkbenchMessages.Workbench_window, IWorkbenchActionConstants.M_WINDOW );

    // menu.add( newWindowAction );
    // menu.add( newEditorAction );
    // menu.add( new Separator() );
    addPerspectiveActions( menu );
    menu.add( new Separator() );
    // addKeyboardShortcuts( menu );
    // menu.add( new Separator( IWorkbenchActionConstants.MB_ADDITIONS ) );
    // menu.add( new Separator( IWorkbenchActionConstants.MB_ADDITIONS + "end" ) ); //$NON-NLS-1$
    menu.add( getAction( ActionFactory.PREFERENCES.getId() ) );

    // menu.add( ContributionItemFactory.OPEN_WINDOWS.create( getWindow() ) );
    return menu;
  }

  /**
   * Adds the perspective actions to the specified menu.
   */
  private void addPerspectiveActions( final MenuManager menu )
  {
    // {
    // String openText = IDEWorkbenchMessages.Workbench_openPerspective;
    // MenuManager changePerspMenuMgr = new MenuManager( openText, "openPerspective" ); //$NON-NLS-1$
    // IContributionItem changePerspMenuItem = ContributionItemFactory.PERSPECTIVES_SHORTLIST.create( getWindow() );
    // changePerspMenuMgr.add( changePerspMenuItem );
    // menu.add( changePerspMenuMgr );
    // }
    {
      final MenuManager showViewMenuMgr = new MenuManager( IDEWorkbenchMessages.Workbench_showView, "showView" ); //$NON-NLS-1$
      final IContributionItem showViewMenu = ContributionItemFactory.VIEWS_SHORTLIST.create( getWindow() );
      showViewMenuMgr.add( showViewMenu );
      menu.add( showViewMenuMgr );
    }

    menu.add( new Separator() );
    // menu.add( editActionSetAction );
    // menu.add( savePerspectiveAction );
    menu.add( getAction( ActionFactory.RESET_PERSPECTIVE.getId() ) );
    // menu.add( closePerspAction );
    // menu.add( closeAllPerspsAction );
  }

  private MenuManager createHelpMenu( )
  {
    final MenuManager menu = new MenuManager( IDEWorkbenchMessages.Workbench_help, IWorkbenchActionConstants.M_HELP );

    // MenuManager menu = new MenuManager(IDEWorkbenchMessages.Workbench_help, IWorkbenchActionConstants.M_HELP);
    addSeparatorOrGroupMarker( menu, "group.intro" ); //$NON-NLS-1$

    // See if a welcome or intro page is specified
    final IAction introAction = getAction( ActionFactory.INTRO.getId() );
    if( introAction != null )
      menu.add( introAction );
    // else if (quickStartAction != null)
    // menu.add(quickStartAction);
    menu.add( new GroupMarker( "group.intro.ext" ) ); //$NON-NLS-1$
    addSeparatorOrGroupMarker( menu, "group.main" ); //$NON-NLS-1$
    menu.add( getAction( ActionFactory.HELP_CONTENTS.getId() ) );
    menu.add( getAction( ActionFactory.HELP_SEARCH.getId() ) );
    // menu.add(dynamicHelpAction);
    addSeparatorOrGroupMarker( menu, "group.assist" ); //$NON-NLS-1$
    // See if a tips and tricks page is specified
    // if (tipsAndTricksAction != null)
    // menu.add(tipsAndTricksAction);
    // HELP_START should really be the first item, but it was after
    // quickStartAction and tipsAndTricksAction in 2.1.
    menu.add( new GroupMarker( IWorkbenchActionConstants.HELP_START ) );
    menu.add( new GroupMarker( "group.main.ext" ) ); //$NON-NLS-1$
    addSeparatorOrGroupMarker( menu, "group.tutorials" ); //$NON-NLS-1$
    addSeparatorOrGroupMarker( menu, "group.tools" ); //$NON-NLS-1$
    addSeparatorOrGroupMarker( menu, "group.updates" ); //$NON-NLS-1$
    menu.add( new GroupMarker( IWorkbenchActionConstants.HELP_END ) );
    addSeparatorOrGroupMarker( menu, IWorkbenchActionConstants.MB_ADDITIONS );
    // about should always be at the bottom
    menu.add( new Separator( "group.about" ) ); //$NON-NLS-1$
    menu.add( getAction( ActionFactory.ABOUT.getId() ) );
    menu.add( new GroupMarker( "group.about.ext" ) ); //$NON-NLS-1$

    return menu;
  }

  /**
   * Adds a <code>GroupMarker</code> or <code>Separator</code> to a menu. The test for whether a separator should be
   * added is done by checking for the existence of a preference matching the string useSeparator.MENUID.GROUPID that is
   * set to <code>true</code>.
   * 
   * @param menu
   *            the menu to add to
   * @param groupId
   *            the group id for the added separator or group marker
   */
  private void addSeparatorOrGroupMarker( final MenuManager menu, final String groupId )
  {
    // String prefId = "useSeparator." + menu.getId() + "." + groupId; //$NON-NLS-1$ //$NON-NLS-2$
    // boolean addExtraSeparators = IDEWorkbenchPlugin.getDefault()
    // .getPreferenceStore().getBoolean(prefId);
    // if (addExtraSeparators) {
    menu.add( new Separator( groupId ) );
    // } else {
    // menu.add(new GroupMarker(groupId));
    // }
  }
}
