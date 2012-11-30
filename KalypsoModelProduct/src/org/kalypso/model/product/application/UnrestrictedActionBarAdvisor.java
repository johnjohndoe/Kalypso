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

import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.GroupMarker;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.util.Util;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.actions.ActionFactory.IWorkbenchAction;
import org.eclipse.ui.actions.ContributionItemFactory;
import org.eclipse.ui.actions.NewWizardMenu;
import org.eclipse.ui.application.ActionBarAdvisor;
import org.eclipse.ui.application.IActionBarConfigurer;
import org.eclipse.ui.ide.IDEActionFactory;
import org.eclipse.ui.internal.ide.IDEWorkbenchMessages;
import org.eclipse.ui.internal.ide.actions.QuickMenuAction;

/**
 * An action bar advisor is responsible for creating, adding, and disposing of the actions added to a workbench window.
 * Each window will be populated with new actions.
 * 
 * @author Holger Albert
 */
@SuppressWarnings( "restriction" )
public class UnrestrictedActionBarAdvisor extends ActionBarAdvisor
{
  /*
   * Actions - important to allocate these only in makeActions, and then use them in the fill methods. This ensures that
   * the actions aren't recreated when fillActionBars is called with FILL_PROXY.
   */
// private IContributionItem m_newWizardShortlistContribution;

  private IWorkbenchAction m_closeAction;

  private IWorkbenchAction m_closeAllAction;

  private IWorkbenchAction m_saveAction;

  private IWorkbenchAction m_saveAsAction;

  private IWorkbenchAction m_saveAllAction;

  private IWorkbenchAction m_printAction;

  private IWorkbenchAction m_renameAction;

  private IWorkbenchAction m_refreshAction;

  private IWorkbenchAction m_openWorkspaceAction;

  private IWorkbenchAction m_propertiesAction;

  private IWorkbenchAction m_quitAction;

  private IWorkbenchAction m_undoAction;

  private IWorkbenchAction m_redoAction;

  private IWorkbenchAction m_cutAction;

  private IWorkbenchAction m_copyAction;

  private IWorkbenchAction m_pasteAction;

  private IWorkbenchAction m_deleteAction;

  private IWorkbenchAction m_selectAllAction;

  private IWorkbenchAction m_findAction;

  private IContributionItem m_perspectivesShortlistContribution;

  private IContributionItem m_viewShortListContribution;

  private IWorkbenchAction m_savePerspectiveAction;

  private IWorkbenchAction m_resetPerspectiveAction;

  private IWorkbenchAction m_closePerspectiveAction;

  private IWorkbenchAction m_closeAllPerspectivesAction;

  private IWorkbenchAction m_preferencesAction;

  private IWorkbenchAction m_aboutAction;

  private IWorkbenchAction m_helpContentsAction;

  private IWorkbenchAction m_introAction;

  private IWorkbenchAction m_newWizardAction;

  private IWorkbenchAction m_importResourcesAction;

  private IWorkbenchAction m_exportResourcesAction;

  // contribution items
  // @issue should obtain from ContributionItemFactory
  private NewWizardMenu newWizardMenu;

  private QuickMenuAction newQuickMenu;

  public UnrestrictedActionBarAdvisor( final IActionBarConfigurer configurer )
  {
    super( configurer );
  }

  private IWorkbenchWindow getWindow( )
  {
    return getActionBarConfigurer().getWindowConfigurer().getWindow();
  }

  /**
   * @see org.eclipse.ui.application.ActionBarAdvisor#makeActions(org.eclipse.ui.IWorkbenchWindow)
   */
  @Override
  protected void makeActions( final IWorkbenchWindow window )
  {
    /*
     * Creates the actions and registers them. Registering is needed to ensure that key bindings work. The corresponding
     * commands keybindings are defined in the plugin.xml file. Registering also provides automatic disposal of the
     * actions when the window is closed.
     */

    /* Actions and ContributionItems for the file menu. */
    // FIXME: calling this here causes dead-locks in workbench-startup...
    // The eclipse code however is the same, so it is probably an eclipse bug...
    m_newWizardAction = ActionFactory.NEW.create( window );
    register( m_newWizardAction );

    m_closeAction = ActionFactory.CLOSE.create( window );
    register( m_closeAction );

    m_closeAllAction = ActionFactory.CLOSE_ALL.create( window );
    register( m_closeAllAction );

    m_saveAction = ActionFactory.SAVE.create( window );
    register( m_saveAction );

    m_saveAsAction = ActionFactory.SAVE_AS.create( window );
    register( m_saveAsAction );

    m_saveAllAction = ActionFactory.SAVE_ALL.create( window );
    register( m_saveAllAction );

    m_printAction = ActionFactory.PRINT.create( window );
    register( m_printAction );

    m_renameAction = ActionFactory.RENAME.create( window );
    register( m_renameAction );

    m_refreshAction = ActionFactory.REFRESH.create( window );
    register( m_refreshAction );

    m_openWorkspaceAction = IDEActionFactory.OPEN_WORKSPACE.create( window );
    register( m_openWorkspaceAction );

    m_importResourcesAction = ActionFactory.IMPORT.create( window );
    register( m_importResourcesAction );

    m_exportResourcesAction = ActionFactory.EXPORT.create( window );
    register( m_exportResourcesAction );

    m_propertiesAction = ActionFactory.PROPERTIES.create( window );

    m_quitAction = ActionFactory.QUIT.create( window );
    register( m_quitAction );

    /* Actions and ContributionItems for the edit menu. */
    m_undoAction = ActionFactory.UNDO.create( window );
    register( m_undoAction );

    m_redoAction = ActionFactory.REDO.create( window );
    register( m_redoAction );

    m_cutAction = ActionFactory.CUT.create( window );
    register( m_cutAction );

    m_copyAction = ActionFactory.COPY.create( window );
    register( m_copyAction );

    m_pasteAction = ActionFactory.PASTE.create( window );
    register( m_pasteAction );

    m_deleteAction = ActionFactory.DELETE.create( window );
    register( m_deleteAction );

    m_selectAllAction = ActionFactory.SELECT_ALL.create( window );
    register( m_selectAllAction );

    m_findAction = ActionFactory.FIND.create( window );
    register( m_findAction );

    /* Actions and ContributionItems for the window menu. */
    m_perspectivesShortlistContribution = ContributionItemFactory.PERSPECTIVES_SHORTLIST.create( window );
    m_viewShortListContribution = ContributionItemFactory.VIEWS_SHORTLIST.create( window );

    m_savePerspectiveAction = ActionFactory.SAVE_PERSPECTIVE.create( window );
    register( m_savePerspectiveAction );

    m_resetPerspectiveAction = ActionFactory.RESET_PERSPECTIVE.create( window );
    register( m_resetPerspectiveAction );

    m_closePerspectiveAction = ActionFactory.CLOSE_PERSPECTIVE.create( window );
    register( m_closePerspectiveAction );

    m_closeAllPerspectivesAction = ActionFactory.CLOSE_ALL_PERSPECTIVES.create( window );
    register( m_closeAllPerspectivesAction );

    m_preferencesAction = ActionFactory.PREFERENCES.create( window );
    register( m_preferencesAction );

    /* Actions and ContributionItems for the help menu. */
    m_introAction = ActionFactory.INTRO.create( window );
    register( m_introAction );

    m_helpContentsAction = ActionFactory.HELP_CONTENTS.create( window );
    register( m_helpContentsAction );

    m_aboutAction = ActionFactory.ABOUT.create( window );
    register( m_aboutAction );

    final String newQuickMenuId = "org.eclipse.ui.file.newQuickMenu"; //$NON-NLS-1$
    newQuickMenu = new QuickMenuAction( newQuickMenuId )
    {
      @Override
      protected void fillMenu( final IMenuManager menu )
      {
        menu.add( new NewWizardMenu( window ) );
      }
    };
    register( newQuickMenu );
  }

  @Override
  protected void fillMenuBar( final IMenuManager menuBar )
  {
    menuBar.add( createFileMenu() );
    final MenuManager editMenu = new MenuManager( IDEWorkbenchMessages.Workbench_edit, IWorkbenchActionConstants.M_EDIT );
    final MenuManager windowMenu = new MenuManager( IDEWorkbenchMessages.Workbench_window, IWorkbenchActionConstants.M_WINDOW );
    final MenuManager openPerspectiveMenu = new MenuManager( IDEWorkbenchMessages.Workbench_openPerspective, "openPerspective" ); //$NON-NLS-1$
    final MenuManager showViewMenu = new MenuManager( IDEWorkbenchMessages.Workbench_showView, "showView" ); //$NON-NLS-1$
    final MenuManager helpMenu = new MenuManager( IDEWorkbenchMessages.Workbench_help, IWorkbenchActionConstants.M_HELP );

    /* The edit menu. */
    menuBar.add( editMenu );
    editMenu.add( new GroupMarker( IWorkbenchActionConstants.EDIT_START ) );
    editMenu.add( m_undoAction );
    editMenu.add( m_redoAction );
    editMenu.add( new GroupMarker( IWorkbenchActionConstants.UNDO_EXT ) );
    editMenu.add( new Separator() );
    editMenu.add( m_cutAction );
    editMenu.add( m_copyAction );
    editMenu.add( m_pasteAction );
    editMenu.add( new GroupMarker( IWorkbenchActionConstants.CUT_EXT ) );
    editMenu.add( new Separator() );
    editMenu.add( m_deleteAction );
    editMenu.add( m_selectAllAction );
    editMenu.add( new Separator() );
    editMenu.add( m_findAction );
    editMenu.add( new GroupMarker( IWorkbenchActionConstants.FIND_EXT ) );
    editMenu.add( new GroupMarker( IWorkbenchActionConstants.EDIT_END ) );

    /* The window menu. */
    menuBar.add( windowMenu );
    windowMenu.add( openPerspectiveMenu );
    openPerspectiveMenu.add( m_perspectivesShortlistContribution );
    windowMenu.add( showViewMenu );
    showViewMenu.add( m_viewShortListContribution );
    windowMenu.add( new Separator() );
    windowMenu.add( m_savePerspectiveAction );
    windowMenu.add( m_resetPerspectiveAction );
    windowMenu.add( m_closePerspectiveAction );
    windowMenu.add( m_closeAllPerspectivesAction );
    windowMenu.add( new GroupMarker( IWorkbenchActionConstants.VIEW_EXT ) );
    windowMenu.add( new Separator() );
    windowMenu.add( new GroupMarker( IWorkbenchActionConstants.WINDOW_EXT ) );
    windowMenu.add( new Separator() );
    windowMenu.add( m_preferencesAction );

    /* Action set menus will appear here. */
    menuBar.add( new GroupMarker( IWorkbenchActionConstants.MB_ADDITIONS ) );

    /* The help menu. */
    menuBar.add( helpMenu );
    helpMenu.add( new GroupMarker( IWorkbenchActionConstants.HELP_START ) );
    helpMenu.add( m_introAction ); //
    helpMenu.add( new Separator( "group.main.ext" ) ); //$NON-NLS-1$
    helpMenu.add( m_helpContentsAction );
    helpMenu.add( new GroupMarker( "group.tutorials" ) ); //$NON-NLS-1$
    helpMenu.add( new GroupMarker( "group.tools" ) ); //$NON-NLS-1$
    helpMenu.add( new Separator( "group.updates" ) ); //$NON-NLS-1$
    helpMenu.add( new GroupMarker( IWorkbenchActionConstants.HELP_END ) );
    helpMenu.add( new GroupMarker( IWorkbenchActionConstants.MB_ADDITIONS ) );
    // about should always be at the bottom
    helpMenu.add( new Separator( "group.about" ) ); //$NON-NLS-1$

    final ActionContributionItem aboutItem = new ActionContributionItem( m_aboutAction );
    aboutItem.setVisible( !Util.isMac() );
    helpMenu.add( m_aboutAction );
    helpMenu.add( new GroupMarker( "group.about.ext" ) ); //$NON-NLS-1$
  }

  /**
   * Creates and returns the File menu.
   */
  private MenuManager createFileMenu( )
  {
    final MenuManager menu = new MenuManager( IDEWorkbenchMessages.Workbench_file, IWorkbenchActionConstants.M_FILE );

    menu.add( new GroupMarker( IWorkbenchActionConstants.FILE_START ) );

    // create the New submenu, using the same id for it as the New action
    {
      final String newText = IDEWorkbenchMessages.Workbench_new;
      final String newId = ActionFactory.NEW.getId();
      final MenuManager newMenu = new MenuManager( newText, newId );
      newMenu.setActionDefinitionId( "org.eclipse.ui.file.newQuickMenu" ); //$NON-NLS-1$
      newMenu.add( new Separator( newId ) );
      this.newWizardMenu = new NewWizardMenu( getWindow() );
      newMenu.add( this.newWizardMenu );
      newMenu.add( new Separator( IWorkbenchActionConstants.MB_ADDITIONS ) );
      menu.add( newMenu );
    }

    menu.add( new GroupMarker( IWorkbenchActionConstants.NEW_EXT ) );
    menu.add( new Separator() );

    menu.add( m_closeAction );
    menu.add( m_closeAllAction );
    menu.add( new GroupMarker( IWorkbenchActionConstants.CLOSE_EXT ) );
    menu.add( new Separator() );

    menu.add( m_saveAction );
    menu.add( m_saveAsAction );
    menu.add( m_saveAllAction );
    // menu.add( getRevertItem() );
    menu.add( new Separator() );
    // menu.add( getMoveItem() );
    menu.add( m_renameAction );
    menu.add( m_refreshAction );
    menu.add( new GroupMarker( IWorkbenchActionConstants.SAVE_EXT ) );
    menu.add( new Separator() );

    menu.add( m_printAction );
    menu.add( new GroupMarker( IWorkbenchActionConstants.PRINT_EXT ) );
    menu.add( new Separator() );

    menu.add( m_openWorkspaceAction );
    menu.add( new GroupMarker( IWorkbenchActionConstants.OPEN_EXT ) );
    menu.add( new Separator() );
    menu.add( m_importResourcesAction );
    menu.add( m_exportResourcesAction );
    menu.add( new GroupMarker( IWorkbenchActionConstants.IMPORT_EXT ) );
    menu.add( new Separator( IWorkbenchActionConstants.MB_ADDITIONS ) );

    menu.add( new Separator() );
    menu.add( m_propertiesAction );

    menu.add( ContributionItemFactory.REOPEN_EDITORS.create( getWindow() ) );
    menu.add( new GroupMarker( IWorkbenchActionConstants.MRU ) );
    menu.add( new Separator() );

    // If we're on OS X we shouldn't show this command in the File menu. It
    // should be invisible to the user. However, we should not remove it -
    // the carbon UI code will do a search through our menu structure
    // looking for it when Cmd-Q is invoked (or Quit is chosen from the
    // application menu.
    final ActionContributionItem quitItem = new ActionContributionItem( m_quitAction );
    quitItem.setVisible( !Util.isMac() );
    menu.add( quitItem );
    menu.add( new GroupMarker( IWorkbenchActionConstants.FILE_END ) );
    return menu;
  }
}