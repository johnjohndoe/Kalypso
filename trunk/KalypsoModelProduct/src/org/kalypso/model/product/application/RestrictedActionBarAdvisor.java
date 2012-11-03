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
import org.eclipse.ui.application.ActionBarAdvisor;
import org.eclipse.ui.application.IActionBarConfigurer;
import org.eclipse.ui.internal.ide.IDEWorkbenchMessages;

/**
 * An action bar advisor is responsible for creating, adding, and disposing of the actions added to a workbench window.
 * Each window will be populated with new actions.
 * 
 * @author Holger Albert
 */
@SuppressWarnings( "restriction" )
public class RestrictedActionBarAdvisor extends ActionBarAdvisor
{
  /*
   * Actions - important to allocate these only in makeActions, and then use them in the fill methods. This ensures that
   * the actions aren't recreated when fillActionBars is called with FILL_PROXY.
   */
  // private IContributionItem m_newWizardShortlistContribution;

  // private IWorkbenchAction m_closeAction;

  // private IWorkbenchAction m_closeAllAction;

  // private IWorkbenchAction m_saveAction;

  // private IWorkbenchAction m_saveAsAction;

  // private IWorkbenchAction m_saveAllAction;

  // private IWorkbenchAction m_printAction;

  // private IWorkbenchAction m_propertiesAction;

  // private IContributionItem m_reopenEditorsContribution;

  // private IWorkbenchAction m_exitAction;

  // private IWorkbenchAction m_undoAction;

  // private IWorkbenchAction m_redoAction;

  // private IWorkbenchAction m_cutAction;

  // private IWorkbenchAction m_copyAction;

  // private IWorkbenchAction m_pasteAction;

  // private IWorkbenchAction m_deleteAction;

  // private IWorkbenchAction m_selectAllAction;

  // private IWorkbenchAction m_findAction;

  // private IContributionItem m_perspectivesShortlistContribution;

  // FIXME: it would be nice to get rid of the 'Other Items' but not the whole ShowView menu...
  private IContributionItem m_viewShortListContribution;

  // private IWorkbenchAction m_savePerspectiveAction;

  private IWorkbenchAction m_resetPerspectiveAction;

  // private IWorkbenchAction m_closePerspectiveAction;

  // private IWorkbenchAction m_closeAllPerspectivesAction;

  private IWorkbenchAction m_preferencesAction;

  private IWorkbenchAction m_aboutAction;

  private IWorkbenchAction m_helpContentsAction;

  private IWorkbenchAction m_introAction;

  private final boolean m_introHidden;

  /**
   * The constructor.
   */
  public RestrictedActionBarAdvisor( final IActionBarConfigurer configurer )
  {
    super( configurer );

    m_introHidden = KalypsoModelApplication.getIntroBehavior() == IntroBehavior.never;
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
    // m_newWizardShortlistContribution = ContributionItemFactory.NEW_WIZARD_SHORTLIST.create( window );
    // m_closeAction = ActionFactory.CLOSE.create( window );
    // m_closeAllAction = ActionFactory.CLOSE_ALL.create( window );
    // m_saveAction = ActionFactory.SAVE.create( window );
    // m_saveAsAction = ActionFactory.SAVE_AS.create( window );
    // m_saveAllAction = ActionFactory.SAVE_ALL.create( window );
    // m_printAction = ActionFactory.PRINT.create( window );
    // m_propertiesAction = ActionFactory.PROPERTIES.create( window );
    // m_reopenEditorsContribution = ContributionItemFactory.REOPEN_EDITORS.create( window );
    // m_exitAction = ActionFactory.QUIT.create( window );

    // register( m_closeAction );
    // register( m_closeAllAction );
    // register( m_saveAction );
    // register( m_saveAsAction );
    // register( m_saveAllAction );
    // register( m_printAction );
    // register( m_propertiesAction );
    // register( m_exitAction );

    /* Actions and ContributionItems for the edit menu. */
    // m_undoAction = ActionFactory.UNDO.create( window );
    // m_redoAction = ActionFactory.REDO.create( window );
    // m_cutAction = ActionFactory.CUT.create( window );
    // m_copyAction = ActionFactory.COPY.create( window );
    // m_pasteAction = ActionFactory.PASTE.create( window );
    // m_deleteAction = ActionFactory.DELETE.create( window );
    // m_selectAllAction = ActionFactory.SELECT_ALL.create( window );
    // m_findAction = ActionFactory.FIND.create( window );

    // register( m_undoAction );
    // register( m_redoAction );
    // register( m_cutAction );
    // register( m_copyAction );
    // register( m_pasteAction );
    // register( m_deleteAction );
    // register( m_selectAllAction );
    // register( m_findAction );

    /* Actions and ContributionItems for the window menu. */
    // m_perspectivesShortlistContribution = ContributionItemFactory.PERSPECTIVES_SHORTLIST.create( window );
    m_viewShortListContribution = ContributionItemFactory.VIEWS_SHORTLIST.create( window );
    // m_savePerspectiveAction = ActionFactory.SAVE_PERSPECTIVE.create( window );
    m_resetPerspectiveAction = ActionFactory.RESET_PERSPECTIVE.create( window );
    // m_closePerspectiveAction = ActionFactory.CLOSE_PERSPECTIVE.create( window );
    // m_closeAllPerspectivesAction = ActionFactory.CLOSE_ALL_PERSPECTIVES.create( window );
    m_preferencesAction = ActionFactory.PREFERENCES.create( window );

    // register( m_savePerspectiveAction );
    register( m_resetPerspectiveAction );
    // register( m_closePerspectiveAction );
    // register( m_closeAllPerspectivesAction );
    register( m_preferencesAction );

    /* Actions and ContributionItems for the help menu. */
    if( !m_introHidden )
    {
      m_introAction = ActionFactory.INTRO.create( window );
      register( m_introAction );
    }

    m_helpContentsAction = ActionFactory.HELP_CONTENTS.create( window );
    m_aboutAction = ActionFactory.ABOUT.create( window );

    register( m_helpContentsAction );
    register( m_aboutAction );
  }

  /**
   * @see org.eclipse.ui.application.ActionBarAdvisor#fillMenuBar(org.eclipse.jface.action.IMenuManager)
   */
  @Override
  protected void fillMenuBar( final IMenuManager menuBar )
  {
    // MenuManager fileMenu = new MenuManager( IDEWorkbenchMessages.Workbench_file, IWorkbenchActionConstants.M_FILE );
    // MenuManager newMenu = new MenuManager( IDEWorkbenchMessages.Workbench_new, ActionFactory.NEW.getId() );
    // MenuManager editMenu = new MenuManager( IDEWorkbenchMessages.Workbench_edit, IWorkbenchActionConstants.M_EDIT );
    final MenuManager windowMenu = new MenuManager( IDEWorkbenchMessages.Workbench_window, IWorkbenchActionConstants.M_WINDOW );
    // MenuManager openPerspectiveMenu = new MenuManager( IDEWorkbenchMessages.Workbench_openPerspective,
    // "openPerspective" );
    final MenuManager showViewMenu = new MenuManager( IDEWorkbenchMessages.Workbench_showView, "showView" ); //$NON-NLS-1$
    final MenuManager helpMenu = new MenuManager( IDEWorkbenchMessages.Workbench_help, IWorkbenchActionConstants.M_HELP );

    /* The file menu. */
    // menuBar.add( fileMenu );
    // fileMenu.add( new GroupMarker( IWorkbenchActionConstants.FILE_START ) );
    // fileMenu.add( newMenu );
    // newMenu.add( m_newWizardShortlistContribution );
    // fileMenu.add( new GroupMarker( IWorkbenchActionConstants.NEW_EXT ) );
    // fileMenu.add( new Separator() );
    // fileMenu.add( m_closeAction );
    // fileMenu.add( m_closeAllAction );
    // fileMenu.add( new GroupMarker( IWorkbenchActionConstants.CLOSE_EXT ) );
    // fileMenu.add( new Separator() );
    // fileMenu.add( m_saveAction );
    // fileMenu.add( m_saveAsAction );
    // fileMenu.add( m_saveAllAction );
    // fileMenu.add( new GroupMarker( IWorkbenchActionConstants.SAVE_EXT ) );
    // fileMenu.add( new Separator() );
    // fileMenu.add( m_printAction );
    // fileMenu.add( new GroupMarker( IWorkbenchActionConstants.PRINT_EXT ) );
    // fileMenu.add( new Separator() );
    // fileMenu.add( m_propertiesAction );
    // fileMenu.add( m_reopenEditorsContribution );
    // fileMenu.add( new Separator() );
    // fileMenu.add( m_exitAction );
    // fileMenu.add( new GroupMarker( IWorkbenchActionConstants.FILE_END ) );

    /* The edit menu. */
    // menuBar.add( editMenu );
    // editMenu.add( new GroupMarker( IWorkbenchActionConstants.EDIT_START ) );
    // editMenu.add( m_undoAction );
    // editMenu.add( m_redoAction );
    // editMenu.add( new GroupMarker( IWorkbenchActionConstants.UNDO_EXT ) );
    // editMenu.add( new Separator() );
    // editMenu.add( m_cutAction );
    // editMenu.add( m_copyAction );
    // editMenu.add( m_pasteAction );
    // editMenu.add( new GroupMarker( IWorkbenchActionConstants.CUT_EXT ) );
    // editMenu.add( new Separator() );
    // editMenu.add( m_deleteAction );
    // editMenu.add( m_selectAllAction );
    // editMenu.add( new Separator() );
    // editMenu.add( m_findAction );
    // editMenu.add( new GroupMarker( IWorkbenchActionConstants.FIND_EXT ) );
    // editMenu.add( new GroupMarker( IWorkbenchActionConstants.EDIT_END ) );

    /* The window menu. */
    menuBar.add( windowMenu );
    // windowMenu.add( openPerspectiveMenu );
    // openPerspectiveMenu.add( m_perspectivesShortlistContribution );
    windowMenu.add( showViewMenu );
    showViewMenu.add( m_viewShortListContribution );
    windowMenu.add( new Separator() );
    // windowMenu.add( m_savePerspectiveAction );
    windowMenu.add( m_resetPerspectiveAction );
    // windowMenu.add( m_closePerspectiveAction );
    // windowMenu.add( m_closeAllPerspectivesAction );
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
    if( !m_introHidden )
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
}