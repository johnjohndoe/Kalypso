package org.kalypso.portal.intro;

import org.eclipse.jface.action.GroupMarker;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.ICoolBarManager;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.action.ToolBarContributionItem;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.ui.IPageListener;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.actions.ContributionItemFactory;
import org.eclipse.ui.actions.NewWizardMenu;
import org.eclipse.ui.actions.ActionFactory.IWorkbenchAction;
import org.eclipse.ui.application.ActionBarAdvisor;
import org.eclipse.ui.application.IActionBarConfigurer;
import org.eclipse.ui.ide.IDEActionFactory;
import org.eclipse.ui.ide.IIDEActionConstants;
import org.eclipse.ui.internal.IPreferenceConstants;
import org.eclipse.ui.internal.WorkbenchPlugin;
import org.eclipse.ui.internal.ide.IDEInternalWorkbenchImages;
import org.eclipse.ui.internal.ide.IDEWorkbenchMessages;
import org.eclipse.ui.internal.ide.actions.QuickMenuAction;
import org.eclipse.ui.internal.util.StatusLineContributionItem;

/**
 * This class is based on the WorkbenchActionBuilder. Some of the menue items and coolbar itmes are not added to the
 * workbench this is a crude way to customize the KalypsoPortal product.
 * 
 * @see org.eclipse.ui.internal.ide.WorkbenchActionBuilder some of the menue items and coolbar itmes are not added to
 *      the workbench this is a crude way to customize the KalypsoPortal product.
 * @author kuepfer
 */
public class KalypsoPlanerClientActionBuilder extends ActionBarAdvisor
{

  IWorkbenchWindow m_window;

  private IWorkbenchAction m_introAction;

  private NewWizardMenu m_newWizardMenu;

  private QuickMenuAction m_showInQuickMenu;

  private IWorkbenchAction m_closeAction;

  private IWorkbenchAction m_closeAllAction;

  private IWorkbenchAction m_saveAction;

  private IWorkbenchAction m_saveAsAction;

  private IWorkbenchAction m_saveAllAction;

  private IWorkbenchAction m_moveAction;

  private IWorkbenchAction m_renameAction;

  private IWorkbenchAction m_refreshAction;

  private IWorkbenchAction m_printAction;

  private IWorkbenchAction m_openWorkspaceAction;

  private IWorkbenchAction m_importResourcesAction;

  private IWorkbenchAction m_exportResourcesAction;

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

  private IWorkbenchAction m_addBookmarkAction;

  private IWorkbenchAction m_addTaskAction;

  private IWorkbenchAction m_goIntoAction;

  private IWorkbenchAction m_backAction;

  private IWorkbenchAction m_forwardAction;

  private IWorkbenchAction m_upAction;

  private IWorkbenchAction m_nextAction;

  private IWorkbenchAction m_previousAction;

  private IWorkbenchAction m_backwardHistoryAction;

  private IWorkbenchAction m_forwardHistoryAction;

  private IWorkbenchAction m_openProjectAction;

  private IWorkbenchAction m_closeProjectAction;

  private IWorkbenchAction m_projectPropertyDialogAction;

  private IAction m_editActionSetAction;

  private IAction m_savePerspectiveAction;

  private IAction m_resetPerspectiveAction;

  private IAction m_closePerspAction;

  private IAction m_closeAllPerspsAction;

  private IAction m_newWindowAction;

  private IAction m_newEditorAction;

  private IAction m_openPreferencesAction;

  private QuickMenuAction m_newQuickMenu;

  private IContributionItem m_pinEditorContributionItem;

  private StatusLineContributionItem m_statusLineItem;

  private IWorkbenchAction m_newWizardAction;

  private IWorkbenchAction m_newWizardDropDownAction;

  private IWorkbenchAction m_closeAllSavedAction;

  private IWorkbenchAction m_aboutAction;

  private IPropertyChangeListener m_propPrefListener;

  private boolean m_isDisposed;

  public KalypsoPlanerClientActionBuilder( IActionBarConfigurer configurer )
  {
    super( configurer );
    m_window = configurer.getWindowConfigurer().getWindow();
  }

  private IWorkbenchWindow getWindow( )
  {
    return m_window;
  }

  /**
   * Create all standard actions for this product workbench
   */
  @Override
  protected void makeActions( final IWorkbenchWindow window )
  {
    // @issue should obtain from ConfigurationItemFactory
    m_statusLineItem = new StatusLineContributionItem( "ModeContributionItem" ); //$NON-NLS-1$

    m_newWizardAction = ActionFactory.NEW.create( window );
    register( m_newWizardAction );

    m_newWizardDropDownAction = IDEActionFactory.NEW_WIZARD_DROP_DOWN.create( window );
    register( m_newWizardDropDownAction );

    m_importResourcesAction = ActionFactory.IMPORT.create( window );
    register( m_importResourcesAction );

    m_exportResourcesAction = ActionFactory.EXPORT.create( window );
    register( m_exportResourcesAction );

    m_saveAction = ActionFactory.SAVE.create( window );
    register( m_saveAction );

    m_saveAsAction = ActionFactory.SAVE_AS.create( window );
    register( m_saveAsAction );

    m_saveAllAction = ActionFactory.SAVE_ALL.create( window );
    register( m_saveAllAction );

    m_newWindowAction = ActionFactory.OPEN_NEW_WINDOW.create( getWindow() );
    m_newWindowAction.setText( IDEWorkbenchMessages.Workbench_openNewWindow );
    register( m_newWindowAction );

    m_newEditorAction = ActionFactory.NEW_EDITOR.create( window );
    register( m_newEditorAction );

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

    m_printAction = ActionFactory.PRINT.create( window );
    register( m_printAction );

    m_selectAllAction = ActionFactory.SELECT_ALL.create( window );
    register( m_selectAllAction );

    m_findAction = ActionFactory.FIND.create( window );
    register( m_findAction );

    m_closeAction = ActionFactory.CLOSE.create( window );
    register( m_closeAction );

    m_closeAllAction = ActionFactory.CLOSE_ALL.create( window );
    register( m_closeAllAction );

    m_closeAllSavedAction = ActionFactory.CLOSE_ALL_SAVED.create( window );
    register( m_closeAllSavedAction );

    m_aboutAction = ActionFactory.ABOUT.create( window );
    m_aboutAction.setImageDescriptor( IDEInternalWorkbenchImages.getImageDescriptor( IDEInternalWorkbenchImages.IMG_OBJS_DEFAULT_PROD ) );
    register( m_aboutAction );

    m_openPreferencesAction = ActionFactory.PREFERENCES.create( window );
    register( m_openPreferencesAction );

    m_addBookmarkAction = IDEActionFactory.BOOKMARK.create( window );
    register( m_addBookmarkAction );

    m_addTaskAction = IDEActionFactory.ADD_TASK.create( window );
    register( m_addTaskAction );

    m_deleteAction = ActionFactory.DELETE.create( window );
    register( m_deleteAction );

    m_savePerspectiveAction = ActionFactory.SAVE_PERSPECTIVE.create( window );
    register( m_savePerspectiveAction );
    m_editActionSetAction = ActionFactory.EDIT_ACTION_SETS.create( window );
    register( m_editActionSetAction );

    m_resetPerspectiveAction = ActionFactory.RESET_PERSPECTIVE.create( window );
    register( m_resetPerspectiveAction );
    m_closePerspAction = ActionFactory.CLOSE_PERSPECTIVE.create( window );
    register( m_closePerspAction );
    m_closeAllPerspsAction = ActionFactory.CLOSE_ALL_PERSPECTIVES.create( window );
    register( m_closeAllPerspsAction );

    m_forwardHistoryAction = ActionFactory.FORWARD_HISTORY.create( window );
    register( m_forwardHistoryAction );

    m_backwardHistoryAction = ActionFactory.BACKWARD_HISTORY.create( window );
    register( m_backwardHistoryAction );

    m_refreshAction = ActionFactory.REFRESH.create( window );
    register( m_refreshAction );

    m_propertiesAction = ActionFactory.PROPERTIES.create( window );
    register( m_propertiesAction );

    m_quitAction = ActionFactory.QUIT.create( window );
    register( m_quitAction );

    m_moveAction = ActionFactory.MOVE.create( window );
    register( m_moveAction );

    m_renameAction = ActionFactory.RENAME.create( window );
    register( m_renameAction );

    m_goIntoAction = ActionFactory.GO_INTO.create( window );
    register( m_goIntoAction );

    m_backAction = ActionFactory.BACK.create( window );
    register( m_backAction );

    m_forwardAction = ActionFactory.FORWARD.create( window );
    register( m_forwardAction );

    m_upAction = ActionFactory.UP.create( window );
    register( m_upAction );

    m_nextAction = ActionFactory.NEXT.create( window );
    m_nextAction.setImageDescriptor( IDEInternalWorkbenchImages.getImageDescriptor( IDEInternalWorkbenchImages.IMG_ETOOL_NEXT_NAV ) );
    register( m_nextAction );

    m_previousAction = ActionFactory.PREVIOUS.create( window );
    m_previousAction.setImageDescriptor( IDEInternalWorkbenchImages.getImageDescriptor( IDEInternalWorkbenchImages.IMG_ETOOL_PREVIOUS_NAV ) );
    register( m_previousAction );

    m_openProjectAction = IDEActionFactory.OPEN_PROJECT.create( window );
    register( m_openProjectAction );

    m_closeProjectAction = IDEActionFactory.CLOSE_PROJECT.create( window );
    register( m_closeProjectAction );

    m_openWorkspaceAction = IDEActionFactory.OPEN_WORKSPACE.create( window );
    register( m_openWorkspaceAction );

    m_projectPropertyDialogAction = IDEActionFactory.OPEN_PROJECT_PROPERTIES.create( window );
    register( m_projectPropertyDialogAction );

    if( window.getWorkbench().getIntroManager().hasIntro() )
    {
      m_introAction = ActionFactory.INTRO.create( window );
      register( m_introAction );
    }

    String showInQuickMenuId = "org.eclipse.ui.navigate.showInQuickMenu"; //$NON-NLS-1$
    m_showInQuickMenu = new QuickMenuAction( showInQuickMenuId )
    {
      @Override
      protected void fillMenu( IMenuManager menu )
      {
        menu.add( ContributionItemFactory.VIEWS_SHOW_IN.create( window ) );
      }
    };
    register( m_showInQuickMenu );

    final String newQuickMenuId = "org.eclipse.ui.file.newQuickMenu"; //$NON-NLS-1$
    m_newQuickMenu = new QuickMenuAction( newQuickMenuId )
    {
      @Override
      protected void fillMenu( IMenuManager menu )
      {
        menu.add( new NewWizardMenu( window ) );
      }
    };
    register( m_newQuickMenu );

    m_pinEditorContributionItem = ContributionItemFactory.PIN_EDITOR.create( window );

  }

  void updateModeLine( final String text )
  {
    m_statusLineItem.setText( text );
  }

  /**
   * generates all standard menus for this product in the menu bar.
   */
  @Override
  protected void fillMenuBar( IMenuManager menuBar )
  {
    menuBar.add( createFileMenu() );
    menuBar.add( createEditMenu() );
    menuBar.add( createNavigateMenu() );
    menuBar.add( createProjectMenu() );

    menuBar.add( new GroupMarker( IWorkbenchActionConstants.MB_ADDITIONS ) );
    menuBar.add( createWindowMenu() );
    // menuBar.add(createHelpMenu());
    MenuManager helpMenuManager = new MenuManager( "&Help", IWorkbenchActionConstants.M_HELP );
    helpMenuManager.add( m_introAction );
    helpMenuManager.add( new Separator( "group.about" ) ); //$NON-NLS-1$
    helpMenuManager.add( m_aboutAction );
    menuBar.add( helpMenuManager );

  }

  /**
   * Creates and returns the Window menu.
   */
  private MenuManager createWindowMenu( )
  {
    MenuManager menu = new MenuManager( IDEWorkbenchMessages.Workbench_window, IWorkbenchActionConstants.M_WINDOW );

    menu.add( m_newWindowAction );
    menu.add( m_newEditorAction );
    menu.add( new Separator() );
    addPerspectiveActions( menu );
    menu.add( new Separator() );
    // addKeyboardShortcuts(menu);
    menu.add( new Separator( IWorkbenchActionConstants.MB_ADDITIONS ) );
    menu.add( new Separator( IWorkbenchActionConstants.MB_ADDITIONS + "end" ) ); //$NON-NLS-1$
    menu.add( m_openPreferencesAction );

    menu.add( ContributionItemFactory.OPEN_WINDOWS.create( getWindow() ) );
    return menu;
  }

  /**
   * Creates and returns the Project menu.
   */
  private MenuManager createProjectMenu( )
  {
    MenuManager menu = new MenuManager( IDEWorkbenchMessages.Workbench_project, IWorkbenchActionConstants.M_PROJECT );
    menu.add( new Separator( IWorkbenchActionConstants.PROJ_START ) );

    menu.add( m_openProjectAction );
    menu.add( m_closeProjectAction );
    menu.add( new GroupMarker( IWorkbenchActionConstants.OPEN_EXT ) );
    menu.add( new Separator() );
    menu.add( new GroupMarker( IWorkbenchActionConstants.BUILD_EXT ) );
    menu.add( new Separator() );

    menu.add( new GroupMarker( IWorkbenchActionConstants.MB_ADDITIONS ) );
    menu.add( new GroupMarker( IWorkbenchActionConstants.PROJ_END ) );
    menu.add( new Separator() );
    menu.add( m_projectPropertyDialogAction );
    return menu;
  }

  /**
   * Creates and returns the Navigate menu.
   */
  @SuppressWarnings("synthetic-access")
  private MenuManager createNavigateMenu( )
  {
    MenuManager menu = new MenuManager( IDEWorkbenchMessages.Workbench_navigate, IWorkbenchActionConstants.M_NAVIGATE );
    menu.add( new GroupMarker( IWorkbenchActionConstants.NAV_START ) );
    menu.add( m_goIntoAction );

    MenuManager goToSubMenu = new MenuManager( IDEWorkbenchMessages.Workbench_goTo, IWorkbenchActionConstants.GO_TO );
    menu.add( goToSubMenu );
    goToSubMenu.add( m_backAction );
    goToSubMenu.add( m_forwardAction );
    goToSubMenu.add( m_upAction );
    goToSubMenu.add( new Separator( IWorkbenchActionConstants.MB_ADDITIONS ) );

    menu.add( new Separator( IWorkbenchActionConstants.OPEN_EXT ) );
    for( int i = 2; i < 5; ++i )
    {
      menu.add( new Separator( IWorkbenchActionConstants.OPEN_EXT + i ) );
    }
    menu.add( new Separator( IWorkbenchActionConstants.SHOW_EXT ) );
    {

      MenuManager showInSubMenu = new MenuManager( IDEWorkbenchMessages.Workbench_showIn, "showIn" ) { //$NON-NLS-1$
        @Override
        public String getMenuText( )
        {
          String result = super.getMenuText();
          if( m_showInQuickMenu == null )
            return null;
          String shortCut = m_showInQuickMenu.getShortCutString();
          if( shortCut == null )
            return result;
          return result + "\t" + shortCut; //$NON-NLS-1$
        }
      };
      showInSubMenu.add( ContributionItemFactory.VIEWS_SHOW_IN.create( getWindow() ) );
      menu.add( showInSubMenu );
    }
    for( int i = 2; i < 5; ++i )
    {
      menu.add( new Separator( IWorkbenchActionConstants.SHOW_EXT + i ) );
    }
    menu.add( new Separator() );
    menu.add( m_nextAction );
    menu.add( m_previousAction );
    menu.add( new Separator( IWorkbenchActionConstants.MB_ADDITIONS ) );
    menu.add( new GroupMarker( IWorkbenchActionConstants.NAV_END ) );

    // TBD: Location of this actions
    menu.add( new Separator() );
    menu.add( m_backwardHistoryAction );
    menu.add( m_forwardHistoryAction );
    return menu;
  }

  /**
   * Creates and returns the Edit menu.
   */
  private MenuManager createEditMenu( )
  {
    MenuManager menu = new MenuManager( IDEWorkbenchMessages.Workbench_edit, IWorkbenchActionConstants.M_EDIT );
    menu.add( new GroupMarker( IWorkbenchActionConstants.EDIT_START ) );

    menu.add( m_undoAction );
    menu.add( m_redoAction );
    menu.add( new GroupMarker( IWorkbenchActionConstants.UNDO_EXT ) );
    menu.add( new Separator() );

    menu.add( m_cutAction );
    menu.add( m_copyAction );
    menu.add( m_pasteAction );
    menu.add( new GroupMarker( IWorkbenchActionConstants.CUT_EXT ) );
    menu.add( new Separator() );

    menu.add( m_deleteAction );
    menu.add( m_selectAllAction );
    menu.add( new Separator() );

    menu.add( m_findAction );
    menu.add( new GroupMarker( IWorkbenchActionConstants.FIND_EXT ) );
    menu.add( new Separator() );

    menu.add( m_addBookmarkAction );
    menu.add( m_addTaskAction );
    menu.add( new GroupMarker( IWorkbenchActionConstants.ADD_EXT ) );

    menu.add( new GroupMarker( IWorkbenchActionConstants.EDIT_END ) );
    menu.add( new Separator( IWorkbenchActionConstants.MB_ADDITIONS ) );
    return menu;
  }

  /**
   * Creates and returns the File menu.
   */
  private MenuManager createFileMenu( )
  {
    MenuManager menu = new MenuManager( IDEWorkbenchMessages.Workbench_file, IWorkbenchActionConstants.M_FILE );
    menu.add( new GroupMarker( IWorkbenchActionConstants.FILE_START ) );

    // create the New submenu, using the same id for it as the New action
    String newText = IDEWorkbenchMessages.Workbench_new;
    String newId = ActionFactory.NEW.getId();
    MenuManager newMenu = new MenuManager( newText, newId )
    { //$NON-NLS-1$

      @Override
      public String getMenuText( )
      {
        String result = super.getMenuText();
        if( m_showInQuickMenu == null )
          return null;
        String shortCut = m_showInQuickMenu.getShortCutString();
        if( shortCut == null )
          return result;
        return result + "\t" + shortCut; //$NON-NLS-1$
      }
    };
    newMenu.add( new Separator( newId ) );
    m_newWizardMenu = new NewWizardMenu( getWindow() );
    newMenu.add( m_newWizardMenu );
    newMenu.add( new Separator( IWorkbenchActionConstants.MB_ADDITIONS ) );
    menu.add( newMenu );
    menu.add( new GroupMarker( IWorkbenchActionConstants.NEW_EXT ) );
    menu.add( new Separator() );
    // close actions
    menu.add( m_closeAction );
    menu.add( m_closeAllAction );
    menu.add( new GroupMarker( IWorkbenchActionConstants.CLOSE_EXT ) );
    menu.add( new Separator() );
    // save actions
    menu.add( m_saveAction );
    menu.add( m_saveAsAction );
    menu.add( m_saveAllAction );
    menu.add( new Separator() );
    menu.add( m_moveAction );
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
    menu.add( m_quitAction );
    menu.add( new GroupMarker( IWorkbenchActionConstants.FILE_END ) );
    return menu;
  }

  /**
   * Adds the perspective actions to the specified menu.
   */
  private void addPerspectiveActions( MenuManager menu )
  {
    {
      String openText = IDEWorkbenchMessages.Workbench_openPerspective;
      MenuManager changePerspMenuMgr = new MenuManager( openText, "openPerspective" ); //$NON-NLS-1$
      IContributionItem changePerspMenuItem = ContributionItemFactory.PERSPECTIVES_SHORTLIST.create( getWindow() );
      changePerspMenuMgr.add( changePerspMenuItem );
      menu.add( changePerspMenuMgr );
    }
    {
      MenuManager showViewMenuMgr = new MenuManager( IDEWorkbenchMessages.Workbench_showView, "showView" ); //$NON-NLS-1$
      IContributionItem showViewMenu = ContributionItemFactory.VIEWS_SHORTLIST.create( getWindow() );
      showViewMenuMgr.add( showViewMenu );
      menu.add( showViewMenuMgr );
    }
    menu.add( new Separator() );
    menu.add( m_editActionSetAction );
    menu.add( m_savePerspectiveAction );
    menu.add( m_resetPerspectiveAction );
    menu.add( m_closePerspAction );
    menu.add( m_closeAllPerspsAction );
  }

  /**
   * Returns true if the menu with the given ID should be considered as an OLE container menu. Container menus are
   * preserved in OLE menu merging.
   */
  @Override
  public boolean isApplicationMenu( String menuId )
  {
    if( menuId.equals( IWorkbenchActionConstants.M_FILE ) )
      return true;
    if( menuId.equals( IWorkbenchActionConstants.M_WINDOW ) )
      return true;
    return false;
  }

  /**
   * Fills the coolbar with the workbench actions.
   */
  @Override
  protected void fillCoolBar( ICoolBarManager coolBar )
  {

    { // Set up the context Menu
      IMenuManager popUpMenu = new MenuManager();
      coolBar.setContextMenuManager( popUpMenu );
    }
    coolBar.add( new GroupMarker( IIDEActionConstants.GROUP_FILE ) );
    { // File Group
      IToolBarManager fileToolBar = new ToolBarManager( coolBar.getStyle() );
      fileToolBar.add( new Separator( IWorkbenchActionConstants.NEW_GROUP ) );
      fileToolBar.add( m_newWizardDropDownAction );
      fileToolBar.add( new GroupMarker( IWorkbenchActionConstants.NEW_EXT ) );
      fileToolBar.add( new GroupMarker( IWorkbenchActionConstants.SAVE_GROUP ) );
      fileToolBar.add( m_saveAction );
      fileToolBar.add( new GroupMarker( IWorkbenchActionConstants.SAVE_EXT ) );
      fileToolBar.add( m_printAction );
      fileToolBar.add( new GroupMarker( IWorkbenchActionConstants.PRINT_EXT ) );

      fileToolBar.add( new Separator( IWorkbenchActionConstants.MB_ADDITIONS ) );

      // Add to the cool bar manager
      coolBar.add( new ToolBarContributionItem( fileToolBar, IWorkbenchActionConstants.TOOLBAR_FILE ) );
    }

    coolBar.add( new GroupMarker( IWorkbenchActionConstants.MB_ADDITIONS ) );

    coolBar.add( new GroupMarker( IIDEActionConstants.GROUP_NAV ) );
    { // Navigate group
      IToolBarManager navToolBar = new ToolBarManager( coolBar.getStyle() );
      navToolBar.add( new Separator( IWorkbenchActionConstants.HISTORY_GROUP ) );
      navToolBar.add( new GroupMarker( IWorkbenchActionConstants.GROUP_APP ) );
      navToolBar.add( m_backwardHistoryAction );
      navToolBar.add( m_forwardHistoryAction );
      navToolBar.add( new Separator( IWorkbenchActionConstants.PIN_GROUP ) );
      navToolBar.add( m_pinEditorContributionItem );

      // Add to the cool bar manager
      coolBar.add( new ToolBarContributionItem( navToolBar, IWorkbenchActionConstants.TOOLBAR_NAVIGATE ) );
    }

    coolBar.add( new GroupMarker( IWorkbenchActionConstants.GROUP_EDITOR ) );

    coolBar.add( new GroupMarker( IWorkbenchActionConstants.GROUP_HELP ) ); //$NON-NLS-1$

    { // Help group
      IToolBarManager helpToolBar = new ToolBarManager( coolBar.getStyle() );
      helpToolBar.add( new Separator( IWorkbenchActionConstants.GROUP_HELP ) ); //$NON-NLS-1$
      // helpToolBar.add(searchComboItem);
      // Add the group for applications to contribute
      helpToolBar.add( new GroupMarker( IWorkbenchActionConstants.GROUP_APP ) );
      // Add to the cool bar manager
      coolBar.add( new ToolBarContributionItem( helpToolBar, IWorkbenchActionConstants.TOOLBAR_HELP ) ); //$NON-NLS-1$
    }

  }

  /**
   * Return whether or not given id matches the id of the coolitems that the workbench creates.
   */
  public boolean isWorkbenchCoolItemId( String id )
  {
    if( IWorkbenchActionConstants.TOOLBAR_FILE.equalsIgnoreCase( id ) )
      return true;
    if( IWorkbenchActionConstants.TOOLBAR_NAVIGATE.equalsIgnoreCase( id ) )
      return true;
    return false;
  }

  /**
   * Hooks listeners on the preference store.
   */
  private void hookListeners( )
  {

    // listener for the "close editors automatically"
    // preference change
    m_propPrefListener = new IPropertyChangeListener()
    {
      public void propertyChange( PropertyChangeEvent event )
      {
        if( event.getProperty().equals( IPreferenceConstants.REUSE_EDITORS_BOOLEAN ) )
        {
          if( m_window.getShell() != null && !m_window.getShell().isDisposed() )
          {
            // this property change notification could be from a non-ui thread
            m_window.getShell().getDisplay().syncExec( new Runnable()
            {
              public void run( )
              {
                updatePinActionToolbar();
              }
            } );
          }
        }
      }
    };
    /*
     * In order to ensure that the pin action toolbar sets its size correctly, the pin action should set its visiblity
     * before we call updatePinActionToolbar(). In other words we always want the PinActionContributionItem to be
     * notified before the WorkbenchActionBuilder.
     */
    WorkbenchPlugin.getDefault().getPreferenceStore().addPropertyChangeListener( m_propPrefListener );

  }

  /**
   * Fills the action bar and
   */
  @Override
  public void fillActionBars( int flags )
  {
    super.fillActionBars( flags );
    if( (flags & FILL_PROXY) == 0 )
    {
      hookListeners();
    }
  }

  /**
   * Update the pin action's tool bar
   */
  void updatePinActionToolbar( )
  {

    ICoolBarManager coolBarManager = getActionBarConfigurer().getCoolBarManager();
    IContributionItem cbItem = coolBarManager.find( IWorkbenchActionConstants.TOOLBAR_NAVIGATE );
    if( !(cbItem instanceof ToolBarContributionItem) )
    {
      // This should not happen
      System.out.println( "Navigation toolbar contribution item is missing" ); //$NON-NLS-1$
      return;
    }
    ToolBarContributionItem toolBarItem = (ToolBarContributionItem) cbItem;
    IToolBarManager toolBarManager = toolBarItem.getToolBarManager();
    if( toolBarManager == null )
    {
      // error if this happens, navigation toolbar assumed to always exist
      System.out.println( "Navigate toolbar is missing" ); //$NON-NLS-1$
      return;
    }

    toolBarManager.update( false );
    toolBarItem.update( ICoolBarManager.SIZE );
  }

  /**
   * Disposes any resources and unhooks any listeners that are no longer needed. Called when the window is closed.
   */
  @Override
  public void dispose( )
  {
    if( m_isDisposed )
      return;
    m_isDisposed = true;
    getActionBarConfigurer().getStatusLineManager().remove( m_statusLineItem );

    if( m_propPrefListener != null )
    {
      WorkbenchPlugin.getDefault().getPreferenceStore().removePropertyChangeListener( m_propPrefListener );
      m_propPrefListener = null;
    }

    m_pinEditorContributionItem.dispose();
    m_showInQuickMenu.dispose();
    m_newQuickMenu.dispose();

    // null out actions to make leak debugging easier
    m_closeAction = null;
    m_closeAllAction = null;
    m_closeAllSavedAction = null;
    m_saveAction = null;
    m_saveAllAction = null;
    m_newWindowAction = null;
    m_newEditorAction = null;
    m_aboutAction = null;
    m_openPreferencesAction = null;
    m_saveAsAction = null;
    m_savePerspectiveAction = null;
    m_resetPerspectiveAction = null;
    m_editActionSetAction = null;
    m_closePerspAction = null;
    m_closeAllPerspsAction = null;
    m_backwardHistoryAction = null;
    m_forwardHistoryAction = null;
    m_undoAction = null;
    m_redoAction = null;
    m_cutAction = null;
    m_copyAction = null;
    m_pasteAction = null;
    m_deleteAction = null;
    m_selectAllAction = null;
    m_findAction = null;
    m_printAction = null;
    m_refreshAction = null;
    m_propertiesAction = null;
    m_quitAction = null;
    m_moveAction = null;
    m_renameAction = null;
    m_goIntoAction = null;
    m_backAction = null;
    m_forwardAction = null;
    m_upAction = null;
    m_nextAction = null;
    m_previousAction = null;
    m_openWorkspaceAction = null;
    m_projectPropertyDialogAction = null;
    m_newWizardAction = null;
    m_newWizardDropDownAction = null;
    m_importResourcesAction = null;
    m_exportResourcesAction = null;
    m_showInQuickMenu = null;
    m_newQuickMenu = null;
    m_addBookmarkAction = null;
    m_addTaskAction = null;
    m_openProjectAction = null;
    m_closeProjectAction = null;
    m_newWizardMenu = null;
    m_pinEditorContributionItem = null;
    m_statusLineItem = null;
    m_propPrefListener = null;
    m_introAction = null;

    super.dispose();
  }
}
