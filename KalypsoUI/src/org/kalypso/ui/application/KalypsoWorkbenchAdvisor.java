package org.kalypso.ui.application;

import org.eclipse.jface.action.GroupMarker;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.application.IActionBarConfigurer;
import org.eclipse.ui.application.IWorkbenchConfigurer;
import org.eclipse.ui.application.IWorkbenchWindowConfigurer;
import org.eclipse.ui.internal.WorkbenchPlugin;
import org.eclipse.ui.internal.ide.IDEWorkbenchAdvisor;
import org.eclipse.ui.internal.registry.ActionSetRegistry;
import org.eclipse.ui.internal.registry.IActionSetDescriptor;
import org.kalypso.ui.IKalypsoUIConstants;

/**
 * @author belger
 */
public class KalypsoWorkbenchAdvisor extends IDEWorkbenchAdvisor
{
  public KalypsoWorkbenchAdvisor()
  {
    super();
  }
  
  /**
   * @see org.eclipse.ui.application.WorkbenchAdvisor#initialize(org.eclipse.ui.application.IWorkbenchConfigurer)
   */
  public void initialize( final IWorkbenchConfigurer configurer )
  {
    super.initialize( configurer );
    
    final ActionSetRegistry reg = WorkbenchPlugin.getDefault().getActionSetRegistry();
    final IActionSetDescriptor[] array = reg.getActionSets();
    final int count = array.length;
    for( int nX = 0; nX < count; nX++ )
    {
      final IActionSetDescriptor desc = array[nX];
      
      desc.setInitiallyVisible( desc.getId().startsWith( "org.kalypso" ) );
    }
  }
  
  /**
   * @see org.eclipse.ui.application.WorkbenchAdvisor#getInitialWindowPerspectiveId()
   */
  public String getInitialWindowPerspectiveId()
  {
    return IKalypsoUIConstants.MODELER_PERSPECTIVE;
  }
  
  /**
   * @see org.eclipse.ui.internal.ide.IDEWorkbenchAdvisor#fillActionBars(org.eclipse.ui.IWorkbenchWindow, org.eclipse.ui.application.IActionBarConfigurer, int)
   */
  public void fillActionBars( final IWorkbenchWindow window, final IActionBarConfigurer actionConfigurer,
      final int flags )
  {
    super.fillActionBars( window, actionConfigurer, flags );

    final IMenuManager menuManager = actionConfigurer.getMenuManager();
    
    // Menüs umbauen
    final IMenuManager windowMenu = (IMenuManager)menuManager.find( IWorkbenchActionConstants.M_WINDOW );
    final IMenuManager navigateMenu = (IMenuManager)windowMenu.find( "shortcuts" );

    menuManager.insertAfter( IWorkbenchActionConstants.M_EDIT, navigateMenu );

    // Menüs entfernen
    menuManager.remove( IWorkbenchActionConstants.M_PROJECT );
    menuManager.remove( IWorkbenchActionConstants.M_NAVIGATE );
    // TODO: auskommentieren
    //menuManager.remove( IWorkbenchActionConstants.M_WINDOW );
    
    // neue Menüs hinzufügen
    final IMenuManager modelMenu = new MenuManager( "Modell", "model" );
    modelMenu.add( new GroupMarker( "modelStart" ) );
    menuManager.insertBefore( IWorkbenchActionConstants.M_FILE, modelMenu );

    final IMenuManager calcMenu = new MenuManager( "Rechenfall", "calcCase" );
    modelMenu.add( new GroupMarker( "calcCaseStart" ) );
    menuManager.insertAfter( "model", calcMenu );

    final IMenuManager templateMenu = new MenuManager( "Vorlage", "template" );
    modelMenu.add(new GroupMarker( "teplateStart" ) );
    menuManager.insertAfter( "calcCase", templateMenu );
  }
  
  /**
   * @see org.eclipse.ui.internal.ide.IDEWorkbenchAdvisor#preWindowOpen(org.eclipse.ui.application.IWorkbenchWindowConfigurer)
   */
  public void preWindowOpen( final IWorkbenchWindowConfigurer windowConfigurer )
  {
    super.preWindowOpen( windowConfigurer );
    
    windowConfigurer.setShowPerspectiveBar( false );
  }
  
}
