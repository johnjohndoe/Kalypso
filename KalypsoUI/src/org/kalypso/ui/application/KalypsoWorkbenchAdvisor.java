package org.kalypso.ui.application;

import org.eclipse.jface.action.IMenuManager;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.application.IActionBarConfigurer;
import org.eclipse.ui.application.IWorkbenchConfigurer;
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
      // TODO: alle verbieten oder nur die die wir benötigen
      final IActionSetDescriptor desc = array[nX];
      desc.setInitiallyVisible( false );
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
    
    // Projekt Menu
    menuManager.remove( IWorkbenchActionConstants.M_PROJECT );
    
    // Windows Menü
    final IMenuManager windowMenu = (IMenuManager)menuManager.find( IWorkbenchActionConstants.M_WINDOW );
    windowMenu.remove( "preferences" );    
  }
  
}
