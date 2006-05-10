package org.kalypso.portal.intro;

import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.actions.ActionFactory.IWorkbenchAction;
import org.eclipse.ui.application.ActionBarAdvisor;
import org.eclipse.ui.application.IActionBarConfigurer;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.IWorkbenchWindow;

public class ApplicationActionBarAdvisor extends ActionBarAdvisor
{

  private IWorkbenchAction m_introAction;

  public ApplicationActionBarAdvisor( IActionBarConfigurer configurer )
  {
    super( configurer );
  }

  @Override
  protected void makeActions( IWorkbenchWindow window )
  {
    m_introAction = ActionFactory.INTRO.create( window );
    register( m_introAction );
  }

  @Override
  protected void fillMenuBar( IMenuManager menuBar )
  {

    MenuManager helpMenu = new MenuManager( "&Help", IWorkbenchActionConstants.M_HELP );
    menuBar.add( helpMenu );

    // Help
    helpMenu.add( m_introAction );

  }

}
