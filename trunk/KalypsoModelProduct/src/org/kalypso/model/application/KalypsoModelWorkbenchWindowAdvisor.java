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
package org.kalypso.model.application;

import org.eclipse.ui.IPerspectiveDescriptor;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.application.ActionBarAdvisor;
import org.eclipse.ui.application.IActionBarConfigurer;
import org.eclipse.ui.application.IWorkbenchWindowConfigurer;
import org.eclipse.ui.internal.intro.impl.IntroPlugin;
import org.eclipse.ui.internal.intro.impl.model.IntroModelRoot;
import org.eclipse.ui.intro.IIntroManager;
import org.eclipse.ui.intro.IIntroPart;
import org.eclipse.ui.intro.config.CustomizableIntroPart;
import org.kalypso.contribs.eclipse.ui.ide.application.IDEWorkbenchWindowAdvisor;

/**
 * @author Gernot Belger
 */
@SuppressWarnings("restriction")
public class KalypsoModelWorkbenchWindowAdvisor extends IDEWorkbenchWindowAdvisor
{
  private final boolean m_restrictedAccess;

  private String m_defaultPerspective;

  /**
   * @param restrictedAccess
   *            If true, only the model-product perspective will be available to the user. Toolbar and perspective bar
   *            will be hidden.
   */
  public KalypsoModelWorkbenchWindowAdvisor( final KalypsoModelWorkbenchAdvisor advisor, final IWorkbenchWindowConfigurer configurer, final boolean restrictedAccess )
  {
    super( advisor, configurer );
    m_restrictedAccess = restrictedAccess;
    m_defaultPerspective = advisor.getInitialWindowPerspectiveId();
  }

  @Override
  public ActionBarAdvisor createActionBarAdvisor( final IActionBarConfigurer configurer )
  {
    if( !m_restrictedAccess )
      return super.createActionBarAdvisor( configurer );

    return new KalypsoModelActionBarAdvisor( configurer );
  }

  /**
   * @see org.eclipse.ui.application.WorkbenchWindowAdvisor#postWindowCreate()
   */
  @Override
  public void postWindowCreate( )
  {
    super.postWindowCreate();

    /* If we are restricted, close all other perspectives now and open only the default one */
    if( m_restrictedAccess )
    {
      /* Reset the perspective, if a normal user is logging in and the expert perspective is active. */
      final IWorkbenchWindow window = getWindowConfigurer().getWindow();
      final IWorkbenchPage activePage = window.getActivePage();

      /* Make sure the default perspective is open. */
      final IPerspectiveDescriptor defaultPerpDesc = window.getWorkbench().getPerspectiveRegistry().findPerspectiveWithId( m_defaultPerspective );
      activePage.setPerspective( defaultPerpDesc );

      /* Close all other perspectives */
      final IPerspectiveDescriptor[] openPerspectives = activePage.getOpenPerspectives();
      for( final IPerspectiveDescriptor perspectiveDescriptor : openPerspectives )
      {
        if( !perspectiveDescriptor.getId().equals( m_defaultPerspective ) )
          activePage.closePerspective( perspectiveDescriptor, true, false );
      }
    }
  }

  @Override
  public void preWindowOpen( )
  {
    super.preWindowOpen();

    final IWorkbenchWindowConfigurer configurer = getWindowConfigurer();
    configurer.setShowCoolBar( !m_restrictedAccess );
    configurer.setShowPerspectiveBar( !m_restrictedAccess );
    configurer.setShowFastViewBars( !m_restrictedAccess );

    configurer.setShowProgressIndicator( true );
    configurer.setShowStatusLine( true );

    configurer.setTitle( "Kalypso Simulation Platform" );
  }

  /**
   * @see org.eclipse.ui.internal.ide.IDEWorkbenchWindowAdvisor#openIntro()
   */
  @Override
  public void openIntro( )
  {
    // TRICKY: we want to open the welcome page always as the starting page, not in
    // standby mode (the latter is the default behaviour, if we closed the workbench in standby)
    // This seemes the only way to force it.
    final IWorkbench workbench = getWindowConfigurer().getWorkbenchConfigurer().getWorkbench();
    final IIntroManager introManager = workbench.getIntroManager();

    final IIntroPart part = introManager.showIntro( null, false );
    if( part instanceof CustomizableIntroPart )
    {
      final CustomizableIntroPart customIntro = (CustomizableIntroPart) part;
      customIntro.getControl().setData( CustomizableIntroPart.SHOW_STANDBY_PART, null );
    }

    if( part != null )
      part.standbyStateChanged( false );

    // always start with the main page
    final IntroModelRoot model = IntroPlugin.getDefault().getIntroModelRoot();
    if( model != null )
      model.setCurrentPageId( "rootPage" );
  }
}
