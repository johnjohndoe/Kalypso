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
package org.kalypso.project.database.client.ui.project.wizard.create;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.IPageChangedListener;
import org.eclipse.jface.dialogs.PageChangedEvent;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.WizardNewProjectCreationPage;
import org.eclipse.ui.forms.events.HyperlinkAdapter;
import org.eclipse.ui.forms.events.HyperlinkEvent;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ImageHyperlink;
import org.kalypso.afgui.extension.IEnteringPageWizardDelegate;
import org.kalypso.afgui.extension.INewProjectWizard;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.wizard.IUpdateable;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.project.database.client.KalypsoProjectDatabaseClient;
import org.kalypso.project.database.common.nature.IRemoteProjectPreferences;
import org.kalypso.project.database.common.nature.RemoteProjectNature;

/**
 * Composite for calling the new project wizard
 * 
 * @author Dirk Kuch
 */
public class CreateProjectComposite extends Composite
{
  public static Image IMG_ADD_PROJECT = new Image( null, CreateProjectComposite.class.getResourceAsStream( "icons/add_project.gif" ) ); //$NON-NLS-1$

  public static Image IMG_EXTRACT_DEMO = new Image( null, CreateProjectComposite.class.getResourceAsStream( "icons/extract_demo.gif" ) ); //$NON-NLS-1$

  private final FormToolkit m_toolkit;

  private final String m_label;

  protected final IEnteringPageWizardDelegate m_delegate;

  public CreateProjectComposite( final String label, final Composite parent, final FormToolkit toolkit, final IEnteringPageWizardDelegate delegate )
  {
    super( parent, SWT.NULL );

    m_label = label;
    m_toolkit = toolkit;
    m_delegate = delegate;

    final GridLayout layout = new GridLayout();
    layout.verticalSpacing = layout.marginWidth = 0;
    this.setLayout( layout );

    update();
  }

  /**
   * @see org.eclipse.swt.widgets.Control#update()
   */
  @Override
  public final void update( )
  {
    if( this.isDisposed() )
    {
      return;
    }

    final ImageHyperlink lnkCreateProject = m_toolkit.createImageHyperlink( this, SWT.NULL );
    lnkCreateProject.setImage( m_delegate.getImage() );
    lnkCreateProject.setText( m_label );

    lnkCreateProject.addHyperlinkListener( new HyperlinkAdapter()
    {
      /**
       * @see org.eclipse.ui.forms.events.HyperlinkAdapter#linkActivated(org.eclipse.ui.forms.events.HyperlinkEvent)
       */
      @Override
      public void linkActivated( final HyperlinkEvent e )
      {
        final INewProjectWizard wizard = m_delegate.getWizard();

        wizard.init( PlatformUI.getWorkbench(), null );
        wizard.setActivateScenarioOnPerformFinish( false );

        final WizardDialog2 dialog = new WizardDialog2( PlatformUI.getWorkbench().getDisplay().getActiveShell(), wizard );
        dialog.setRememberSize( true );

        dialog.addPageChangedListener( new IPageChangedListener()
        {
          private boolean resetProjectName = true;

          public void pageChanged( final PageChangedEvent event )
          {
            final Object page = event.getSelectedPage();
            if( page instanceof IUpdateable )
            {
              final IUpdateable update = (IUpdateable) page;
              update.update();
            }
            else if( wizard instanceof WizardCreateProject && page instanceof WizardNewProjectCreationPage )
            {
              if( resetProjectName )
              {
                final WizardNewProjectCreationPage myPage = (WizardNewProjectCreationPage) page;
                DisableCreateProjectWizardPageElements.disableElementsForProjectCreation( myPage );

                resetProjectName = false;
              }
            }
          }
        } );

        dialog.open();
        if( dialog.getReturnCode() == Window.OK )
        {
          try
          {
            final IProject project = wizard.getNewProject();
            final IProjectNature nature = project.getNature( RemoteProjectNature.NATURE_ID );
            if( nature instanceof RemoteProjectNature )
            {
              final RemoteProjectNature remote = (RemoteProjectNature) nature;
              final IRemoteProjectPreferences preferences = remote.getRemotePreferences( project, null );
              preferences.setVersion( -1 );
              preferences.setIsOnServer( Boolean.FALSE );
              preferences.setProjectType( m_delegate.getRemoteCommitType() );
            }
          }
          catch( final CoreException e1 )
          {
            KalypsoProjectDatabaseClient.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e1 ) );
          }
        }
      }
    } );

    m_toolkit.adapt( this );
    this.layout();
  }
}
