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
package org.kalypso.afgui.wizards;

import java.lang.reflect.InvocationTargetException;
import java.net.URL;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.IPageChangingListener;
import org.eclipse.jface.dialogs.PageChangingEvent;
import org.eclipse.jface.wizard.IWizardContainer;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.eclipse.ui.dialogs.WizardNewProjectCreationPage;
import org.eclipse.ui.dialogs.WizardNewProjectReferencePage;
import org.eclipse.ui.wizards.newresource.BasicNewProjectResourceWizard;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.ScenarioHandlingProjectNature;
import org.kalypso.afgui.extension.INewProjectWizard;
import org.kalypso.afgui.scenarios.IScenario;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.eclipse.core.resources.ProjectTemplate;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.contribs.eclipse.jface.wizard.ProjectTemplatePage;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;

/**
 * Basic wizard implementation for the various workflow/scenario based projects.<br>
 * Normally, only the location of the project-template (-zip) should be enough.<br>
 * 
 * @author Gernot Belger
 */
public class NewProjectWizard extends BasicNewProjectResourceWizard implements INewProjectWizard
{
  private final IPageChangingListener m_pageChangeingListener = new IPageChangingListener()
  {
    public void handlePageChanging( final PageChangingEvent event )
    {
      doHandlePageChangeing( event );
    }
  };

  private final ProjectTemplatePage m_templateProjectPage;

  protected boolean m_activateScenario = true;

  /**
   * @param categoryId
   *          If non-<code>null</code>, only project templates with this categoryId are shown.
   */
  public NewProjectWizard( final String categoryId, final boolean showTemplatePage )
  {
    m_templateProjectPage = new ProjectTemplatePage( categoryId );
    if( showTemplatePage )
      addPage( m_templateProjectPage );
  }

  public NewProjectWizard( final ProjectTemplatePage page, final boolean showTemplates )
  {
    m_templateProjectPage = page;
    if( showTemplates )
      addPage( m_templateProjectPage );
  }

  public void setActivateScenarioOnPerformFinish( final boolean activate )
  {
    m_activateScenario = activate;
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#setContainer(org.eclipse.jface.wizard.IWizardContainer)
   */
  @Override
  public void setContainer( final IWizardContainer wizardContainer )
  {
    final IWizardContainer currentContainer = getContainer();
    if( currentContainer instanceof WizardDialog )
      ((WizardDialog) currentContainer).removePageChangingListener( m_pageChangeingListener );

    super.setContainer( wizardContainer );

    if( wizardContainer instanceof WizardDialog )
      ((WizardDialog) wizardContainer).addPageChangingListener( m_pageChangeingListener );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#dispose()
   */
  @Override
  public void dispose( )
  {
    final IWizardContainer wizardContainer = getContainer();
    if( wizardContainer instanceof WizardDialog )
      ((WizardDialog) wizardContainer).removePageChangingListener( m_pageChangeingListener );

    super.dispose();
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#createPageControls(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPageControls( final Composite pageContainer )
  {
    // Overwritten in order to NOT create the pages during initialization, so we can set
    // the project name later before the next page is created, this works however only once :-(

    // HACK: special case: the resource page: will lead to NPE if not created
    final IWizardPage page = getPage( "basicReferenceProjectPage" );
    if( page != null )
      page.createControl( pageContainer );
  }

  protected void doHandlePageChangeing( final PageChangingEvent event )
  {
    if( event.getCurrentPage() == m_templateProjectPage )
    {
      final ProjectTemplate demoProject = m_templateProjectPage.getSelectedProject();
      // TODO: Does work only the first time :-( ,see above createPageControls

      final WizardNewProjectCreationPage createProjectPage = (WizardNewProjectCreationPage) getPage( "basicNewProjectPage" );
      createProjectPage.setInitialProjectName( demoProject.getProjectName() );
    }
  }

  @Override
  /*
   * This method was overridden in order to get rid of the 'select dependent projects' page from the
   * BasicNewProjectResourceWizard.
   */
  public IWizardPage getNextPage( final IWizardPage page )
  {
    // HACK: to do so, we just skip this particular page
    // Unfortunately we cannot just override 'addPages' and do not add the second page,
    // because the BasicNewProjectResourceWizard relies on the second page to exist.
    final IWizardPage nextPage = super.getNextPage( page );
    if( nextPage instanceof WizardNewProjectReferencePage )
      return super.getNextPage( nextPage );

    return nextPage;
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#getPreviousPage(org.eclipse.jface.wizard.IWizardPage)
   */
  @Override
  public IWizardPage getPreviousPage( final IWizardPage page )
  {
    // HACK: see get next page
    final IWizardPage previousPage = super.getPreviousPage( page );
    if( previousPage instanceof WizardNewProjectReferencePage )
      return super.getPreviousPage( previousPage );

    return previousPage;
  }

  @Override
  public boolean performFinish( )
  {
    final boolean result = super.performFinish();

    if( !result )
      return false;

    final URL zipURl = m_templateProjectPage.getSelectedProject().getData();
    final IProject project = getNewProject();
    // final String newName = project.getName();

    final WorkspaceModifyOperation operation = new WorkspaceModifyOperation( project.getWorkspace().getRoot() )
    {
      @Override
      public void execute( final IProgressMonitor monitor ) throws CoreException, InvocationTargetException
      {
        final SubMonitor progress = SubMonitor.convert( monitor, "Projektstruktur wird erzeugt", 80 );
        try
        {
          // REMARK: we unpack into a closed project here (not using unzip(URL, IFolder)), as else
          // the project description will not be up-to-date in time, resulting in missing natures.
          project.close( progress.newChild( 10 ) );

          /* Unpack project from template */
          ZipUtilities.unzip( zipURl, project.getLocation().toFile() );
          ProgressUtilities.worked( progress, 40 );
          project.open( progress.newChild( 10 ) );
          // IMPORTANT: As the project was already open before, we need to refresh here, else
          // not all resources are up-to-date
          project.refreshLocal( IResource.DEPTH_INFINITE, progress.newChild( 10 ) );

          /* validate and configure all natures of this project */
          final IProjectDescription description = project.getDescription();

          final String[] natureIds = description.getNatureIds();
          final IStatus validateNatureSetStatus = project.getWorkspace().validateNatureSet( natureIds );
          if( !validateNatureSetStatus.isOK() )
            throw new CoreException( validateNatureSetStatus );

          progress.setWorkRemaining( natureIds.length );

          for( final String natureId : natureIds )
          {
            final IProjectNature nature = project.getNature( natureId );
            nature.configure();
            ProgressUtilities.worked( progress, 1 );
          }

          /* Also activate new project */
          final ScenarioHandlingProjectNature nature = ScenarioHandlingProjectNature.toThisNature( project );
          if( nature == null )
            return;

          final IScenario caze = nature.getCaseManager().getCases().get( 0 );

          if( m_activateScenario )
            KalypsoAFGUIFrameworkPlugin.getDefault().getActiveWorkContext().setCurrentCase( caze );
        }
        catch( final CoreException t )
        {
          // If anything went wrong, clean up the project
          progress.setWorkRemaining( 10 );
          project.delete( true, progress );

          throw t;
        }
        catch( final Throwable t )
        {
          // If anything went wrong, clean up the project
          progress.setWorkRemaining( 10 );
          project.delete( true, progress );

          throw new InvocationTargetException( t );
        }
      }
    };

    final IStatus resultStatus = RunnableContextHelper.execute( getContainer(), true, true, operation );
    KalypsoAFGUIFrameworkPlugin.getDefault().getLog().log( resultStatus );
    ErrorDialog.openError( getShell(), "Projekt Neu", "Fehler beim Erzeugen des Projekts", resultStatus );

    // REMARK: we always return here, because the BasicNewProjectWizard does not allow to create a project twice
    // So the wizard must be closed now
    return true;
  }
}
