package org.kalypso.portal.wizard;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.contribs.eclipse.core.resources.IProjectProvider;

public class LoadProjectFromWorkspaceWizard extends Wizard implements INewWizard, IProjectProvider
{

  private IWorkbench m_workbench;

  private IStructuredSelection m_selection;

  private ContainerSelectionWizardPage mainPage;

  private IWorkspaceRoot m_root;

  public LoadProjectFromWorkspaceWizard( )
  {
    m_root = ResourcesPlugin.getWorkspace().getRoot();
  }

  public void addPages( )
  {
    super.addPages();
    mainPage = new ContainerSelectionWizardPage( "basicNewProjectPage", m_root );//$NON-NLS-1$
    mainPage.setTitle( "Neues Projekt vom Arbeitsplatz auswählen..." );
    mainPage.setDescription( "Wählen eines bestehnden Projektes" );
    this.addPage( mainPage );
  }

  @Override
  public boolean canFinish( )
  {
    boolean complete = super.canFinish();
    if( (getProject() instanceof IProject) && complete )
      return true;
    return false;
  }

  public boolean performFinish( )
  {
    return canFinish();
  }

  public void init( IWorkbench workbench, IStructuredSelection selection )
  {
    m_workbench = workbench;
    m_selection = selection;
  }

  public IProject getProject( )
  {
    return mainPage.getSelectedProject();
  }

}
