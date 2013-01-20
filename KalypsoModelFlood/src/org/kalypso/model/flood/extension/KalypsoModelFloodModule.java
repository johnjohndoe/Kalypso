package org.kalypso.model.flood.extension;

import java.net.URL;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.kalypso.afgui.scenarios.SzenarioProjectOpenAction;
import org.kalypso.model.flood.KalypsoModelFloodPlugin;
import org.kalypso.model.flood.ui.wizards.NewDemoProjectWizard;
import org.kalypso.model.flood.ui.wizards.NewProjectWizard;
import org.kalypso.module.AbstractKalypsoModule;
import org.kalypso.module.IKalypsoModuleProjectOpenAction;
import org.kalypso.module.IKalypsoModuleWelcomePageFrame;
import org.kalypso.module.ISetAsBaseScenarioHandler;
import org.kalypso.module.welcome.INewProjectWizard;
import org.kalypso.module.welcome.INewProjectWizardProvider;

import de.renew.workflow.base.IWorkflow;
import de.renew.workflow.connector.WorkflowProjectNature;

public class KalypsoModelFloodModule extends AbstractKalypsoModule
{
  public static final String ID = "KalypsoFloodModelType"; //$NON-NLS-1$

  // public constructor, needed because of declared extension point and java class loader
  public KalypsoModelFloodModule( )
  {
  }

  @Override
  public IKalypsoModuleWelcomePageFrame getWelcomePageFrame( )
  {
    return new KalypsoFloodWelcomePageFrame();
  }

  @Override
  public String getHeader( )
  {
    return "KalypsoFlood"; //$NON-NLS-1$
  }

  @Override
  public Integer getPriority( )
  {
    return 4;
  }

  @Override
  public URL getInfoURL( )
  {
    return getInfoURL( getClass(), KalypsoModelFloodPlugin.getDefault() );
  }

  @Override
  public String getId( )
  {
    return ID;
  }

  /**
   * @see org.kalypso.project.database.client.extension.AbstractKalypsoModule#getNewProjectWizard()
   */
  @Override
  protected INewProjectWizardProvider getNewProjectWizard( )
  {
    return new INewProjectWizardProvider()
    {
      @Override
      public INewProjectWizard createWizard( )
      {
        return new NewProjectWizard();
      }
    };
  }

  /**
   * @see org.kalypso.project.database.client.extension.AbstractKalypsoModule#getDemoProjectWizard()
   */
  @Override
  protected INewProjectWizardProvider getDemoProjectWizard( )
  {
    return new INewProjectWizardProvider()
    {
      @Override
      public INewProjectWizard createWizard( )
      {
        return new NewDemoProjectWizard();
      }
    };
  }

  @Override
  public IKalypsoModuleProjectOpenAction getProjectOpenAction( )
  {
    return new SzenarioProjectOpenAction( ID );
  }

  /**
   * @see org.kalypso.module.IKalypsoModule#acceptProject(org.eclipse.core.resources.IProject)
   */
  @Override
  public boolean acceptProject( final IProject project ) throws CoreException
  {
    final WorkflowProjectNature nature = WorkflowProjectNature.toThisNature( project );
    if( nature == null )
      return false;

    final IWorkflow workflow = nature.getCurrentWorklist();
    final String uri = workflow.getURI();

    return uri.contains( "org.kalypso.model.flood.WF_KalypsoFlood" ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.module.IKalypsoModule#getNewProjectCategoryId()
   */
  @Override
  public String getNewProjectCategoryId( )
  {
    return NewProjectWizard.CATEGORY_ID;
  }

  /**
   * @see org.kalypso.module.IKalypsoModule#getSetAsBaseScenarioHandler()
   */
  @Override
  public ISetAsBaseScenarioHandler getSetAsBaseScenarioHandler( )
  {
    return null;
  }
}