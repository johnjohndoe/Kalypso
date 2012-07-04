package org.kalypso.risk.extension;

import java.net.URL;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.kalypso.module.AbstractKalypsoModule;
import org.kalypso.module.IKalypsoModuleProjectOpenAction;
import org.kalypso.module.IKalypsoModuleWelcomePageFrame;
import org.kalypso.module.ISetAsBaseScenarioHandler;
import org.kalypso.module.welcome.INewProjectWizard;
import org.kalypso.module.welcome.INewProjectWizardProvider;
import org.kalypso.project.database.client.extension.project.SzenarioProjectOpenAction;
import org.kalypso.risk.plugin.KalypsoRiskPlugin;
import org.kalypso.risk.project.KalypsoRiskDemoProjectWizard;
import org.kalypso.risk.project.KalypsoRiskProjectWizard;

import de.renew.workflow.base.IWorkflow;
import de.renew.workflow.connector.WorkflowProjectNature;

public class KalypsoRiskModule extends AbstractKalypsoModule
{
  public static final String ID = "KalypsoRiskModel"; //$NON-NLS-1$

  // public constructor, needed because of extension point and java class loader
  public KalypsoRiskModule( )
  {
  }

  @Override
  public String getHeader( )
  {
    return "KalypsoRisk"; //$NON-NLS-1$
  }

  @Override
  public URL getInfoURL( )
  {
    return getInfoURL( getClass(), KalypsoRiskPlugin.getDefault() );
  }

  @Override
  public IKalypsoModuleWelcomePageFrame getWelcomePageFrame( )
  {
    return new KalypsoRiskWelcomePageFrame();
  }

  @Override
  public Integer getPriority( )
  {
    return 5;
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
        return new KalypsoRiskProjectWizard();
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
        return new KalypsoRiskDemoProjectWizard();
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

    return uri.contains( "http___www.tu-harburg.de_wb_kalypso_risk__WF_KalypsoRisk" ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.module.IKalypsoModule#getNewProjectCategoryId()
   */
  @Override
  public String getNewProjectCategoryId( )
  {
    return KalypsoRiskProjectWizard.CATEGORY_ID;
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