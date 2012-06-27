package org.kalypso.statistics.extension;

import java.net.URL;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.kalypso.module.AbstractKalypsoModule;
import org.kalypso.module.IKalypsoModuleProjectOpenAction;
import org.kalypso.module.IKalypsoModuleWelcomePageFrame;
import org.kalypso.module.welcome.INewProjectWizard;
import org.kalypso.module.welcome.INewProjectWizardProvider;
import org.kalypso.project.database.client.extension.project.SzenarioProjectOpenAction;
import org.kalypso.statistics.i18n.Messages;
import org.kalypso.statistics.plugin.KalypsoStatisticsPlugin;
import org.kalypso.statistics.project.KalypsoStatisticsDemoProjectWizard;
import org.kalypso.statistics.project.KalypsoStatisticsProjectWizard;

import de.renew.workflow.base.IWorkflow;
import de.renew.workflow.connector.WorkflowProjectNature;

public class KalypsoStatisticsModule extends AbstractKalypsoModule
{
  public static final String ID = "KalypsoStatisticsModule"; //$NON-NLS-1$

  // public constructor, needed because of extension point and java class loader
  public KalypsoStatisticsModule( )
  {
  }

  @Override
  public String getHeader( )
  {
    return Messages._MODULE_TITLE;
  }

  @Override
  public URL getInfoURL( )
  {
    return getInfoURL( getClass(), KalypsoStatisticsPlugin.getDefault() );
  }

  @Override
  public IKalypsoModuleWelcomePageFrame getWelcomePageFrame( )
  {
    return new KalypsoStatisticsWelcomePageFrame();
  }

  @Override
  public Integer getPriority( )
  {
    return 10;
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
        return new KalypsoStatisticsProjectWizard();
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
        return new KalypsoStatisticsDemoProjectWizard();
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

    return uri.contains( "http___www.tu-harburg.de_wb_kalypso_statistics__WF_KalypsoStatistics" ); //$NON-NLS-1$
  }

}
