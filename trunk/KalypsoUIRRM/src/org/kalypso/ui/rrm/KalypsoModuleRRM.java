package org.kalypso.ui.rrm;

import java.net.URL;
import java.util.Collection;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.action.IAction;
import org.kalypso.afgui.wizards.ProjectConversionWizard;
import org.kalypso.model.hydrology.project.RrmProject;
import org.kalypso.model.hydrology.project.RrmScenario;
import org.kalypso.module.AbstractKalypsoModule;
import org.kalypso.module.IKalypsoModuleProjectOpenAction;
import org.kalypso.module.IKalypsoModuleWelcomePageFrame;
import org.kalypso.module.INewProjectHandler;
import org.kalypso.module.ISetAsBaseScenarioHandler;
import org.kalypso.module.welcome.INewProjectWizard;
import org.kalypso.module.welcome.INewProjectWizardProvider;
import org.kalypso.module.welcome.SpecialImportProjectAction;
import org.kalypso.project.database.client.extension.project.SzenarioProjectOpenAction;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.newproject.KalypsoNAProjectWizard;
import org.kalypso.ui.rrm.internal.utils.KalypsoRrmSetAsBaseScenarioHandler;
import org.kalypso.ui.rrm.internal.welcome.KalypsoRrmNewProjectHandler;
import org.kalypso.ui.rrm.internal.welcome.KalypsoRrmWelcomePageFrame;

public class KalypsoModuleRRM extends AbstractKalypsoModule
{
  public static final String ID = "KalypsRrmModel"; //$NON-NLS-1$

  // public constructor, needed because of declared extension point and java class loader
  public KalypsoModuleRRM( )
  {
  }

  @Override
  public String getHeader( )
  {
    return "KalypsoHydrology"; //$NON-NLS-1$
  }

  @Override
  public URL getInfoURL( )
  {
    return getInfoURL( KalypsoRrmWelcomePageFrame.class, KalypsoUIRRMPlugin.getDefault() );
  }

  @Override
  public Integer getPriority( )
  {
    return 1;
  }

  @Override
  public IKalypsoModuleWelcomePageFrame getWelcomePageFrame( )
  {
    return new KalypsoRrmWelcomePageFrame();
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
        return new KalypsoNAProjectWizard();
      }
    };
  }

  /**
   * @see org.kalypso.project.database.client.extension.AbstractKalypsoModule#getDemoProjectWizard()
   */
  @Override
  protected INewProjectWizardProvider getDemoProjectWizard( )
  {
    return null;
  }

  /**
   * @see org.kalypso.project.database.client.extension.AbstractKalypsoModule#addProjectActions(java.util.Collection)
   */
  @Override
  protected void addProjectActions( final Collection<IAction> actions )
  {
    final INewProjectWizardProvider provider = new INewProjectWizardProvider()
    {
      @Override
      public INewProjectWizard createWizard( )
      {
        return new ProjectConversionWizard( ID, KalypsoNAProjectWizard.CATEGORY_TEMPLATE );
      }
    };

    actions.add( new SpecialImportProjectAction( Messages.getString( "KalypsoModuleRRM_0" ), provider ) ); //$NON-NLS-1$
  }

  @Override
  public IKalypsoModuleProjectOpenAction getProjectOpenAction( )
  {
    return new SzenarioProjectOpenAction( KalypsoModuleRRM.ID );
  }

  @Override
  public boolean acceptProject( final IProject project )
  {
    final RrmProject rrmProject = new RrmProject( project );
    final RrmScenario baseScenario = rrmProject.getBaseScenario();

    final IFile hydrotopeFile = baseScenario.getHydrotopGml();

    return hydrotopeFile.exists();
  }

  @Override
  public INewProjectHandler getNewProjectHandler( )
  {
    return new KalypsoRrmNewProjectHandler();
  }

  /**
   * @see org.kalypso.module.IKalypsoModule#getNewProjectCategoryId()
   */
  @Override
  public String getNewProjectCategoryId( )
  {
    return KalypsoNAProjectWizard.CATEGORY_TEMPLATE;
  }

  /**
   * @see org.kalypso.module.IKalypsoModule#getSetAsBaseScenarioHandler()
   */
  @Override
  public ISetAsBaseScenarioHandler getSetAsBaseScenarioHandler( )
  {
    return new KalypsoRrmSetAsBaseScenarioHandler();
  }
}