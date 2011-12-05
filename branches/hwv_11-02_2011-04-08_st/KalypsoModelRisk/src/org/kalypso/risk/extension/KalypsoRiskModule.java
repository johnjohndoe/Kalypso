package org.kalypso.risk.extension;

import java.net.URL;

import org.kalypso.afgui.wizards.INewProjectWizard;
import org.kalypso.afgui.wizards.INewProjectWizardProvider;
import org.kalypso.module.IKalypsoModuleProjectOpenAction;
import org.kalypso.module.IKalypsoModuleWelcomePageFrame;
import org.kalypso.project.database.client.extension.AbstractKalypsoModule;
import org.kalypso.project.database.client.extension.database.IKalypsoModuleDatabaseSettings;
import org.kalypso.project.database.client.extension.project.SzenarioProjectOpenAction;
import org.kalypso.risk.plugin.KalypsoRiskPlugin;
import org.kalypso.risk.project.KalypsoRiskDemoProjectWizard;
import org.kalypso.risk.project.KalypsoRiskProjectWizard;

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

  /**
   * @see org.kalypso.project.database.client.extension.IKalypsoModule#getRemoteDatabaseSettings()
   */
  @Override
  public IKalypsoModuleDatabaseSettings getDatabaseSettings( )
  {
    return new KalypsoRiskRemoteDatabaseSettings();
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

}
