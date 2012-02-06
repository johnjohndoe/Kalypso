package org.kalypso.model.flood.extension;

import java.net.URL;

import org.kalypso.afgui.wizards.INewProjectWizard;
import org.kalypso.afgui.wizards.INewProjectWizardProvider;
import org.kalypso.model.flood.KalypsoModelFloodPlugin;
import org.kalypso.model.flood.ui.wizards.NewDemoProjectWizard;
import org.kalypso.model.flood.ui.wizards.NewProjectWizard;
import org.kalypso.module.IKalypsoModuleProjectOpenAction;
import org.kalypso.module.IKalypsoModuleWelcomePageFrame;
import org.kalypso.project.database.client.extension.AbstractKalypsoModule;
import org.kalypso.project.database.client.extension.database.IKalypsoModuleDatabaseSettings;
import org.kalypso.project.database.client.extension.project.SzenarioProjectOpenAction;

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

  /**
   * @see org.kalypso.project.database.client.extension.IKalypsoModule#getRemoteDatabaseSettings()
   */
  @Override
  public IKalypsoModuleDatabaseSettings getDatabaseSettings( )
  {
    return new KalypsoFloodRemoteDatabaseSettings();
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

}
