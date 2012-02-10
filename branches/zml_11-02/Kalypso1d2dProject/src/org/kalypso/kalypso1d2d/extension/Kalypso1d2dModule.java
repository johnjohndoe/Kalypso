package org.kalypso.kalypso1d2d.extension;

import java.net.URL;

import org.kalypso.afgui.wizards.INewProjectWizard;
import org.kalypso.afgui.wizards.INewProjectWizardProvider;
import org.kalypso.kalypso1d2d.pjt.Kalypso1D2DDemoProjectWizard;
import org.kalypso.kalypso1d2d.pjt.Kalypso1D2DNewProjectWizard;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypso.module.IKalypsoModuleProjectOpenAction;
import org.kalypso.module.IKalypsoModuleWelcomePageFrame;
import org.kalypso.project.database.client.extension.AbstractKalypsoModule;
import org.kalypso.project.database.client.extension.database.IKalypsoModuleDatabaseSettings;
import org.kalypso.project.database.client.extension.project.SzenarioProjectOpenAction;

public class Kalypso1d2dModule extends AbstractKalypsoModule
{
  public static final String ID = "Kalypso1d2dModel"; //$NON-NLS-1$

  // public constructor, needed because of declared extension point and java class loader
  public Kalypso1d2dModule( )
  {
  }

  @Override
  public String getHeader( )
  {
    return "Kalypso1D2D"; //$NON-NLS-1$
  }

  @Override
  public URL getInfoURL( )
  {
    return getInfoURL( getClass(), Kalypso1d2dProjectPlugin.getDefault() );
  }

  @Override
  public Integer getPriority( )
  {
    return 3;
  }

  @Override
  public IKalypsoModuleWelcomePageFrame getWelcomePageFrame( )
  {
    return new Kalypso1d2dWelcomePageFrame();
  }

  /**
   * @see org.kalypso.project.database.client.extension.IKalypsoModule#getRemoteDatabaseSettings()
   */
  @Override
  public IKalypsoModuleDatabaseSettings getDatabaseSettings( )
  {
    return new Kalypso1d2dRemoteDatabaseSettings();
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
        return new Kalypso1D2DNewProjectWizard();
      }
    };
  }

  @Override
  protected INewProjectWizardProvider getDemoProjectWizard( )
  {
    return new INewProjectWizardProvider()
    {
      @Override
      public INewProjectWizard createWizard( )
      {
        return new Kalypso1D2DDemoProjectWizard();
      }
    };
  }

  @Override
  public IKalypsoModuleProjectOpenAction getProjectOpenAction( )
  {
    return new SzenarioProjectOpenAction( ID );
  }

}
