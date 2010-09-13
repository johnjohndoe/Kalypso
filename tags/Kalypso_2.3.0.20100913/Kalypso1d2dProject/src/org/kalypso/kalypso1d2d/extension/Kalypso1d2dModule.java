package org.kalypso.kalypso1d2d.extension;

import org.kalypso.project.database.client.extension.IKalypsoModule;
import org.kalypso.project.database.client.extension.database.IKalypsoModuleDatabaseSettings;
import org.kalypso.project.database.client.extension.pages.module.IKalypsoModulePage;
import org.kalypso.project.database.client.extension.pages.welcome.IKalypsoModuleWelcomePageFrame;

public class Kalypso1d2dModule implements IKalypsoModule
{
  public static final String ID = "Kalypso1d2dModel"; //$NON-NLS-1$
  
  // public constructor, needed because of declared extension point and java class loader
  public Kalypso1d2dModule( )
  {
  }

  @Override
  public IKalypsoModuleWelcomePageFrame getWelcomePageFrame( )
  {
    return new Kalypso1d2dWelcomePageFrame();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.extension.IKalypsoModule#getModuleEnteringPage()
   */
  @Override
  public IKalypsoModulePage getModulePage( )
  {
    return new Kalypso1d2dModulePage( this );

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
}
