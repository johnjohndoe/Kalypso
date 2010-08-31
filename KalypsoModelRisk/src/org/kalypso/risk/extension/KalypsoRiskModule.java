package org.kalypso.risk.extension;

import org.kalypso.project.database.client.extension.IKalypsoModule;
import org.kalypso.project.database.client.extension.database.IKalypsoModuleDatabaseSettings;
import org.kalypso.project.database.client.extension.pages.module.IKalypsoModulePage;
import org.kalypso.project.database.client.extension.pages.welcome.IKalypsoModuleWelcomePageFrame;

public class KalypsoRiskModule implements IKalypsoModule
{
  public static final String ID = "KalypsoRiskModel"; //$NON-NLS-1$
  
  // public constructor, needed because of extension point and java class loader
  public KalypsoRiskModule( )
  {
  }

  @Override
  public IKalypsoModuleWelcomePageFrame getWelcomePageFrame( )
  {
    return new KalypsoRiskWelcomePageFrame();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.extension.IKalypsoModule#getModuleEnteringPage()
   */
  @Override
  public IKalypsoModulePage getModulePage( )
  {
    return new KalypsoRiskModulePage( this );
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
}
