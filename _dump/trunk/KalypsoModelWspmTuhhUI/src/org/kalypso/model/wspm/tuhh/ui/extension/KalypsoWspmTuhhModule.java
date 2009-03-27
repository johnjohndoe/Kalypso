package org.kalypso.model.wspm.tuhh.ui.extension;

import org.kalypso.project.database.client.extension.IKalypsoModule;
import org.kalypso.project.database.client.extension.database.IKalypsoRemoteDatabaseSettings;
import org.kalypso.project.database.client.extension.pages.module.IKalypsoModulePage;
import org.kalypso.project.database.client.extension.pages.welcome.IKalypsoModuleWelcomePageFrame;

public class KalypsoWspmTuhhModule implements IKalypsoModule
{

  // public constructor, needed because of declared extension point and java class loader
  public KalypsoWspmTuhhModule( )
  {
  }

  @Override
  public IKalypsoModuleWelcomePageFrame getWelcomePageFrame( )
  {
    return new KalypsoWspmWelcomePageFrame();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.extension.IKalypsoModule#getModuleEnteringPage()
   */
  @Override
  public IKalypsoModulePage getModulePage( )
  {
    return new KalypsoWspmModulePage( this );
  }

  /**
   * @see org.kalypso.project.database.client.extension.IKalypsoModule#getRemoteDatabaseSettings()
   */
  @Override
  public IKalypsoRemoteDatabaseSettings getRemoteDatabaseSettings( )
  {
    return new KalypsoWspmRemoteDatabaseSettings();
  }

}
