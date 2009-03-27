package org.kalypso.model.flood.extension;

import org.kalypso.project.database.client.extension.IKalypsoModule;
import org.kalypso.project.database.client.extension.database.IKalypsoRemoteDatabaseSettings;
import org.kalypso.project.database.client.extension.pages.module.IKalypsoModulePage;
import org.kalypso.project.database.client.extension.pages.welcome.IKalypsoModuleWelcomePageFrame;

public class KalypsoModelFloodModule implements IKalypsoModule
{

  // public constructor, needed because of declared extension point and java class loader
  public KalypsoModelFloodModule( )
  {
  }

  @Override
  public IKalypsoModuleWelcomePageFrame getWelcomePageFrame( )
  {
    return new KalypsoFloodWelcomePageFrame();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.extension.IKalypsoModule#getModuleEnteringPage()
   */
  @Override
  public IKalypsoModulePage getModulePage( )
  {
    return new KalypsoFloodModulePage( this );
  }

  /**
   * @see org.kalypso.project.database.client.extension.IKalypsoModule#getRemoteDatabaseSettings()
   */
  @Override
  public IKalypsoRemoteDatabaseSettings getRemoteDatabaseSettings( )
  {
    return new KalypsoFloodRemoteDatabaseSettings();
  }

}
