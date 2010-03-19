package org.kalypso.model.flood.extension;

import org.kalypso.project.database.client.extension.IKalypsoModule;
import org.kalypso.project.database.client.extension.database.IKalypsoModuleDatabaseSettings;
import org.kalypso.project.database.client.extension.pages.module.IKalypsoModulePage;
import org.kalypso.project.database.client.extension.pages.welcome.IKalypsoModuleWelcomePageFrame;

public class KalypsoModelFloodModule implements IKalypsoModule
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
  public IKalypsoModuleDatabaseSettings getDatabaseSettings( )
  {
    return new KalypsoFloodRemoteDatabaseSettings();
  }

  @Override
  public String getId( )
  {
    return ID;
  }
}
