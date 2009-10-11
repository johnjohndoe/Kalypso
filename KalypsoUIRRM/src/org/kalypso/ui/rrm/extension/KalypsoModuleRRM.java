package org.kalypso.ui.rrm.extension;

import org.kalypso.project.database.client.extension.IKalypsoModule;
import org.kalypso.project.database.client.extension.database.IKalypsoModuleDatabaseSettings;
import org.kalypso.project.database.client.extension.pages.module.IKalypsoModulePage;
import org.kalypso.project.database.client.extension.pages.welcome.IKalypsoModuleWelcomePageFrame;

public class KalypsoModuleRRM implements IKalypsoModule
{
  public static final String ID = "KalypsRrmModel"; //$NON-NLS-1$
  // public constructor, needed because of declared extension point and java class loader
  public KalypsoModuleRRM( )
  {
  }

  @Override
  public IKalypsoModuleWelcomePageFrame getWelcomePageFrame( )
  {
    return new KalypsoRrmWelcomePageFrame();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.extension.IKalypsoModule#getModuleEnteringPage()
   */
  @Override
  public IKalypsoModulePage getModulePage( )
  {
    // FIXME: this is called too often! Why all these different interfaces at all?
    // Most of the information should be put into an extension-point as parameters. No need for this much
    // (often repeated) code.
    return new KalypsoRrmModulePage( this );
  }

  
  /**
   * @see org.kalypso.project.database.client.extension.IKalypsoModule#getRemoteDatabaseSettings()
   */
  @Override
  public IKalypsoModuleDatabaseSettings getDatabaseSettings( )
  {
    return new KalypsoRrmRemoteDatabaseSettings();
  }

  @Override
  public String getId( )
  {
    return ID;
  }
}
