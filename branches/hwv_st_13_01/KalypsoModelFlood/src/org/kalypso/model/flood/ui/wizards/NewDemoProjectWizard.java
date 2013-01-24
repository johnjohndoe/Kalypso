package org.kalypso.model.flood.ui.wizards;

import org.kalypso.model.flood.extension.KalypsoModelFloodModule;

public class NewDemoProjectWizard extends org.kalypso.afgui.wizards.NewProjectWizard
{
  public NewDemoProjectWizard( )
  {
    super( "org.kalypso.model.flood.demoProject", true, KalypsoModelFloodModule.ID ); //$NON-NLS-1$
  }
}
