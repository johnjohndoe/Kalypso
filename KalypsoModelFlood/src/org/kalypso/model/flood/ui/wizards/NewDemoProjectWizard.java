package org.kalypso.model.flood.ui.wizards;

import org.kalypso.model.flood.extension.KalypsoModelFloodModule;

public class NewDemoProjectWizard extends org.kalypso.afgui.wizards.NewProjectWizard
{
  public static final String CATEGORY_ID = "org.kalypso.model.flood.demoProject"; //$NON-NLS-1$

  public NewDemoProjectWizard( )
  {
    super( CATEGORY_ID, true, KalypsoModelFloodModule.ID );
  }
}