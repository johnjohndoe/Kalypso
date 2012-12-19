package org.kalypso.risk.project;

import org.kalypso.risk.extension.KalypsoRiskModule;

public class KalypsoRiskProjectWizard extends org.kalypso.afgui.wizards.NewProjectWizard
{
  public static final String CATEGORY_ID = "org.kalypso.risk.projectTemplate"; //$NON-NLS-1$

  public static final String ID = "org.kalypso.risk.project.KalypsoRiskProjectWizard"; //$NON-NLS-1$

  public KalypsoRiskProjectWizard( )
  {
    super( CATEGORY_ID, false, KalypsoRiskModule.ID );
  }
}