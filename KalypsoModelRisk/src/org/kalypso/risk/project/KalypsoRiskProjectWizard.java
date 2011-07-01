package org.kalypso.risk.project;

import org.kalypso.risk.extension.KalypsoRiskModule;

public class KalypsoRiskProjectWizard extends org.kalypso.afgui.wizards.NewProjectWizard
{
  final static public String ID = "org.kalypso.risk.project.KalypsoRiskProjectWizard"; //$NON-NLS-1$

  public KalypsoRiskProjectWizard( )
  {
    super( "org.kalypso.risk.projectTemplate", false, KalypsoRiskModule.ID ); //$NON-NLS-1$
  }
}
