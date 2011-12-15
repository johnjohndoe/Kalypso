package org.kalypso.statistics.project;

import org.kalypso.statistics.extension.KalypsoStatisticsModule;

public class KalypsoStatisticsProjectWizard extends org.kalypso.afgui.wizards.NewProjectWizard
{
  final static public String ID = "org.kalypso.statistics.project.KalypsoStatisticsProjectWizard"; //$NON-NLS-1$

  public KalypsoStatisticsProjectWizard( )
  {
    super( "org.kalypso.statistics.projectTemplate", false, KalypsoStatisticsModule.ID ); //$NON-NLS-1$
  }
}
