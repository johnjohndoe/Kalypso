package org.kalypso.statistics.project;

import org.kalypso.statistics.extension.KalypsoStatisticsModule;

public class KalypsoStatisticsProjectWizard extends org.kalypso.afgui.wizards.NewProjectWizard
{
  public static final String CATEGORY_STATISTIC_TEMPLATE = "org.kalypso.statistics.projectTemplate";

  static final public String ID = "org.kalypso.statistics.project.KalypsoStatisticsProjectWizard"; //$NON-NLS-1$

  public KalypsoStatisticsProjectWizard( )
  {
    super( CATEGORY_STATISTIC_TEMPLATE, false, KalypsoStatisticsModule.ID ); //$NON-NLS-1$
  }
}
