package org.kalypso.kalypso1d2d.pjt;

import org.kalypso.kalypso1d2d.extension.Kalypso1d2dModule;

/**
 * Creates a demo Kalypso 1d 2d Project
 * 
 * @author Thomas Jung
 */
public class Kalypso1D2DDemoProjectWizard extends org.kalypso.afgui.wizards.NewProjectWizard
{
  public static final String CATEGORY_ID = "org.kalypso.kalypso1d2d.pjt.demoProject"; //$NON-NLS-1$

  public static final String ID = "org.kalypso.kalypso1d2d.pjt.Kalypso1D2DDemoProjectWizard"; //$NON-NLS-1$

  public Kalypso1D2DDemoProjectWizard( )
  {
    super( CATEGORY_ID, true, Kalypso1d2dModule.ID );//$NON-NLS-1$
  }
}