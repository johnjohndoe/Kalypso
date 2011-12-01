package org.kalypso.kalypso1d2d.pjt;

import org.kalypso.afgui.wizards.NewProjectWizard;
import org.kalypso.kalypso1d2d.extension.Kalypso1d2dModule;

/**
 * Creates a demo Kalypso 1d 2d Project
 * 
 * @author Thomas Jung
 */
public class Kalypso1D2DDemoProjectWizard extends NewProjectWizard
{
  public final static String ID = "org.kalypso.kalypso1d2d.pjt.Kalypso1D2DDemoProjectWizard"; //$NON-NLS-1$

  public Kalypso1D2DDemoProjectWizard( )
  {
    super( "org.kalypso.kalypso1d2d.pjt.demoProject", true, Kalypso1d2dModule.ID );//$NON-NLS-1$
  }
}
