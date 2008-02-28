package org.kalypso.kalypso1d2d.pjt;

import org.kalypso.afgui.wizards.NewProjectWizard;

/**
 * Creates a new Kalypso 1d 2d Project
 * 
 * @author Patrice Congo
 */
public class Kalypso1D2DNewProjectWizard extends NewProjectWizard
{
  public final static String ID = "org.kalypso.kalypso1d2d.pjt.Kalypso1D2DNewProjectWizard"; //$NON-NLS-1$

  public Kalypso1D2DNewProjectWizard( )
  {
    super( "org.kalypso.kalypso1d2d.pjt.projectTemplate", false );//$NON-NLS-1$
  }
}
