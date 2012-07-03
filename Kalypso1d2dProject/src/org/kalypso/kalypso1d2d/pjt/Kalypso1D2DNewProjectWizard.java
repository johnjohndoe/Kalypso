package org.kalypso.kalypso1d2d.pjt;

import org.kalypso.kalypso1d2d.extension.Kalypso1d2dModule;

/**
 * Creates a new Kalypso 1d 2d Project
 * 
 * @author Patrice Congo
 */
public class Kalypso1D2DNewProjectWizard extends org.kalypso.afgui.wizards.NewProjectWizard
{
  public static final String CATEGORY_ID = "org.kalypso.kalypso1d2d.pjt.projectTemplate"; //$NON-NLS-1$

  public static final String ID = "org.kalypso.kalypso1d2d.pjt.Kalypso1D2DNewProjectWizard"; //$NON-NLS-1$

  public Kalypso1D2DNewProjectWizard( )
  {
    super( CATEGORY_ID, false, Kalypso1d2dModule.ID );
  }
}