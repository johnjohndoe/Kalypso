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

  private static final String EMPTY_PROJECT_ZIP_PATH = "resources/emptyProject.zip"; //$NON-NLS-1$

  public Kalypso1D2DNewProjectWizard( )
  {
    super( Kalypso1D2DNewProjectWizard.class.getResource( EMPTY_PROJECT_ZIP_PATH ) );
  }
}
