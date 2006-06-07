package org.kalypso.model.wspm.ui.profil.view;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

/**
 * <p>
 * Allgemeines Interface für eine ProfilView
 * </p>
 * <p>
 * Enthält eine Referenz auf ein {@link org.kalypso.model.wspm.core.profil.IProfil}
 * und eine Referenz auf ein {@link org.kalypso.model.wspm.ui.profil.view.ProfilViewData}
 * </p>
 * 
 * @author gernot
 * 
 */
public interface IProfilView extends IProfilViewProvider
{
  /**
   * <p>
   * Creates control of this view and returns it
   * </p>
   */
  public Control createControl( final Composite parent, final int style );

  /**
   * Return the control of this view.
   */
  public Control getControl( );

  public void dispose( );
}
