package org.kalypso.model.flood.ui.dialogs;

import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

/**
 * @author Thomas Jung
 * 
 */
public final class WaterlevelDialog extends TitleAreaDialog
{
  public WaterlevelDialog( Shell parentShell )
  {
    super( parentShell );
  }

  /**
   * @see org.eclipse.jface.dialogs.TitleAreaDialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createDialogArea( Composite parent )
  {
    final Composite panel = (Composite) super.createDialogArea( parent );

    return panel;
  }
}