package org.kalypso.eclipse.jface.dialogs;

import org.eclipse.jface.dialogs.IInputValidator;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

/**
 * @author belger
 */
public class PasswordDialog extends InputDialog
{
  public PasswordDialog( final Shell parentShell, final String dialogTitle,
      final String dialogMessage )
  {
    super( parentShell, dialogTitle, dialogMessage, "", new PasswordValidator() );
  }

  protected Control createDialogArea( Composite parent )
  {
    final Control composite = super.createDialogArea(parent);
    
    getText().setEchoChar( '*' );
    
    return composite;
  }

  private static class PasswordValidator implements IInputValidator
  {
    /**
     * @see org.eclipse.jface.dialogs.IInputValidator#isValid(java.lang.String)
     */
    public String isValid( final String newText )
    {
      if( newText == null || newText.length() == 0 )
        return "Passwort darf nicht leer sein";

      return null;
    }
  }

}