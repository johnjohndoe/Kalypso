package org.kalypso.ui.calcwizard;

import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;

/**
 * @author belger
 */
public class CalcWizardDialog extends WizardDialog
{
  public CalcWizardDialog( final Shell parentShell, final CalcWizard newWizard )
  {
    super( parentShell, newWizard );
  }

  protected void backPressed()
  {
    if( !((CalcWizard)getWizard()).doBack( getCurrentPage() ) )
      return;
    
    super.backPressed();
  }
  protected void nextPressed()
  {
    if( !((CalcWizard)getWizard()).doNext( getCurrentPage() ) )
      return;
    
    super.nextPressed();
  }
}
