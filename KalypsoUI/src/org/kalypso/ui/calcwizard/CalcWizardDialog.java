package org.kalypso.ui.calcwizard;

import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Shell;

/**
 * @author belger
 */
public class CalcWizardDialog extends WizardDialog
{
  public CalcWizardDialog( final Shell parentShell, final CalcWizard newWizard )
  {
    super( parentShell, newWizard );
    
    final Point size = parentShell.getSize();
    setMinimumPageSize( (int)( size.x * 1.0 ), (int)( size.y *  0.66 ) );
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
