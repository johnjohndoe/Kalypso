package org.kalypso.ui.calcwizard;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.ui.calcwizard.modelpages.IModelWizardPage;

/**
 * @author belger
 */
public class CalcWizardDialog extends WizardDialog
{
  public final static int NEW_PROGNOSE_ID = IDialogConstants.CLIENT_ID + 1;
  
  public CalcWizardDialog( final Shell parentShell, final CalcWizard newWizard )
  {
    super( parentShell, newWizard );
    
    final Point size = parentShell.getSize();
    setMinimumPageSize( (int)( size.x * 0.8 ), (int)( size.y *  0.66 ) );
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
  
  /**
   * @see org.eclipse.jface.wizard.WizardDialog#buttonPressed(int)
   */
  protected void buttonPressed( int buttonId )
  {
    if( buttonId == NEW_PROGNOSE_ID )
    {
      final CalcWizard wizard = (CalcWizard)getWizard();
      wizard.restart();
    }
    
    super.buttonPressed( buttonId );
  }
  
  /**
   * @see org.eclipse.jface.wizard.WizardDialog#createButtonsForButtonBar(org.eclipse.swt.widgets.Composite)
   */
  protected void createButtonsForButtonBar( Composite parent )
  {
    createButton( parent, NEW_PROGNOSE_ID, "Neue Vorhersage anlegen", false );
    
    super.createButtonsForButtonBar( parent );
  }
  
  /**
   * @see org.eclipse.jface.wizard.WizardDialog#updateButtons()
   */
  public void updateButtons()
  {
    super.updateButtons();
    
    final IWizardPage currentPage = getCurrentPage();

    final IWizardPage previousPage = currentPage.getPreviousPage();
    
    final Button backButton = getButton( IDialogConstants.BACK_ID );
    final Button newPrognoseButton = getButton(  NEW_PROGNOSE_ID );

    backButton.setEnabled( previousPage != null && previousPage instanceof IModelWizardPage );
    
//    if( currentPage instanceof IModelWizardPage )
//    {
//      if( previousPage != null )
//    }
//    else
//      backButton.setEnabled( false );
    
    newPrognoseButton.setEnabled( currentPage instanceof IModelWizardPage );
  }
  
}
