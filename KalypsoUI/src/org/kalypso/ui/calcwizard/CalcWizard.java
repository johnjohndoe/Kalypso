package org.kalypso.ui.calcwizard;

import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.Wizard;


public class CalcWizard extends Wizard
{
  public boolean performFinish()
  {
    final IWizardPage[] pages = getPages();
    for( int i = 0; i < pages.length; i++ )
    {
      final ICalcWizardPage page = (ICalcWizardPage)pages[i];
      final boolean bFinished = page.performFinish();
      if( bFinished == false )
        return false;
    }
    
    return true;
  }
}