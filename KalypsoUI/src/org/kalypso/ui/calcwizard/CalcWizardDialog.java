/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
  
---------------------------------------------------------------------------------------------------*/
package org.kalypso.ui.calcwizard;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.wizard.IWizard;
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

    final Button backButton = getButton( IDialogConstants.BACK_ID );
    final Button newPrognoseButton = getButton(  NEW_PROGNOSE_ID );
    final Button finishedButton = getButton( IDialogConstants.FINISH_ID );
    final Button cancelButton = getButton( IDialogConstants.CANCEL_ID );

    // lock if locked
    final IWizard wizard = getWizard();
    if( wizard instanceof CalcWizard )
    {
      if( ((CalcWizard)wizard).isButtonsLocked() )
      {
        backButton.setEnabled( false );
        getButton( IDialogConstants.NEXT_ID ).setEnabled( false );
        cancelButton.setEnabled( false );
        finishedButton.setEnabled( false );
        getButton( IDialogConstants.HELP_ID ).setEnabled( false );
        newPrognoseButton.setEnabled( false );
        
        return;
      }

      // der cancel button reaktiviert sich nicht von alleine
      cancelButton.setEnabled( true );
    }
    
    final IWizardPage currentPage = getCurrentPage();
    final IWizardPage previousPage = currentPage.getPreviousPage();
    
    backButton.setEnabled( previousPage != null && previousPage instanceof IModelWizardPage );
    newPrognoseButton.setEnabled( currentPage instanceof IModelWizardPage );
  }
  
}
