/**
 * 
 */
package org.kalypso.model.wspm.ui.view.table;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.ui.IViewActionDelegate;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.actions.ActionDelegate;
import org.kalypso.model.wspm.ui.profil.wizard.pointsInsert.InsertPointsWizard;


/**
 * @author Belger
 */
public class InsertPointsActionDelegate extends ActionDelegate implements IViewActionDelegate
{
  private TableView m_view;

  /**
   * @see org.eclipse.ui.IViewActionDelegate#init(org.eclipse.ui.IViewPart)
   */
  public void init( final IViewPart view )
  {
    m_view = (TableView)view;
  }

  @Override
  public void run( final IAction action )
  {
    final InsertPointsWizard wizard = new InsertPointsWizard( m_view.getView().getProfilEventManager());
    final WizardDialog dialog = new WizardDialog( m_view.getViewSite().getShell(), wizard );
    dialog.open();
  }
}
