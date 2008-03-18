package org.kalypso.model.wspm.ui.action;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.actions.ActionDelegate;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.model.wspm.ui.Messages;
import org.kalypso.model.wspm.ui.wizard.FeatureThemeWizardUtilitites;
import org.kalypso.model.wspm.ui.wizard.IntersectRoughnessWizard;
import org.kalypso.model.wspm.ui.wizard.FeatureThemeWizardUtilitites.FOUND_PROFILES;

public class IntersectRoughnessMapThemeAction extends ActionDelegate
{
  private ISelection m_selection;

  /**
   * @see org.eclipse.ui.actions.ActionDelegate#runWithEvent(org.eclipse.jface.action.IAction,
   *      org.eclipse.swt.widgets.Event)
   */
  @Override
  public void runWithEvent( final IAction action, final Event event )
  {
    /* retrieve selected profiles, abort if none */
    final FOUND_PROFILES foundProfiles = FeatureThemeWizardUtilitites.getProfileFeaturesFromThemeSelection( m_selection );
    
    final Shell shell = event.display.getActiveShell();

    if( foundProfiles == null || foundProfiles.foundProfiles.length == 0 )
    {
      MessageDialog.openWarning( shell, Messages.IntersectRoughnessMapThemeAction_0, Messages.IntersectRoughnessMapThemeAction_1 );
      return;
    }

    final IWizard intersectWizard = new IntersectRoughnessWizard( foundProfiles );

    /* show intersection wizard */
    final WizardDialog2 dialog = new WizardDialog2( shell, intersectWizard );
    dialog.setRememberSize( true );
    dialog.open();
  }

  /**
   * @see org.eclipse.ui.actions.ActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  @Override
  public void selectionChanged( final IAction action, final ISelection selection )
  {
    m_selection = selection;
  }
}
