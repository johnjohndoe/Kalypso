package org.kalypso.ui.rrm.action;

import javax.xml.namespace.QName;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IActionDelegate;
import org.eclipse.ui.PlatformUI;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.model.hydrology.binding.model.KMChannel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypso.ui.rrm.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.kmupdate.KMUpdateWizard;
import org.kalypsodeegree.model.feature.Feature;

public class KMUpdateAction implements IActionDelegate
{
  private IFeatureSelection m_selection = null;

  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  @Override
  public void run( final IAction action )
  {
    if( m_selection == null )
      return;

    final Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
    final Feature feature = m_selection.getFocusedFeature();
    final CommandableWorkspace workspace = m_selection.getWorkspace( feature );

    final KMUpdateWizard kmWizard = new KMUpdateWizard( workspace, m_selection );
    final IDialogSettings dialogSettings = PluginUtilities.getDialogSettings( KalypsoUIRRMPlugin.getDefault(), "kmUpdateAction" );
    kmWizard.setDialogSettings( dialogSettings );

    final WizardDialog2 dialog = new WizardDialog2( shell, kmWizard );
    dialog.setRememberSize( true );
    dialog.open();
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  @Override
  public void selectionChanged( final IAction action, final ISelection selection )
  {
    action.setEnabled( false );
    m_selection = null;
    if( selection instanceof IFeatureSelection )
    {
      m_selection = (IFeatureSelection) selection;
      final Feature feature = (Feature) m_selection.getFirstElement();
      final QName qName = feature.getFeatureType().getQName();
      if( KMChannel.FEATURE_KM_CHANNEL.equals( qName ) )
        action.setEnabled( true );
    }
  }
}
