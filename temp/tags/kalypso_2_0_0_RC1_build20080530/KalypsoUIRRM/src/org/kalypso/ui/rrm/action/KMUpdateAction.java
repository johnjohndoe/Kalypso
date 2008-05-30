package org.kalypso.ui.rrm.action;

import javax.xml.namespace.QName;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IActionDelegate;
import org.eclipse.ui.internal.Workbench;
import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypso.ui.rrm.kmupdate.KMUpdateWizard;
import org.kalypsodeegree.model.feature.Feature;

public class KMUpdateAction implements IActionDelegate
{

  private IFeatureSelection m_selection = null;

  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( IAction action )
  {
    if( m_selection != null )
    {
      final Shell shell = Workbench.getInstance().getActiveWorkbenchWindow().getShell();
      final Feature feature = m_selection.getFocusedFeature();
      final CommandableWorkspace workspace = m_selection.getWorkspace( feature );

      final WizardDialog dialog = new WizardDialog( shell, new KMUpdateWizard( workspace, m_selection ) );
      dialog.open();
    }
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( IAction action, ISelection selection )
  {
    action.setEnabled( false );
    m_selection = null;
    if( selection instanceof IFeatureSelection )
    {
      m_selection = (IFeatureSelection) selection;
      final Feature feature = (Feature) m_selection.getFirstElement();
      final QName qName = feature.getFeatureType().getQName();
      if( NaModelConstants.KM_CHANNEL_ELEMENT_FT.equals( qName ) )
        action.setEnabled( true );
    }
  }
}
