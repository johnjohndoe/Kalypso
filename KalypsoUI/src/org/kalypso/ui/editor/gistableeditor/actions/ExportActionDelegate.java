package org.kalypso.ui.editor.gistableeditor.actions;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.kalypso.ogc.gml.table.wizard.ExportTableWizard;

/**
 * @author Belger
 */
public class ExportActionDelegate extends GisTableAbstractActionDelagate
{
  /**
   * @see org.kalypso.ui.editor.gistableeditor.actions.GisTableAbstractActionDelagate#isEnabled(org.eclipse.jface.viewers.ISelection)
   */
  protected boolean isEnabled( final ISelection selection )
  {
    return true;
  }

  /**
   * @see org.kalypso.ui.editor.gistableeditor.actions.GisTableAbstractActionDelagate#isChecked()
   */
  protected boolean isChecked()
  {
    return false;
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( final IAction action )
  {
    final Wizard exportWizard = new ExportTableWizard( getEditor().getLayerTable() );

    final WizardDialog dialog = new WizardDialog( getEditor().getSite().getShell(), exportWizard );
    dialog.open();
  }
}