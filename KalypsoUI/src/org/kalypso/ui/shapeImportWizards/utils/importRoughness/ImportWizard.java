/**
 * 
 */
package org.kalypso.ui.shapeImportWizards.utils.importRoughness;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 *
 */
public class ImportWizard extends Wizard implements INewWizard {
  PageMain pageMain;		// wizard page
//  PageStyleSelection pageStyleSelection;
  DataContainer m_data;	// the data model
  protected IStructuredSelection selection;	// workbench selection when the wizard was started
  protected boolean wizardCompleted = false;	// flag indicated whether the wizard can be completed or not
  protected IWorkbench workbench;	// the workbench instance - NOT USED HERE

  public ImportWizard() {
    super();
    m_data = new DataContainer();
    setNeedsProgressMonitor(true);
  }

  /**
   * @see IWorkbenchWizard#init(IWorkbench, IStructuredSelection)
   */
  public void init(IWorkbench workbench, IStructuredSelection selection) 
  {
    this.workbench = workbench;
    this.selection = selection;
  }

  @Override
  public void addPages()
  {
    pageMain = new PageMain(selection);
//    pageStyleSelection = new PageStyleSelection();
    addPage(pageMain);
//    addPage(pageStyleSelection);
  }

  @Override
  public boolean canFinish()
  {
    wizardCompleted = pageMain.cmb_ShapeProperty.isEnabled() && pageMain.isTextNonEmpty(pageMain.txt_OutputFile);
    return wizardCompleted;
  }

  @Override
  public boolean performFinish() 
  {
    pageMain.saveDataToModel();
    final ICoreRunnableWithProgress operation = new TransformerShapeToIRoughnessCollection( m_data); //$NON-NLS-1$
    final IStatus status = RunnableContextHelper.execute( getContainer(), true, true, operation);
    ErrorDialog.openError(getShell(), getWindowTitle(), "", status );
    return status.isOK();
  }
}
