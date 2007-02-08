/**
 * 
 */
package org.kalypso.ui.wizards.imports.roughness;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class ImportWizard extends Wizard implements INewWizard
{
  protected DataContainer m_data; // the data model

  private PageMain pageMain;

  // workbench selection when the wizard was started
  protected IStructuredSelection selection;

  // flag indicated whether the wizard can be completed or not
  protected boolean wizardCompleted = false;

  public ImportWizard()
  {
    super();
    setWindowTitle( "Shape import" );
    m_data = new DataContainer();
//    m_data.setWorkspace( workspace );
    setNeedsProgressMonitor( true );
  }

  public ImportWizard( GMLWorkspace workspace )
  {
    super();
    setWindowTitle( "Shape import" );
    m_data = new DataContainer();
    m_data.setWorkspace( workspace );
    setNeedsProgressMonitor( true );
  }

  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
   *      org.eclipse.jface.viewers.IStructuredSelection)
   */
  public void init( IWorkbench iWorkbench, IStructuredSelection iSelection )
  {
  }

  @Override
  public void addPages( )
  {
    pageMain = new PageMain( m_data );
    addPage( pageMain );
  }

  @Override
  public boolean canFinish( )
  {
//    WizardPage pageSecond = m_data.isOutputToFile() ? pageOutputFile : pageOutputFeature;
//    wizardCompleted = pageMain.canFlipToNextPage() && pageSecond.isPageComplete();
//    return wizardCompleted;
    return true;
  }

  @Override
  public boolean performFinish( )
  {
    pageMain.saveDataToModel();
    final ICoreRunnableWithProgress operation = new TransformerShapeToIRoughnessCollection( m_data ); //$NON-NLS-1$
    final IStatus status = RunnableContextHelper.execute( getContainer(), true, true, operation );
    ErrorDialog.openError( getShell(), getWindowTitle(), "", status );
    return status.isOK();
  }

}
