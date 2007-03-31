/**
 * 
 */
package org.kalypso.wizards.import1d2d;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d;
import org.kalypso.ui.wizards.imports.INewWizardKalypsoImport;
import org.kalypso.ui.wizards.imports.ISzenarioSourceProvider;
import org.kalypso.ui.wizards.imports.Messages;

import de.renew.workflow.cases.ICaseDataProvider;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class ImportWizard extends Wizard implements INewWizardKalypsoImport
{
  protected DataContainer m_data; // the data model

  protected PageMain m_pageMain;

  protected final ICoreRunnableWithProgress m_operation;

  // workbench selection when the wizard was started
  protected IStructuredSelection selection;

  // flag indicated whether the wizard can be completed or not
  protected boolean wizardCompleted = false;

  public ImportWizard( )
  {
    super();
    m_data = new DataContainer();
    setNeedsProgressMonitor( true );
    m_operation = new Transformer( m_data ); //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
   *      org.eclipse.jface.viewers.IStructuredSelection)
   */
  public void init( IWorkbench iWorkbench, IStructuredSelection iSelection )
  {
    setWindowTitle( Messages.getString( "org.kalypso.wizards.import1d2d.ImportWizard.Title" ) ); //$NON-NLS-1$
    selection = iSelection;
  }

  /**
   * @see org.kalypso.ui.wizards.imports.INewWizardKalypsoImport#initModelProperties(java.util.HashMap)
   */
  public void initModelProperties( IEvaluationContext context )
  {
    final ICaseDataProvider szenarioDataProvider = (ICaseDataProvider) context.getVariable( ISzenarioSourceProvider.ACTIVE_SZENARIO_DATA_PROVIDER_NAME );
    IFEDiscretisationModel1d2d model;
    try
    {
      model = (IFEDiscretisationModel1d2d) szenarioDataProvider.getModel( IFEDiscretisationModel1d2d.class );
      m_data.setFE1D2DDiscretisationModel( model );
    }
    catch( CoreException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    // FE1D2DDiscretisationModel feature = (FE1D2DDiscretisationModel) context.get( "IFEDiscretisationModel1d2d" );
    // final IFolder currentFolder = (IFolder) context.getVariable(
    // ISzenarioSourceProvider.ACTIVE_SZENARIO_FOLDER_NAME);
    // m_data.setProjectBaseFolder( currentFolder );
  }

  @Override
  public void addPages( )
  {
    m_pageMain = new PageMain( m_data );
    addPage( m_pageMain );
  }

  @Override
  public boolean canFinish( )
  {
    return m_pageMain.isPageComplete();
  }

  @Override
  public boolean performCancel( )
  {
    try
    {
      IResource resource = (IResource) selection.getFirstElement();
      if( resource != null )
        resource.getProject().refreshLocal( IResource.DEPTH_INFINITE, null );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    return true;
  }

  @Override
  public boolean performFinish( )
  {
    m_pageMain.saveDataToModel();
    final IStatus status = RunnableContextHelper.execute( getContainer(), true, true, m_operation );
    ErrorDialog.openError( getShell(), getWindowTitle(), "", status );
    // try
    // {
    // IResource resource = (IResource) selection.getFirstElement();
    // resource.getProject().refreshLocal( IResource.DEPTH_INFINITE, null );
    // }
    // catch( Exception e )
    // {
    // e.printStackTrace();
    // }

    return status.isOK();
  }

}
