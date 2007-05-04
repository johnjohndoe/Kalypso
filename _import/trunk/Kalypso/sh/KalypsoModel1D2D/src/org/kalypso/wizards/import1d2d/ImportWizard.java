/**
 * 
 */
package org.kalypso.wizards.import1d2d;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.ui.wizards.imports.ISzenarioSourceProvider;
import org.kalypso.ui.wizards.imports.Messages;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

import de.renew.workflow.cases.ICaseDataProvider;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class ImportWizard extends Wizard implements INewWizard
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
  public void init( IWorkbench workbench, IStructuredSelection iSelection )
  {
    final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
    final IEvaluationContext context = handlerService.getCurrentState();
    final ICaseDataProvider<IFeatureWrapper2> szenarioDataProvider = (ICaseDataProvider<IFeatureWrapper2>) context.getVariable( ISzenarioSourceProvider.ACTIVE_SZENARIO_DATA_PROVIDER_NAME );
    m_data.setSzenarioDataProvider( szenarioDataProvider );
    setWindowTitle( Messages.getString( "org.kalypso.wizards.import1d2d.ImportWizard.Title" ) ); //$NON-NLS-1$
    selection = iSelection;
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

    try
    {
      /* post empty command(s) in order to make pool dirty. */
      m_data.postCommand( IFEDiscretisationModel1d2d.class, new EmptyCommand( "Profile importieren", false ) );
    }
    catch( final Exception e )
    {
      // will never happen?
      e.printStackTrace();
    }

    ErrorDialog.openError( getShell(), getWindowTitle(), "", status );

    return status.isOK();
  }

}
