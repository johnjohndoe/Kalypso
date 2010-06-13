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
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.wizards.i18n.Messages;

import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class Import2dWizard extends Wizard implements INewWizard
{
  protected DataContainer m_data; // the data model

  protected PageMain m_pageMain;

  protected final ICoreRunnableWithProgress m_operation;

  // workbench selection when the wizard was started
  protected IStructuredSelection selection;

  // flag indicated whether the wizard can be completed or not
  protected boolean wizardCompleted = false;

  public Import2dWizard( )
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
  @Override
  public void init( IWorkbench workbench, IStructuredSelection iSelection )
  {
    final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
    final IEvaluationContext context = handlerService.getCurrentState();
    final SzenarioDataProvider szenarioDataProvider = (SzenarioDataProvider) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
    m_data.setSzenarioDataProvider( szenarioDataProvider );
    setWindowTitle( Messages.getString( "org.kalypso.wizards.import1d2d.Import2dWizard.Title" ) ); //$NON-NLS-1$
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
      final IResource resource = (IResource) selection.getFirstElement();
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
    if( status.isOK() )
      try
      {
        /* post empty command(s) in order to make pool dirty. */
        m_data.postCommand( IFEDiscretisationModel1d2d.class, new EmptyCommand(Messages.getString( "org.kalypso.wizards.import1d2d.Import2dWizard.1" ), false ) ); //$NON-NLS-1$
      }
      catch( final Exception e )
      {
        // will never happen?
        e.printStackTrace();
      }

    ErrorDialog.openError( getShell(), getWindowTitle(), "", status ); //$NON-NLS-1$

    return status.isOK();
  }

}
