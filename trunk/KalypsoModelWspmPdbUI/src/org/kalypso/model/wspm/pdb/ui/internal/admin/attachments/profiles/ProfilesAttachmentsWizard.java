package org.kalypso.model.wspm.pdb.ui.internal.admin.attachments.profiles;

import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.IPageChangeProvider;
import org.eclipse.jface.dialogs.IPageChangedListener;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.dialogs.PageChangedEvent;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.IWizardContainer;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWizard;
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.contribs.eclipse.jface.wizard.IUpdateable;
import org.kalypso.contribs.eclipse.ui.dialogs.IGenericWizard;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.IPdbOperation;
import org.kalypso.model.wspm.pdb.connect.command.ExecutorRunnable;
import org.kalypso.model.wspm.pdb.db.mapping.Document;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.ui.internal.admin.attachments.ImportAttachmentsOperation;
import org.kalypso.model.wspm.pdb.ui.internal.content.ElementSelector;
import org.kalypso.model.wspm.pdb.ui.internal.content.IConnectionViewer;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;

public class ProfilesAttachmentsWizard extends Wizard implements IWorkbenchWizard, IGenericWizard
{
  private final IPageChangedListener m_pageListener = new IPageChangedListener()
  {
    @Override
    public void pageChanged( final PageChangedEvent event )
    {
      handlePageChange( (IWizardPage)event.getSelectedPage() );
    }
  };

  private ProfilesAttachmentsData m_data;

  private IConnectionViewer m_viewer;

  public ProfilesAttachmentsWizard( )
  {
    setNeedsProgressMonitor( true );
    setDialogSettings( DialogSettingsUtils.getDialogSettings( WspmPdbUiPlugin.getDefault(), getClass().getName() ) );
    setWindowTitle( Messages.getString( "ImportAttachmentsWizard.0" ) ); //$NON-NLS-1$
  }

  @Override
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    final IWorkbenchPart activePart = workbench.getActiveWorkbenchWindow().getActivePage().getActivePart();
    m_viewer = (IConnectionViewer)activePart;

    final IPdbConnection connection = m_viewer.getConnection();

    m_data = new ProfilesAttachmentsData( connection );
    m_data.init( selection, getDialogSettings() );
  }

  @Override
  public IStatus postInit( final IProgressMonitor monitor ) throws CoreException, InvocationTargetException
  {
    final Map<BigDecimal, List<Document>> dbHash = getDbDocuments( monitor );
    m_data.setDbHash( dbHash );
    return Status.OK_STATUS;
  }

  private Map<BigDecimal, List<Document>> getDbDocuments( final IProgressMonitor monitor ) throws CoreException, InvocationTargetException
  {
    /* HINT: The name of the state is used as path. */
    final State state = m_data.getState();
    final String path = state.getName();
    final QueryDocumentsOperation operation = new QueryDocumentsOperation( path );
    final ExecutorRunnable runnable = new ExecutorRunnable( m_data.getConnection(), operation );
    final IStatus result = runnable.execute( monitor );
    if( !result.isOK() )
      throw new CoreException( result );

    return operation.getResult();
  }

  @Override
  public void addPages( )
  {
    addPage( new ProfilesAttachmentsOptionsPage( "optionsPage", m_data ) ); //$NON-NLS-1$
    addPage( new ProfilesAttachmentsPreviewPage( "previewPage", m_data ) ); //$NON-NLS-1$
  }

  @Override
  public boolean canFinish( )
  {
    /* Only finish on last page */
    final IWizardPage currentPage = getContainer().getCurrentPage();
    if( currentPage.getNextPage() != null )
      return false;

    return super.canFinish();
  }

  @Override
  public void setContainer( final IWizardContainer wizardContainer )
  {
    final IWizardContainer oldContainer = getContainer();

    if( oldContainer instanceof IPageChangeProvider )
      ((IPageChangeProvider)oldContainer).removePageChangedListener( m_pageListener );

    super.setContainer( wizardContainer );

    if( wizardContainer instanceof IPageChangeProvider )
      ((IPageChangeProvider)wizardContainer).addPageChangedListener( m_pageListener );
  }

  protected void handlePageChange( final IWizardPage page )
  {
    if( page instanceof IUpdateable )
      ((IUpdateable)page).update();
  }

  @Override
  public boolean performCancel( )
  {
    storeData();

    return super.performCancel();
  }

  @Override
  public boolean performFinish( )
  {
    storeData();

    if( !checkForZip() )
      return false;

    final IPdbConnection connection = m_data.getConnection();
    final IPdbOperation operation = new ImportAttachmentsOperation( m_data );

    final ExecutorRunnable runnable = new ExecutorRunnable( connection, operation );

    final IStatus status = RunnableContextHelper.execute( getContainer(), true, false, runnable );
    if( !status.isOK() )
      new StatusDialog( getShell(), status, getWindowTitle() ).open();

    final ElementSelector selector = new ElementSelector();
    selector.addStateName( m_data.getState().getName() );
    m_viewer.reload( selector );

    return !status.matches( IStatus.ERROR );
  }

  private boolean checkForZip( )
  {
    final File zipFile = m_data.getZipFile();
    if( zipFile == null || !zipFile.exists() )
      return true;

    final String message = String.format( Messages.getString( "ImportAttachmentsWizard.1" ), zipFile.getName() ); //$NON-NLS-1$
    return MessageDialog.openConfirm( getShell(), getWindowTitle(), message );
  }

  private void storeData( )
  {
    final IDialogSettings settings = getDialogSettings();
    if( settings != null )
      m_data.store( settings );
  }
}