package org.kalypso.model.wspm.pdb.ui.internal.admin.attachments;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.IPageChangeProvider;
import org.eclipse.jface.dialogs.IPageChangedListener;
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
import org.kalypso.core.status.StatusDialog2;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.IPdbOperation;
import org.kalypso.model.wspm.pdb.ui.internal.ExecutorRunnable;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.ui.internal.content.IConnectionViewer;

public class ImportAttachmentsWizard extends Wizard implements IWorkbenchWizard
{
  private final IPageChangedListener m_pageListener = new IPageChangedListener()
  {
    @Override
    public void pageChanged( final PageChangedEvent event )
    {
      handlePageChange( (IWizardPage) event.getSelectedPage() );
    }
  };

  private ImportAttachmentsData m_data;

  public ImportAttachmentsWizard( )
  {
    setNeedsProgressMonitor( true );
    setDialogSettings( DialogSettingsUtils.getDialogSettings( WspmPdbUiPlugin.getDefault(), getClass().getName() ) );
    setWindowTitle( "Import Attachments" );
  }

  @Override
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    final IWorkbenchPart activePart = workbench.getActiveWorkbenchWindow().getActivePage().getActivePart();
    final IConnectionViewer viewer = (IConnectionViewer) activePart;

    final IPdbConnection connection = viewer.getConnection();

    m_data = new ImportAttachmentsData( connection );
    m_data.init( selection, getDialogSettings() );
  }

  @Override
  public void addPages( )
  {
    addPage( new ImportAttachmentsOptionsPage( "optionsPage", m_data ) ); //$NON-NLS-1$
    addPage( new ImportAttachmentsPreviewPage( "previewPage", m_data ) ); //$NON-NLS-1$
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
      ((IPageChangeProvider) oldContainer).removePageChangedListener( m_pageListener );

    super.setContainer( wizardContainer );

    if( wizardContainer instanceof IPageChangeProvider )
      ((IPageChangeProvider) wizardContainer).addPageChangedListener( m_pageListener );
  }

  protected void handlePageChange( final IWizardPage page )
  {
    if( page instanceof IUpdateable )
      ((IUpdateable) page).update();
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

    final IPdbConnection connection = m_data.getConnection();
    final IPdbOperation operation = new ImportAttachmentsOperation( m_data );

    final ExecutorRunnable runnable = new ExecutorRunnable( connection, operation );

    final IStatus status = RunnableContextHelper.execute( getContainer(), true, false, runnable );
    if( !status.isOK() )
      new StatusDialog2( getShell(), status, getWindowTitle() ).open();

    return !status.matches( IStatus.ERROR );
  }

  private void storeData( )
  {
    final IDialogSettings settings = getDialogSettings();
    if( settings != null )
      m_data.store( settings );
  }
}