/**
 * 
 */
package org.kalypso.kalypso1d2d.internal.bce2d.imports;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.kalypso1d2d.internal.bce2d.i18n.Messages;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;

import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class Import2dWizard extends Wizard implements INewWizard
{
  private final Import2dData m_data;

  private final ICoreRunnableWithProgress m_operation;

  public Import2dWizard( )
  {
    m_data = new Import2dData();
    m_operation = new Import2dOperation( m_data ); //$NON-NLS-1$

    setWindowTitle( Messages.getString( "org.kalypso.kalypso1d2d.internal.bce2d.imports.Import2dWizard.Title" ) ); //$NON-NLS-1$
    setNeedsProgressMonitor( true );

    setDialogSettings( DialogSettingsUtils.getDialogSettings( Kalypso1d2dProjectPlugin.getDefault(), getClass().getName() ) );

    addPage( new Import2dPage( m_data ) );
  }

  @Override
  public void init( final IWorkbench workbench, final IStructuredSelection iSelection )
  {
    final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
    final IEvaluationContext context = handlerService.getCurrentState();
    final SzenarioDataProvider szenarioDataProvider = (SzenarioDataProvider) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );

    m_data.init( szenarioDataProvider, getDialogSettings() );
  }

  @Override
  public boolean performFinish( )
  {
    m_data.save( getDialogSettings() );

    final IStatus status = RunnableContextHelper.execute( getContainer(), true, true, m_operation );

    new StatusDialog( getShell(), status, getWindowTitle() ).open();
    return !status.matches( IStatus.ERROR );
  }
}