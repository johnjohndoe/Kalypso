/**
 *
 */
package org.kalypso.kalypso1d2d.internal.import2d;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

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
    final IScenarioDataProvider szenarioDataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();

    m_data.init( szenarioDataProvider, getDialogSettings() );
  }

  @Override
  public boolean performFinish( )
  {
    m_data.save( getDialogSettings() );

    // FIXME: canceling is not correctly implemented: elements are added but the models are not dirty; leads to strange
    // behavior
    final IStatus status = RunnableContextHelper.execute( getContainer(), true, false, m_operation );

    new StatusDialog( getShell(), status, getWindowTitle() ).open();
    return !status.matches( IStatus.ERROR );
  }
}