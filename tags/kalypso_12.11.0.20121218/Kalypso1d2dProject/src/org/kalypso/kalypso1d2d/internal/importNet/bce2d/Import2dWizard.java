/**
 *
 */
package org.kalypso.kalypso1d2d.internal.importNet.bce2d;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.wizard.Wizard;
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
public class Import2dWizard extends Wizard
{
  public static final String TITLE = Messages.getString( "org.kalypso.kalypso1d2d.internal.bce2d.imports.Import2dWizard.Title" ); //$NON-NLS-1$

  private final Import2dData m_data;

  private final ICoreRunnableWithProgress m_operation;

  public Import2dWizard( )
  {
    setDialogSettings( DialogSettingsUtils.getDialogSettings( Kalypso1d2dProjectPlugin.getDefault(), getClass().getName() ) );

    m_data = new Import2dData();

    final IScenarioDataProvider szenarioDataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
    m_data.init( szenarioDataProvider, getDialogSettings() );

    m_operation = new Import2dOperation( m_data ); //$NON-NLS-1$

    setWindowTitle( TITLE );
    setNeedsProgressMonitor( true );

    setDialogSettings( DialogSettingsUtils.getDialogSettings( Kalypso1d2dProjectPlugin.getDefault(), getClass().getName() ) );

    addPage( new Import2dPage( m_data ) );
  }

  @Override
  public boolean performFinish( )
  {
    m_data.save( getDialogSettings() );

    // FIXME: canceling is not correctly implemented: elements are added but the models are not dirty; leads to strange
    // behavior
    final IStatus status = RunnableContextHelper.execute( getContainer(), true, false, m_operation );

    StatusDialog.open( getShell(), status, getWindowTitle() );
    return !status.matches( IStatus.ERROR );
  }
}