/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.tuhh.ui.imports.ctripple;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.PageChangedEvent;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.IWizardContainer;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWizard;
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypso.contribs.eclipse.jface.dialog.WizardPageChangedListener;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.tuhh.ui.imports.WspmTuhhProjectSelection;

/**
 * @author Holger Albert
 */
public class CodedTrippleImportWizard extends Wizard implements IWorkbenchWizard
{
  private final WizardPageChangedListener m_pageListener = new WizardPageChangedListener()
  {
    @Override
    public void pageChanged( final PageChangedEvent event )
    {
      handlePageChanged( event.getSelectedPage() );
    }
  };

  private AbstractCodedTrippleWorker m_worker;

  private CodedTrippleImportData m_data;

  private CodedTrippleImportFilesPage m_importFilesPage;

  private CodedTripplePreviewProfilesPage m_profilesPreviewPage;

  public CodedTrippleImportWizard( )
  {
    m_worker = null;
    m_data = null;
    m_importFilesPage = null;
    m_profilesPreviewPage = null;

    setDialogSettings( DialogSettingsUtils.getDialogSettings( KalypsoModelWspmTuhhUIPlugin.getDefault(), "codedTrippleImportWizard" ) ); //$NON-NLS-1$
    setWindowTitle( Messages.getString( "CodedTrippleImportWizard.0" ) ); //$NON-NLS-1$
    setNeedsProgressMonitor( true );
  }

  @Override
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    final WspmTuhhProjectSelection projectSelection = new WspmTuhhProjectSelection( selection );
    if( !projectSelection.hasProject() )
      throw new IllegalArgumentException( Messages.getString( "CodedTrippleImportWizard.1" ) ); //$NON-NLS-1$

    m_worker = new CodedTrippleWorker( projectSelection.getWorkspace(), projectSelection.getProject() );
    m_data = new CodedTrippleImportData();
    m_data.init( getDialogSettings() );
  }

  @Override
  public void addPages( )
  {
    m_importFilesPage = new CodedTrippleImportFilesPage( m_data );
    m_profilesPreviewPage = new CodedTripplePreviewProfilesPage( m_data );

    addPage( m_importFilesPage );
    addPage( m_profilesPreviewPage );
  }

  @Override
  public void setContainer( final IWizardContainer container )
  {
    m_pageListener.setContainer( getContainer(), container );

    super.setContainer( container );
  }

  @Override
  public boolean canFinish( )
  {
    /* Do not allow to finish early. */
    final IWizardPage currentPage = getContainer().getCurrentPage();
    if( currentPage.getNextPage() != null )
      return false;

    return super.canFinish();
  }

  @Override
  public boolean performFinish( )
  {
    /* Save the dialog settings. */
    m_data.storeSettings( getDialogSettings() );

    final CodedTrippleImportOperation operation = new CodedTrippleImportOperation( m_worker, m_data );
    final IStatus result = RunnableContextHelper.execute( getContainer(), true, true, operation );
    if( !result.isOK() )
      StatusDialog.open( getShell(), result, getWindowTitle() );

    return !result.matches( IStatus.ERROR );
  }

  protected void handlePageChanged( final Object selectedPage )
  {
    if( selectedPage == m_profilesPreviewPage )
    {
      final CodedTrippleCreateProfilesOperation operation = new CodedTrippleCreateProfilesOperation( m_data );
      final IStatus status = RunnableContextHelper.execute( getContainer(), true, true, operation );
      if( !status.isOK() )
        new StatusDialog( getShell(), status, getWindowTitle() ).open();

      m_profilesPreviewPage.updateControls();
    }
  }
}