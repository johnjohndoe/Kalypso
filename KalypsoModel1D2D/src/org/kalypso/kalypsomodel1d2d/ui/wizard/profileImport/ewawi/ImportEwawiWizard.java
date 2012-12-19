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
package org.kalypso.kalypsomodel1d2d.ui.wizard.profileImport.ewawi;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IPageChangeProvider;
import org.eclipse.jface.dialogs.IPageChangedListener;
import org.eclipse.jface.dialogs.PageChangedEvent;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.IWizardContainer;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.ui.IWorkbench;
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.wizard.profileImport.AbstractImportProfileWizard;
import org.kalypso.kalypsomodel1d2d.ui.wizard.profileImport.ImportProfileData;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.imports.ewawi.EwawiCreateProfilesOperation;
import org.kalypso.model.wspm.tuhh.ui.imports.ewawi.EwawiImportData;
import org.kalypso.model.wspm.tuhh.ui.imports.ewawi.EwawiImportFilesPage;
import org.kalypso.model.wspm.tuhh.ui.imports.ewawi.EwawiPreviewProfilesPage;
import org.kalypso.ui.views.map.MapView;

/**
 * @author Holger Albert
 */
public class ImportEwawiWizard extends AbstractImportProfileWizard
{
  private final IPageChangedListener m_pageListener = new IPageChangedListener()
  {
    @Override
    public void pageChanged( final PageChangedEvent event )
    {
      handlePageChanged( event.getSelectedPage() );
    }
  };

  private EwawiImportData m_data;

  private EwawiImportFilesPage m_importFilesPage;

  private EwawiPreviewProfilesPage m_profilesPreviewPage;

  public ImportEwawiWizard( )
  {
    m_data = null;
    m_importFilesPage = null;
    m_profilesPreviewPage = null;

    setDialogSettings( DialogSettingsUtils.getDialogSettings( KalypsoModelWspmTuhhUIPlugin.getDefault(), "ewawiImportWizard" ) ); //$NON-NLS-1$
    setWindowTitle( Messages.getString("ImportEwawiWizard.0") ); //$NON-NLS-1$
    setNeedsProgressMonitor( true );
  }

  @Override
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    m_data = new EwawiImportData();
    m_data.init( getDialogSettings() );
  }

  @Override
  public void addPages( )
  {
    m_importFilesPage = new EwawiImportFilesPage( m_data );
    m_profilesPreviewPage = new EwawiPreviewProfilesPage( m_data );

    addPage( m_importFilesPage );
    addPage( m_profilesPreviewPage );
  }

  @Override
  public void setContainer( final IWizardContainer container )
  {
    final IWizardContainer oldContainer = getContainer();
    if( oldContainer instanceof IPageChangeProvider )
      ((IPageChangeProvider)oldContainer).removePageChangedListener( m_pageListener );

    super.setContainer( container );

    if( container instanceof IPageChangeProvider )
      ((IPageChangeProvider)container).addPageChangedListener( m_pageListener );
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

    final MapView mapView = findMapView();
    final ImportProfileData profileData = new ImportProfileData( mapView );
    final ImportEwawiOperation operation = new ImportEwawiOperation( profileData, m_data );
    final IStatus result = RunnableContextHelper.execute( getContainer(), true, true, operation );
    if( !result.isOK() )
      StatusDialog.open( getShell(), result, getWindowTitle() );

    return !result.matches( IStatus.ERROR );
  }

  protected void handlePageChanged( final Object selectedPage )
  {
    if( selectedPage == m_profilesPreviewPage )
    {
      final EwawiCreateProfilesOperation operation = new EwawiCreateProfilesOperation( m_data );
      final IStatus status = RunnableContextHelper.execute( getContainer(), true, true, operation );
      if( !status.isOK() )
        new StatusDialog( getShell(), status, getWindowTitle() ).open();

      m_profilesPreviewPage.updateControls();
    }
  }
}