/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.wspm.pdb.ui.internal.admin.gaf;

import java.lang.reflect.InvocationTargetException;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
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
import org.kalypso.contribs.eclipse.ui.dialogs.IGenericWizard;
import org.kalypso.core.status.StatusDialog2;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.gaf.ImportGafData;
import org.kalypso.model.wspm.pdb.gaf.ImportGafOperation;
import org.kalypso.model.wspm.pdb.gaf.ReadGafOperation;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.ui.internal.admin.state.EditStatePage;
import org.kalypso.model.wspm.pdb.ui.internal.admin.state.EditStatePage.Mode;
import org.kalypso.model.wspm.pdb.ui.internal.admin.state.IStatesProvider;
import org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody.ChooseWaterPage;
import org.kalypso.model.wspm.pdb.ui.internal.content.ElementSelector;
import org.kalypso.model.wspm.pdb.ui.internal.content.IConnectionViewer;

public class ImportGafWizard extends Wizard implements IWorkbenchWizard, IStatesProvider, IGenericWizard
{
  private final IPageChangedListener m_pageListener = new IPageChangedListener()
  {
    @Override
    public void pageChanged( final PageChangedEvent event )
    {
      handlePageChanged( event.getSelectedPage() );
    }
  };

  private ImportGafData m_data;

  private GafProfilesPage m_gafProfilesPage;

  private GafOptionsPage m_optionsPage;

  private AddWaterLevelPage m_waterLevelPage;


  private IConnectionViewer m_viewer;

  public ImportGafWizard( )
  {
    final IDialogSettings settings = DialogSettingsUtils.getDialogSettings( WspmPdbUiPlugin.getDefault(), getClass().getName() );

    setDialogSettings( settings );
    setNeedsProgressMonitor( true );
    setWindowTitle( "Import GAF" );
  }

  @Override
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    final IWorkbenchPart activePart = workbench.getActiveWorkbenchWindow().getActivePage().getActivePart();
    m_viewer = (IConnectionViewer) activePart;
    m_data = new ImportGafData( m_viewer.getConnection() );
  }

  @Override
  public IStatus postInit( final IProgressMonitor monitor ) throws InvocationTargetException
  {
    try
    {
      monitor.beginTask( "Initalizing wizard...", IProgressMonitor.UNKNOWN );
      m_data.initFromDb();
      m_data.init( getDialogSettings() );
      return Status.OK_STATUS;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new InvocationTargetException( e );
    }
    finally
    {
      monitor.done();
    }
  }

  @Override
  public void addPages( )
  {
    addPage( new ImportGafPage( "gaf", m_data ) ); //$NON-NLS-1$

    final IPdbConnection connection = m_data.getConnection();
    final IObservableValue waterValue = BeansObservables.observeValue( m_data, ImportGafData.PROPERTY_WATER_BODY );

    final ChooseWaterPage waterPage = new ChooseWaterPage( "waterBody", connection, waterValue ); //$NON-NLS-1$
    waterPage.setDescription( "Choose the water body into which the profiles will be imported" );
    addPage( waterPage );

    m_optionsPage = new GafOptionsPage( "options", m_data ); //$NON-NLS-1$
    addPage( m_optionsPage );

    m_gafProfilesPage = new GafProfilesPage( "profiles", m_data ); //$NON-NLS-1$
    addPage( m_gafProfilesPage );

    final EditStatePage editStatePage = new EditStatePage( "state", m_data.getState(), this, Mode.NEW ); //$NON-NLS-1$
    editStatePage.setTitle( EditStatePage.STR_ENTER_STATE_PROPERTIES );
    editStatePage.setDescription( EditStatePage.STR_ENTER_THE_PROPERTIES_OF_THE_FRESHLY_CREATED_STATE );
    addPage( editStatePage );

    m_waterLevelPage = new AddWaterLevelPage( "addWaterlevel", m_data ); //$NON-NLS-1$
    addPage( m_waterLevelPage );
  }

  @Override
  public void setContainer( final IWizardContainer container )
  {
    final IWizardContainer oldContainer = getContainer();
    if( oldContainer instanceof IPageChangeProvider )
      ((IPageChangeProvider) oldContainer).removePageChangedListener( m_pageListener );

    super.setContainer( container );

    if( container instanceof IPageChangeProvider )
      ((IPageChangeProvider) container).addPageChangedListener( m_pageListener );
  }

  @Override
  public boolean canFinish( )
  {
    /* Do not allow to finish early */
    final IWizardPage currentPage = getContainer().getCurrentPage();
    if( currentPage.getNextPage() != null )
      return false;

    return super.canFinish();
  }

  @Override
  public boolean performFinish( )
  {
    final ImportGafOperation operation = new ImportGafOperation( m_data );
    final IStatus result = RunnableContextHelper.execute( getContainer(), true, true, operation );
    new StatusDialog2( getShell(), result, getWindowTitle() ).open();

    final IDialogSettings settings = getDialogSettings();
    if( settings != null )
      m_data.store( settings );

    if( result.matches( IStatus.ERROR ) )
      return false;

    /* Select new element in tree */
    final ElementSelector selector = new ElementSelector();
    selector.addStateName( m_data.getState().getName() );
    m_viewer.reload( selector );

    return true;
  }

  protected void handlePageChanged( final Object selectedPage )
  {
    if( selectedPage == m_optionsPage )
    {
      final ReadGafOperation operation = new ReadGafOperation( m_data );
      final IStatus status = RunnableContextHelper.execute( getContainer(), true, true, operation );
      if( !status.isOK() )
        new StatusDialog2( getShell(), status, getWindowTitle() ).open();

      m_optionsPage.updateControl();
    }

    if( selectedPage == m_gafProfilesPage )
    {
      m_data.createProfiles();

      m_gafProfilesPage.updateControl();
    }

    if( selectedPage == m_waterLevelPage )
    {
      final Event event = m_data.getWaterlevelEvent();
      if( StringUtils.isBlank( event.getName() ) )
        event.setName( m_data.getState().getName() );

      m_waterLevelPage.updateControl();
    }
  }

  @Override
  public State[] getStates( )
  {
    return m_data.getExistingStates();
  }
}