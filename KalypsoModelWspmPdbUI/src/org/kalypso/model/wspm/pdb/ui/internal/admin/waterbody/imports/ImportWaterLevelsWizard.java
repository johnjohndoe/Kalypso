/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
package org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody.imports;

import java.lang.reflect.InvocationTargetException;
import java.util.Collection;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.IPageChangeProvider;
import org.eclipse.jface.dialogs.IPageChangedListener;
import org.eclipse.jface.dialogs.PageChangedEvent;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.IWizardContainer;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWizard;
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.contribs.eclipse.jface.wizard.IUpdateable;
import org.kalypso.contribs.eclipse.ui.dialogs.IGenericWizard;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.command.ExecutorRunnable;
import org.kalypso.model.wspm.pdb.db.PdbInfo;
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.db.utils.WaterBodyUtils;
import org.kalypso.model.wspm.pdb.ui.internal.PdbUiUtils;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiImages;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.ui.internal.admin.event.EditEventPage;
import org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody.ChooseWaterPage;
import org.kalypso.model.wspm.pdb.ui.internal.content.ElementSelector;
import org.kalypso.model.wspm.pdb.ui.internal.content.IConnectionViewer;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;
import org.kalypso.model.wspm.pdb.wspm.SaveEventOperation;
import org.kalypso.ui.wizard.shape.SelectShapeFilePage;

/**
 * @author Gernot Belger
 */
public class ImportWaterLevelsWizard extends Wizard implements IWorkbenchWizard, IGenericWizard
{
  private final IPageChangedListener m_pageListener = new IPageChangedListener()
  {
    @Override
    public void pageChanged( final PageChangedEvent event )
    {
      handlePageChange( (IWizardPage)event.getSelectedPage() );
    }
  };

  private SelectShapeFilePage m_shapeFilePage;

  private ImportWaterLevelsData m_data;

  private IConnectionViewer m_viewer;

  private IObservableValue m_waterValue;

  public ImportWaterLevelsWizard( )
  {
    setWindowTitle( Messages.getString( "ImportWaterLevelsWizard.0" ) ); //$NON-NLS-1$
    setDialogSettings( DialogSettingsUtils.getDialogSettings( WspmPdbUiPlugin.getDefault(), getClass().getName() ) );

    setNeedsProgressMonitor( true );
  }

  @Override
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    try
    {
      m_viewer = PdbUiUtils.getConnectionViewerChecked( workbench );

      final IPdbConnection connection = m_viewer.getConnection();

      m_data = new ImportWaterLevelsData( connection );
    }
    catch( final ExecutionException e )
    {
      e.printStackTrace();
      // TODO: better error handling
    }
  }

  @Override
  public void addPages( )
  {
    m_shapeFilePage = new SelectShapeFilePage( "selectPage", Messages.getString( "ImportWaterLevelsWizard.1" ), WspmPdbUiImages.IMG_WIZBAN_IMPORT_WIZ ); //$NON-NLS-1$ //$NON-NLS-2$
    m_shapeFilePage.setDescription( Messages.getString( "ImportWaterLevelsWizard.2" ) ); //$NON-NLS-1$
    addPage( m_shapeFilePage );

    /* Page to choose a water body */
    final Event event = m_data.getEvent();

    /* guess water and/or state from current selection */
    final WaterBody preferredWater = findPreferredWater( m_viewer.getSelection() );
    final State preferredState = findPreferredState( m_viewer.getSelection() );

    event.setWaterBody( preferredWater );
    event.setState( preferredState );

    m_waterValue = BeansObservables.observeValue( event, Event.PROPERTY_WATER_BODY );

    addPage( new ImportWaterlevelsSelectAttributesPage( "selectAttributes", m_data ) ); //$NON-NLS-1$
    addPage( new ImportWaterlevelsPreviewPage( "previewPage", m_data ) ); //$NON-NLS-1$

    /* Choose water body */
    final IPdbConnection connection = m_data.getConnection();
    final ChooseWaterPage waterPage = new ChooseWaterPage( "waterPage", connection, m_waterValue );
    waterPage.setDescription( Messages.getString( "ImportWaterLevelsWizard.3" ) ); //$NON-NLS-1$
    addPage( waterPage );

    /* Edit event properties */
    addPage( new EditEventPage( "eventPage", m_data, true ) ); //$NON-NLS-1$
  }

  @Override
  public IStatus postInit( final IProgressMonitor monitor ) throws InvocationTargetException
  {
    try
    {
      monitor.beginTask( Messages.getString( "ImportWaterLevelsWizard.5" ), IProgressMonitor.UNKNOWN ); //$NON-NLS-1$
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
  public boolean canFinish( )
  {
    final boolean canFinish = super.canFinish();
    final boolean hasNextPage = getNextPage( getContainer().getCurrentPage() ) != null;

    return canFinish && !hasNextPage;
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
    final String shapeFile = m_shapeFilePage.getShapeFile();
    final String srs = m_shapeFilePage.getSoureCRS();
    m_data.setShapeInput( shapeFile, srs );

    if( page instanceof EditEventPage )
    {
      /* set collection of possible states to this page */
      final Collection<State> possibleStates = getPossibleStates();
      m_data.setStates( possibleStates );
    }

    if( page instanceof IUpdateable )
      ((IUpdateable)page).update();
  }

  private Collection<State> getPossibleStates( )
  {
    final WaterBody value = (WaterBody)m_waterValue.getValue();
    return WaterBodyUtils.getPossibleStates( value );
  }

  @Override
  public boolean performFinish( )
  {
    final IPdbConnection connection = m_data.getConnection();

    final Event event = m_data.getEvent();

    if( !SaveEventOperation.askForEmptyState( event, getShell(), getWindowTitle() ) )
      return false;

    final IStatus status = runOperation( connection, event );
    if( !status.isOK() )
      StatusDialog.open( getShell(), status, getWindowTitle() );

    /* Select new element in tree */
    final ElementSelector selector = new ElementSelector();
    selector.addEventName( event.getName() );
    m_viewer.reload( selector );

    return !status.matches( IStatus.ERROR );
  }

  private IStatus runOperation( final IPdbConnection connection, final Event event )
  {
    final SaveEventOperation operation = createCheckinOperation( connection, event );

    final ExecutorRunnable runnable = new ExecutorRunnable( connection, operation );

    final IStatus status = RunnableContextHelper.execute( getContainer(), true, false, runnable );

    final IStatus log = operation.getLog();

    return createStatus( status, log );
  }

  private IStatus createStatus( final IStatus status1, final IStatus status2 )
  {
    if( !status1.isOK() && !status2.isOK() )
      return new MultiStatus( WspmPdbUiPlugin.PLUGIN_ID, 0, new IStatus[] { status1, status2 }, "Several problems while importing waterlevel", null );

    if( !status1.isOK() )
      return status1;

    if( !status2.isOK() )
      return status1;

    return new Status( IStatus.OK, WspmPdbUiPlugin.PLUGIN_ID, "Waterlevel was successfully uploaded" );
  }

  private SaveEventOperation createCheckinOperation( final IPdbConnection connection, final Event event )
  {
    final String username = connection.getSettings().getUsername();

    final PdbInfo info = connection.getInfo();
    final int dbSRID = info.getSRID();

    return new SaveEventOperation( event, username, dbSRID );
  }

  private WaterBody findPreferredWater( final IStructuredSelection selection )
  {
    final Object firstElement = selection.getFirstElement();

    if( firstElement instanceof WaterBody )
      return (WaterBody)firstElement;

    if( firstElement instanceof State )
    {
      /* Recurse into parent via viewer structure */
      final Object parent = m_viewer.getStructure().getParent( firstElement );
      return findPreferredWater( new StructuredSelection( parent ) );
    }

    if( firstElement instanceof Event )
      return ((Event)firstElement).getWaterBody();

    return null;
  }

  private State findPreferredState( final IStructuredSelection selection )
  {
    final Object firstElement = selection.getFirstElement();

    if( firstElement instanceof State )
      return (State)firstElement;

    if( firstElement instanceof Event )
      return ((Event)firstElement).getState();

    return null;
  }
}