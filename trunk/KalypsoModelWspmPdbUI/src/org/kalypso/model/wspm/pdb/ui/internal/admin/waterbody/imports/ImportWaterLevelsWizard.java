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
package org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody.imports;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
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
import org.kalypso.contribs.eclipse.ui.dialogs.IGenericWizard;
import org.kalypso.core.status.StatusDialog2;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.IPdbOperation;
import org.kalypso.model.wspm.pdb.connect.command.ExecutorRunnable;
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiImages;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.ui.internal.admin.event.EditEventPage;
import org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody.ChooseWaterPage;
import org.kalypso.model.wspm.pdb.ui.internal.content.ElementSelector;
import org.kalypso.model.wspm.pdb.ui.internal.content.IConnectionViewer;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;
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
      handlePageChange( (IWizardPage) event.getSelectedPage() );
    }
  };

  private SelectShapeFilePage m_shapeFilePage;

  private ImportWaterLevelsData m_data;

  private IConnectionViewer m_viewer;

  private EditEventPage m_eventPage;

  public ImportWaterLevelsWizard( )
  {
    setWindowTitle( Messages.getString("ImportWaterLevelsWizard.0") ); //$NON-NLS-1$
    setDialogSettings( DialogSettingsUtils.getDialogSettings( WspmPdbUiPlugin.getDefault(), getClass().getName() ) );
    setNeedsProgressMonitor( true );
  }

  @Override
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    final IWorkbenchPart activePart = workbench.getActiveWorkbenchWindow().getActivePage().getActivePart();
    m_viewer = (IConnectionViewer) activePart;

    final IPdbConnection connection = m_viewer.getConnection();

    m_data = new ImportWaterLevelsData( connection );
  }

  @Override
  public void addPages( )
  {
    m_shapeFilePage = new SelectShapeFilePage( "selectPage", Messages.getString("ImportWaterLevelsWizard.1"), WspmPdbUiImages.IMG_WIZBAN_IMPORT_WIZ ); //$NON-NLS-1$ //$NON-NLS-2$

    m_shapeFilePage.setDescription( Messages.getString("ImportWaterLevelsWizard.2") ); //$NON-NLS-1$
    addPage( m_shapeFilePage );

    /* Page to choose a water body */
    final Event event = m_data.getEvent();

    final IObservableValue waterValue = BeansObservables.observeValue( event, Event.PROPERTY_WATER_BODY );

    addPage( new ImportWaterlevelsSelectAttributesPage( "selectAttributes", m_data ) ); //$NON-NLS-1$
    addPage( new ImportWaterlevelsPreviewPage( "previewPage", m_data ) ); //$NON-NLS-1$

    /* Choose water body */
    final IPdbConnection connection = m_data.getConnection();
    final ChooseWaterPage waterPage = new ChooseWaterPage( "waterPage", connection, waterValue ); //$NON-NLS-1$
    waterPage.setDescription( Messages.getString("ImportWaterLevelsWizard.3") ); //$NON-NLS-1$
    addPage( waterPage );

    /* Edit event properties */
    m_eventPage = new EditEventPage( "eventPage", m_data, true ); //$NON-NLS-1$
    addPage( m_eventPage );
  }

  @Override
  public IStatus postInit( final IProgressMonitor monitor ) throws InvocationTargetException
  {
    try
    {
      monitor.beginTask( Messages.getString("ImportWaterLevelsWizard.5"), IProgressMonitor.UNKNOWN ); //$NON-NLS-1$
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
      ((IPageChangeProvider) oldContainer).removePageChangedListener( m_pageListener );

    super.setContainer( wizardContainer );

    if( wizardContainer instanceof IPageChangeProvider )
      ((IPageChangeProvider) wizardContainer).addPageChangedListener( m_pageListener );
  }

  protected void handlePageChange( final IWizardPage page )
  {
    final String shapeFile = m_shapeFilePage.getShapeFile();
    final String srs = m_shapeFilePage.getSoureCRS();
    m_data.setShapeInput( shapeFile, srs );

    if( page instanceof IUpdateable )
      ((IUpdateable) page).update();
  }

  @Override
  public boolean performFinish( )
  {
    final Event event = m_data.getEvent();

    final IPdbConnection connection = m_data.getConnection();
    final String username = connection.getSettings().getUsername();
    final IPdbOperation operation = new SaveEventOperation( event, username );

    final ExecutorRunnable runnable = new ExecutorRunnable( connection, operation );

    final IStatus status = RunnableContextHelper.execute( getContainer(), true, false, runnable );
    if( !status.isOK() )
      new StatusDialog2( getShell(), status, getWindowTitle() ).open();

    /* Select new element in tree */
    final ElementSelector selector = new ElementSelector();
    selector.addEventName( event.getName() );
    m_viewer.reload( selector );

    return !status.matches( IStatus.ERROR );
  }
}