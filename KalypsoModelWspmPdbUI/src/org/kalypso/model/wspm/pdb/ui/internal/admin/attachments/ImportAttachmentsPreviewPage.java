/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.wspm.pdb.ui.internal.admin.attachments;

import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.databinding.observable.value.WritableValue;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.databinding.viewers.IViewerObservableValue;
import org.eclipse.jface.databinding.viewers.ViewersObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.IOpenListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.OpenEvent;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.eclipse.ui.internal.WorkbenchMessages;
import org.kalypso.commons.databinding.DataBinder;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.commons.databinding.validation.NumberNotExactValidator;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.contribs.eclipse.jface.viewers.table.ColumnsResizeControlListener;
import org.kalypso.contribs.eclipse.jface.wizard.IUpdateable;
import org.kalypso.contribs.eclipse.swt.widgets.ColumnSortListener;
import org.kalypso.contribs.eclipse.swt.widgets.ColumnViewerSorter;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.core.status.StatusDialog2;
import org.kalypso.model.wspm.pdb.db.mapping.Document;
import org.kalypso.model.wspm.pdb.ui.internal.admin.attachments.ImportAttachmentsDocumentsData.ImportMode;

/**
 * @author Gernot Belger
 */
@SuppressWarnings("restriction")
public class ImportAttachmentsPreviewPage extends WizardPage implements IUpdateable
{
  private final ImportAttachmentsData m_data;

  private CheckboxTableViewer m_viewer;

  private DocumentsCheckstateHandler m_checkStateHandler;

  private DatabindingWizardPage m_binding;

  private final ImportAttachmentsDocumentsData m_documentData;

  public ImportAttachmentsPreviewPage( final String pageName, final ImportAttachmentsData data )
  {
    super( pageName );

    m_data = data;
    m_documentData = m_data.getDocumentData();

    setTitle( "Preview" );
    setDescription( "Preview and select the found documents on this page." );
  }

  @Override
  public void createControl( final Composite parent )
  {
    m_binding = new DatabindingWizardPage( this, null );

    final Composite panel = new Composite( parent, SWT.NONE );
    GridLayoutFactory.swtDefaults().applyTo( panel );
    setControl( panel );

    createDocumentsTable( panel );
    createLowerPanel( panel );
  }

  private void createDocumentsTable( final Composite parent )
  {
    final Table table = new Table( parent, SWT.CHECK | SWT.FULL_SELECTION | SWT.BORDER );
    table.setHeaderVisible( true );
    table.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    m_viewer = new CheckboxTableViewer( table );
    m_viewer.setContentProvider( new ArrayContentProvider() );
    m_checkStateHandler = new DocumentsCheckstateHandler( m_viewer, m_data );
    m_viewer.setCheckStateProvider( m_checkStateHandler );

    // status
    final TableViewerColumn statusColumn = new TableViewerColumn( m_viewer, SWT.LEFT );
    statusColumn.setLabelProvider( new DocumentsStatusProvider( m_documentData ) );
    statusColumn.getColumn().setText( "Status" );
    statusColumn.getColumn().setResizable( false );
    ColumnViewerSorter.registerSorter( statusColumn, new DocumentsStatusComparator( m_documentData ) );
    ColumnsResizeControlListener.setMinimumPackWidth( statusColumn.getColumn() );

    // station
    final TableViewerColumn stationColumn = new TableViewerColumn( m_viewer, SWT.RIGHT );
    stationColumn.setLabelProvider( new DocumentsStationProvider( m_documentData ) );
    stationColumn.getColumn().setText( "Station [km]" );
    stationColumn.getColumn().setResizable( false );
    ColumnViewerSorter.registerSorter( stationColumn, new DocumentsStationComparator( m_documentData ) );
    ColumnsResizeControlListener.setMinimumPackWidth( stationColumn.getColumn() );

    /* name */
    final TableViewerColumn nameColumn = new TableViewerColumn( m_viewer, SWT.LEFT );
    nameColumn.setLabelProvider( new DocumentsNameProvider() );
    nameColumn.getColumn().setText( "Filename" );
    nameColumn.getColumn().setResizable( false );
    ColumnViewerSorter.registerSorter( nameColumn, new DocumentsNameComparator() );
    ColumnsResizeControlListener.setMinimumPackWidth( nameColumn.getColumn() );

    /* mime/type */
    final TableViewerColumn typeColumn = new TableViewerColumn( m_viewer, SWT.LEFT );
    typeColumn.setLabelProvider( new DocumentsTypeProvider() );
    typeColumn.getColumn().setText( "Type" );
    typeColumn.getColumn().setResizable( false );
    ColumnViewerSorter.registerSorter( typeColumn, new DocumentsTypeComparator() );
    ColumnsResizeControlListener.setMinimumPackWidth( typeColumn.getColumn() );

    table.addControlListener( new ColumnsResizeControlListener() );

    ColumnSortListener.setSortState( stationColumn, Boolean.TRUE );

    /* Double-click shows status */
    m_viewer.addOpenListener( new IOpenListener()
    {
      @Override
      public void open( final OpenEvent event )
      {
        handleShowDocumentStatus( (IStructuredSelection) event.getSelection() );
      }
    } );

    m_viewer.addCheckStateListener( m_checkStateHandler );

    final IObservableValue target = new WritableValue();
    final IObservableValue model = BeansObservables.observeValue( m_data, ImportAttachmentsData.PROPERTY_SELECTION_COUNT );

    final DataBinder countBinder = new DataBinder( target, model );
    countBinder.addModelAfterGetValidator( new NumberNotExactValidator( Integer.valueOf( 0 ), IStatus.ERROR, "No document is selected for import" ) );
    m_binding.bindValue( countBinder );
  }

  protected void handleShowDocumentStatus( final IStructuredSelection selection )
  {
    if( selection.isEmpty() )
      return;

    final Object firstElement = selection.getFirstElement();
    if( firstElement instanceof Document )
    {
      final IStatus status = m_documentData.getStatus( (Document) firstElement );
      new StatusDialog( getShell(), status, getWizard().getWindowTitle() ).open();
    }
  }

  private void createLowerPanel( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    GridLayoutFactory.fillDefaults().numColumns( 5 ).applyTo( panel );
    panel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    createImportModelControl( panel );
    new Label( panel, SWT.NONE ).setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    createSelectionButtons( panel );
  }

  private void createImportModelControl( final Composite parent )
  {
    final Label label = new Label( parent, SWT.NONE );
    label.setText( "Existing Documents: " );

    final ComboViewer viewer = new ComboViewer( parent, SWT.DROP_DOWN | SWT.READ_ONLY );
    viewer.setContentProvider( new ArrayContentProvider() );
    viewer.setLabelProvider( new LabelProvider() );
    viewer.setInput( ImportMode.values() );

    final IViewerObservableValue target = ViewersObservables.observeSinglePostSelection( viewer );
    final IObservableValue model = BeansObservables.observeValue( m_data, ImportAttachmentsData.PROPERTY_IMPORT_MODE );
    m_binding.bindValue( target, model );
  }

  private void createSelectionButtons( final Composite parent )
  {
    final DocumentsCheckstateHandler checkStateHandler = m_checkStateHandler;

    final Button selectAllButton = new Button( parent, SWT.PUSH );
    selectAllButton.setText( WorkbenchMessages.SelectionDialog_selectLabel );
    selectAllButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        checkStateHandler.selectAll();
      }
    } );

    final Button deselectAllButton = new Button( parent, SWT.PUSH );
    deselectAllButton.setText( WorkbenchMessages.SelectionDialog_deselectLabel );
    deselectAllButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        checkStateHandler.unselectAll();
      }
    } );
  }

  @Override
  public void update( )
  {
    readDocuments();

    m_data.clearSelection();
    m_viewer.setInput( m_documentData.getDocuments() );
    m_checkStateHandler.selectAll();

    ColumnsResizeControlListener.refreshColumnsWidth( m_viewer.getTable() );
  }

  private void readDocuments( )
  {
    final SearchDocumentsOperation operation = new SearchDocumentsOperation( m_data, m_documentData );

    final IStatus status = RunnableContextHelper.execute( getContainer(), true, false, operation );
    if( !status.isOK() )
    {
      final String windowTitle = getWizard().getWindowTitle();
      new StatusDialog2( getShell(), status, windowTitle ).open();
    }
  }
}