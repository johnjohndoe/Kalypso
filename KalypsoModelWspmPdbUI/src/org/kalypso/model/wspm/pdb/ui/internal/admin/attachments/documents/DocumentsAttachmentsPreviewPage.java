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
package org.kalypso.model.wspm.pdb.ui.internal.admin.attachments.documents;

import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.databinding.observable.value.IValueChangeListener;
import org.eclipse.core.databinding.observable.value.ValueChangeEvent;
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
import org.kalypso.model.wspm.pdb.db.mapping.Document;
import org.kalypso.model.wspm.pdb.ui.internal.admin.attachments.AbstractDocumentInfo;
import org.kalypso.model.wspm.pdb.ui.internal.admin.attachments.DocumentsCheckstateHandler;
import org.kalypso.model.wspm.pdb.ui.internal.admin.attachments.DocumentsNameComparator;
import org.kalypso.model.wspm.pdb.ui.internal.admin.attachments.DocumentsNameProvider;
import org.kalypso.model.wspm.pdb.ui.internal.admin.attachments.DocumentsStatusComparator;
import org.kalypso.model.wspm.pdb.ui.internal.admin.attachments.DocumentsStatusProvider;
import org.kalypso.model.wspm.pdb.ui.internal.admin.attachments.DocumentsTypeComparator;
import org.kalypso.model.wspm.pdb.ui.internal.admin.attachments.DocumentsTypeProvider;
import org.kalypso.model.wspm.pdb.ui.internal.admin.attachments.ImportMode;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;

/**
 * @author Gernot Belger
 * @author Holger Albert
 */
@SuppressWarnings( "restriction" )
public class DocumentsAttachmentsPreviewPage extends WizardPage implements IUpdateable
{
  private final DocumentsAttachmentsData m_data;

  private final DocumentsAttachmentsDocumentsData m_documentData;

  private CheckboxTableViewer m_viewer;

  private DocumentsCheckstateHandler m_checkStateHandler;

  private DatabindingWizardPage m_binding;

  public DocumentsAttachmentsPreviewPage( final String pageName, final DocumentsAttachmentsData data )
  {
    super( pageName );

    m_data = data;
    m_documentData = (DocumentsAttachmentsDocumentsData)m_data.getDocumentData();

    setTitle( Messages.getString( "ImportAttachmentsPreviewPage.0" ) ); //$NON-NLS-1$
    setDescription( Messages.getString( "ImportAttachmentsPreviewPage.1" ) ); //$NON-NLS-1$
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

    /* Name. */
    final TableViewerColumn nameColumn = new TableViewerColumn( m_viewer, SWT.LEFT );
    nameColumn.setLabelProvider( new DocumentsNameProvider() );
    nameColumn.getColumn().setText( Messages.getString( "ImportAttachmentsPreviewPage.4" ) ); //$NON-NLS-1$
    nameColumn.getColumn().setResizable( false );
    ColumnViewerSorter.registerSorter( nameColumn, new DocumentsNameComparator() );
    ColumnsResizeControlListener.setMinimumPackWidth( nameColumn.getColumn() );

    /* Mime type. */
    final TableViewerColumn typeColumn = new TableViewerColumn( m_viewer, SWT.LEFT );
    typeColumn.setLabelProvider( new DocumentsTypeProvider() );
    typeColumn.getColumn().setText( Messages.getString( "ImportAttachmentsPreviewPage.5" ) ); //$NON-NLS-1$
    typeColumn.getColumn().setResizable( false );
    ColumnViewerSorter.registerSorter( typeColumn, new DocumentsTypeComparator() );
    ColumnsResizeControlListener.setMinimumPackWidth( typeColumn.getColumn() );

    /* Status. */
    final TableViewerColumn statusColumn = new TableViewerColumn( m_viewer, SWT.LEFT );
    statusColumn.setLabelProvider( new DocumentsStatusProvider( m_documentData ) );
    statusColumn.getColumn().setText( Messages.getString( "ImportAttachmentsPreviewPage.2" ) ); //$NON-NLS-1$
    statusColumn.getColumn().setResizable( false );
    ColumnViewerSorter.registerSorter( statusColumn, new DocumentsStatusComparator( m_documentData ) );
    ColumnsResizeControlListener.setMinimumPackWidth( statusColumn.getColumn() );

    table.addControlListener( new ColumnsResizeControlListener() );

    ColumnSortListener.setSortState( nameColumn, Boolean.TRUE );

    /* Double-click shows status */
    m_viewer.addOpenListener( new IOpenListener()
    {
      @Override
      public void open( final OpenEvent event )
      {
        handleShowDocumentStatus( (IStructuredSelection)event.getSelection() );
      }
    } );

    m_viewer.addCheckStateListener( m_checkStateHandler );

    final IObservableValue target = new WritableValue();
    final IObservableValue model = BeansObservables.observeValue( m_data, DocumentsAttachmentsData.PROPERTY_SELECTION_COUNT );

    final DataBinder countBinder = new DataBinder( target, model );
    countBinder.addModelAfterGetValidator( new NumberNotExactValidator( Integer.valueOf( 0 ), IStatus.ERROR, Messages.getString( "ImportAttachmentsPreviewPage.6" ) ) ); //$NON-NLS-1$
    m_binding.bindValue( countBinder );
  }

  protected void handleShowDocumentStatus( final IStructuredSelection selection )
  {
    if( selection.isEmpty() )
      return;

    final Object firstElement = selection.getFirstElement();
    if( firstElement instanceof Document )
    {
      final AbstractDocumentInfo info = m_documentData.getInfo( (Document)firstElement );
      final IStatus status = info.getStatus();
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
    label.setText( Messages.getString( "ImportAttachmentsPreviewPage.7" ) ); //$NON-NLS-1$

    final ComboViewer viewer = new ComboViewer( parent, SWT.DROP_DOWN | SWT.READ_ONLY );
    viewer.setContentProvider( new ArrayContentProvider() );
    viewer.setLabelProvider( new LabelProvider() );
    viewer.setInput( ImportMode.values() );

    final IViewerObservableValue target = ViewersObservables.observeSinglePostSelection( viewer );
    final IObservableValue model = BeansObservables.observeValue( m_data, DocumentsAttachmentsData.PROPERTY_IMPORT_MODE );
    m_binding.bindValue( target, model );

    // TRICKY: we need to refresh the table when the import mode changes, in order
    // to show the gray state correctly
    model.addValueChangeListener( new IValueChangeListener()
    {
      @Override
      public void handleValueChange( final ValueChangeEvent event )
      {
        handleImportModeChanged();
      }
    } );
  }

  protected void handleImportModeChanged( )
  {
    m_viewer.update( m_documentData.getDocuments(), null );
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
    final DocumentsSearchDocumentsOperation operation = new DocumentsSearchDocumentsOperation( m_data, m_documentData );

    final IStatus status = RunnableContextHelper.execute( getContainer(), true, false, operation );
    if( !status.isOK() )
    {
      final String windowTitle = getWizard().getWindowTitle();
      new StatusDialog( getShell(), status, windowTitle ).open();
    }
  }
}