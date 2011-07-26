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

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.contribs.eclipse.jface.viewers.table.ColumnsResizeControlListener;
import org.kalypso.contribs.eclipse.jface.wizard.IUpdateable;
import org.kalypso.contribs.eclipse.swt.widgets.ColumnSortListener;
import org.kalypso.core.status.StatusDialog2;

/**
 * @author Gernot Belger
 */
public class ImportAttachmentsPreviewPage extends WizardPage implements IUpdateable
{
  private final ImportAttachmentsDocumentsData m_documentData = new ImportAttachmentsDocumentsData();

  private final ImportAttachmentsData m_data;


  private CheckboxTableViewer m_viewer;

  public ImportAttachmentsPreviewPage( final String pageName, final ImportAttachmentsData data )
  {
    super( pageName );

    m_data = data;

    setTitle( "Preview" );
    setDescription( "Preview and select the found documents on this page." );
  }

  @Override
  public void createControl( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    GridLayoutFactory.swtDefaults().applyTo( panel );
    setControl( panel );

    createDocumentsTable( panel );
  }

  private void createDocumentsTable( final Composite parent )
  {
    final Table table = new Table( parent, SWT.CHECK | SWT.FULL_SELECTION | SWT.BORDER );
    table.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    m_viewer = new CheckboxTableViewer( table );
    m_viewer.setContentProvider( new ArrayContentProvider() );

    // status
    final TableViewerColumn statusColumn = new TableViewerColumn( m_viewer, SWT.LEFT );
    statusColumn.setLabelProvider( new DocumentsStatusProvider( m_documentData ) );
    statusColumn.getColumn().setText( "Station" );
    statusColumn.getColumn().setResizable( false );
    ColumnsResizeControlListener.setMinimumPackWidth( statusColumn.getColumn() );

    // station
    final TableViewerColumn stationColumn = new TableViewerColumn( m_viewer, SWT.LEFT );
    stationColumn.setLabelProvider( new DocumentsStationProvider( m_documentData ) );
    stationColumn.getColumn().setText( "Station" );
    stationColumn.getColumn().setResizable( false );
    ColumnsResizeControlListener.setMinimumPackWidth( stationColumn.getColumn() );

    /* name */
    final TableViewerColumn nameColumn = new TableViewerColumn( m_viewer, SWT.LEFT );
    nameColumn.setLabelProvider( new DocumentsNameProvider() );
    nameColumn.getColumn().setText( "Filename" );
    nameColumn.getColumn().setResizable( false );
    ColumnsResizeControlListener.setMinimumPackWidth( nameColumn.getColumn() );

    /* mime/type */
    final TableViewerColumn typeColumn = new TableViewerColumn( m_viewer, SWT.LEFT );
    typeColumn.setLabelProvider( new DocumentsTypeProvider() );
    typeColumn.getColumn().setText( "Type" );
    typeColumn.getColumn().setResizable( false );
    ColumnsResizeControlListener.setMinimumPackWidth( typeColumn.getColumn() );

    table.addControlListener( new ColumnsResizeControlListener() );

    ColumnSortListener.setSortState( stationColumn, Boolean.TRUE );
  }

  @Override
  public void update( )
  {
    readDocuments();

    m_viewer.setInput( m_documentData.getDocuments() );
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