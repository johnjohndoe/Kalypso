/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ui.rrm.logviewer;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.ui.part.ViewPart;

/**
 * This class wraps the TableViewer and creates the column to define the table.
 * 
 * @author Madan
 */
public class LogTableViewer extends ViewPart
{
  private TableViewer m_viewer;

  private final LogViewer m_parentViewer;

  private final TableContentProvider m_tableContentProvider;

  private final TableViewLabelProvider m_tableViewLabelProvider;

  public LogTableViewer( final LogViewer parentViewer )
  {
    m_parentViewer = parentViewer;
    m_tableContentProvider = new TableContentProvider();
    m_tableViewLabelProvider = new TableViewLabelProvider();
  }

  public TableViewer getViewer( )
  {
    return m_viewer;
  }

  @Override
  public void setFocus( )
  {
    m_viewer.getControl().setFocus();
  }

  @Override
  public void createPartControl( final Composite parent )
  {
    m_viewer = new TableViewer( parent, SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL );
    m_viewer.setContentProvider( m_tableContentProvider );
    m_viewer.setLabelProvider( m_tableViewLabelProvider );

    m_viewer.setInput( m_parentViewer.getViewSite() );

    // Setup the CF5 Format table
    final Table table = m_viewer.getTable();
    table.setHeaderVisible( true );
    table.setLinesVisible( true );
    TableColumn column = new TableColumn( table, SWT.LEFT, 0 );
    column.setText( "Level" ); //$NON-NLS-1$
    column.setWidth( 70 );

    column = new TableColumn( table, SWT.LEFT, 1 );
    column.setText( "Element" ); //$NON-NLS-1$
    column.setWidth( 100 );

    column = new TableColumn( table, SWT.LEFT, 2 );
    column.setText( "Message" ); //$NON-NLS-1$
    column.setWidth( 500 );
    
    column = new TableColumn( table, SWT.LEFT, 3 );
    column.setText( "Parameter" ); //$NON-NLS-1$
    column.setWidth( 200 );
  }

  public void fileSelectionChange( final IFile file )
  {
    final TableContentProvider cp = new TableContentProvider( m_viewer, file );
    m_viewer.getTable().clearAll();
    m_viewer.setContentProvider( cp );
    m_viewer.getTable().setTopIndex( cp.getRowCount() );
  }

  @Override
  public void dispose( )
  {
    super.dispose();
  }
}