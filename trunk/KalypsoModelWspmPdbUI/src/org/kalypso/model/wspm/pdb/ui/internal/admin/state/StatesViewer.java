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
package org.kalypso.model.wspm.pdb.ui.internal.admin.state;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.core.databinding.beans.BeanProperties;
import org.eclipse.core.databinding.observable.list.WritableList;
import org.eclipse.core.databinding.property.value.IValueProperty;
import org.eclipse.jface.databinding.viewers.ViewerSupport;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Table;
import org.hibernate.Session;
import org.kalypso.contribs.eclipse.jface.viewers.table.ColumnsResizeControlListener;
import org.kalypso.contribs.eclipse.swt.widgets.ColumnViewerSorter;
import org.kalypso.model.wspm.pdb.connect.Executor;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.connect.command.ListOperation;
import org.kalypso.model.wspm.pdb.db.mapping.States;

/**
 * @author Gernot Belger
 */
public class StatesViewer
{
  private TableViewer m_viewer;

  private WritableList m_tableInput;

  private final Session m_session;

  public StatesViewer( final Session session )
  {
    m_session = session;
  }

  public TableViewer createTableViewer( final Composite parent )
  {
    m_viewer = new TableViewer( parent, SWT.BORDER | SWT.FULL_SELECTION | SWT.H_SCROLL );
    final Table table = m_viewer.getTable();
    table.setHeaderVisible( true );

    table.addControlListener( new ColumnsResizeControlListener() );

    final TableViewerColumn nameColumn = new TableViewerColumn( m_viewer, SWT.LEFT );
    nameColumn.getColumn().setText( "Name" );
    nameColumn.getColumn().setResizable( false );
    nameColumn.getColumn().setData( ColumnsResizeControlListener.DATA_MIN_COL_WIDTH, ColumnsResizeControlListener.MIN_COL_WIDTH_PACK );
    ColumnViewerSorter.registerSorter( nameColumn, new ViewerSorter() );

    ColumnViewerSorter.setSortState( nameColumn, Boolean.FALSE );

    m_tableInput = new WritableList( new ArrayList<States>(), States.class );

    refreshStates( null );

    final IValueProperty nameProperty = BeanProperties.value( States.class, States.PROPERTY_STATE );

    final IValueProperty[] labelProperties = new IValueProperty[] { nameProperty };
    ViewerSupport.bind( m_viewer, m_tableInput, labelProperties );

    return m_viewer;
  }

  public void refreshStates( final String id )
  {
    m_tableInput.clear();
    final List<States> states = Arrays.asList( loadStates() );
    m_tableInput.addAll( states );

    final States toSelect = findState( states, id );

    if( toSelect == null )
      m_viewer.setSelection( StructuredSelection.EMPTY );
    else
      m_viewer.setSelection( new StructuredSelection( toSelect ) );
  }

  private static States findState( final List<States> states, final String name )
  {
    if( name == null )
      return null;

    for( final States state : states )
    {
      if( state.getState().equals( name ) )
        return state;
    }

    return null;
  }

  public Control getControl( )
  {
    return m_viewer.getControl();
  }

  protected States[] loadStates( )
  {
    try
    {
      final ListOperation<States> operation = new ListOperation<States>( States.class );
      new Executor( m_session, operation ).execute();
      final List<States> states = operation.getList();
      return states.toArray( new States[states.size()] );
    }
    catch( final PdbConnectException e )
    {
      e.printStackTrace();
      return new States[] {};
    }
  }

  public TableViewer getViewer( )
  {
    return m_viewer;
  }

  public States[] getExistingStates( )
  {
    return (States[]) m_tableInput.toArray( new States[m_tableInput.size()] );
  }
}