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
import org.eclipse.jface.viewers.ColumnViewer;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.ViewerColumn;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Table;
import org.hibernate.Session;
import org.kalypso.contribs.eclipse.jface.viewers.ColumnViewerUtil;
import org.kalypso.contribs.eclipse.jface.viewers.ViewerColumnItem;
import org.kalypso.contribs.eclipse.jface.viewers.table.ColumnsResizeControlListener;
import org.kalypso.contribs.eclipse.swt.widgets.ColumnViewerSorter;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.connect.command.GetPdbList;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.ui.internal.content.PdbLabelProvider;
import org.kalypso.model.wspm.pdb.ui.internal.content.PdbMeasurementDateComparator;
import org.kalypso.model.wspm.pdb.ui.internal.content.PdbMeasurementLabelProvider;

/**
 * @author Gernot Belger
 */
public class StatesViewer
{
  private TableViewer m_viewer;

  private WritableList m_tableInput;

  // FIXME: we should not leave the session all the time open... better to have the connection and reopen session
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

    final ViewerColumn nameColumn = createNameColumn( m_viewer, false );
    createMeasurementDateColumn( m_viewer, false );

    ColumnViewerSorter.setSortState( nameColumn, Boolean.FALSE );

    m_tableInput = new WritableList( new ArrayList<State>(), State.class );

    refreshState( null );

    final IValueProperty nameProperty = BeanProperties.value( State.class, State.PROPERTY_NAME );
    final IValueProperty measurementProperty = BeanProperties.value( State.class, State.PROPERTY_MEASUREMENTDATE_FORMATTED );

    final IValueProperty[] labelProperties = new IValueProperty[] { nameProperty, measurementProperty };
    ViewerSupport.bind( m_viewer, m_tableInput, labelProperties );

    return m_viewer;
  }

  public void refreshState( final String id )
  {
    m_tableInput.clear();
    final List<State> states = Arrays.asList( loadState() );
    m_tableInput.addAll( states );

    final State toSelect = findState( states, id );

    if( toSelect == null )
      m_viewer.setSelection( StructuredSelection.EMPTY );
    else
      m_viewer.setSelection( new StructuredSelection( toSelect ) );
  }

  private static State findState( final List<State> states, final String name )
  {
    if( name == null )
      return null;

    for( final State state : states )
    {
      if( state.getName().equals( name ) )
        return state;
    }

    return null;
  }

  public Control getControl( )
  {
    return m_viewer.getControl();
  }

  protected State[] loadState( )
  {
    try
    {
      final List<State> states = GetPdbList.getList( m_session, State.class );
      return states.toArray( new State[states.size()] );
    }
    catch( final PdbConnectException e )
    {
      e.printStackTrace();
      return new State[] {};
    }
  }

  public TableViewer getViewer( )
  {
    return m_viewer;
  }

  public State[] getExistingState( )
  {
    return (State[]) m_tableInput.toArray( new State[m_tableInput.size()] );
  }

  public static ViewerColumn createNameColumn( final ColumnViewer viewer, final boolean setLabelProvider )
  {
    final ViewerColumn nameColumn = ColumnViewerUtil.createViewerColumn( viewer, SWT.LEFT );
    final ViewerColumnItem column = new ViewerColumnItem( nameColumn );
    column.setText( "Name" );
    column.setResizable( false );
    column.setMoveable( false );
    ColumnsResizeControlListener.setMinimumPackWidth( column.getColumn() );

    if( setLabelProvider )
      nameColumn.setLabelProvider( new PdbLabelProvider() );

    ColumnViewerSorter.registerSorter( nameColumn, new ViewerComparator() );
    return nameColumn;
  }

  public static ViewerColumn createMeasurementDateColumn( final ColumnViewer viewer, final boolean setLabelProvider )
  {
    final ViewerColumn measurementDateColumn = ColumnViewerUtil.createViewerColumn( viewer, SWT.LEFT );
    final ViewerColumnItem column = new ViewerColumnItem( measurementDateColumn );
    column.setText( "Measurement" );
    column.setResizable( false );
    column.setMoveable( false );
    ColumnsResizeControlListener.setMinimumPackWidth( column.getColumn() );

    if( setLabelProvider )
      measurementDateColumn.setLabelProvider( new PdbMeasurementLabelProvider() );

    ColumnViewerSorter.registerSorter( measurementDateColumn, new PdbMeasurementDateComparator() );

    return null;
  }
}