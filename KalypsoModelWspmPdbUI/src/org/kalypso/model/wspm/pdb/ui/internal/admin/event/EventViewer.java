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
package org.kalypso.model.wspm.pdb.ui.internal.admin.event;

import java.util.List;

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ColumnViewer;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.ViewerColumn;
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
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.ui.internal.admin.state.PdbNameComparator;
import org.kalypso.model.wspm.pdb.ui.internal.admin.state.StatesViewer;
import org.kalypso.model.wspm.pdb.ui.internal.content.PdbLabelProvider;

/**
 * @author Gernot Belger
 */
public class EventViewer
{
  private TableViewer m_viewer;

  private final Session m_session;

  public EventViewer( final Session session )
  {
    m_session = session;
  }

  public TableViewer createTableViewer( final Composite parent )
  {
    m_viewer = new TableViewer( parent, SWT.BORDER | SWT.FULL_SELECTION );
    m_viewer.setContentProvider( new ArrayContentProvider() );

    configureViewer( m_viewer );

    refreshEvents( null );

    return m_viewer;
  }

  public static void configureViewer( final TableViewer viewer )
  {
    viewer.setUseHashlookup( true );

    final Table table = viewer.getTable();
    table.setHeaderVisible( true );

    table.addControlListener( new ColumnsResizeControlListener() );

    createNameColumn( viewer );
    createMeasurementDateColumn( viewer );
  }

  public static ViewerColumn createNameColumn( final ColumnViewer viewer )
  {
    final ViewerColumn nameColumn = ColumnViewerUtil.createViewerColumn( viewer, SWT.LEFT );
    final ViewerColumnItem column = new ViewerColumnItem( nameColumn );

    column.setText( "Name" );
    column.setResizable( false );
    ColumnsResizeControlListener.setMinimumPackWidth( column.getColumn() );
    nameColumn.setLabelProvider( new PdbLabelProvider() );
    ColumnViewerSorter.registerSorter( nameColumn, new PdbNameComparator() );
    return nameColumn;
  }

  public static ViewerColumn createMeasurementDateColumn( final ColumnViewer viewer )
  {
    return StatesViewer.createMeasurementDateColumn( viewer );
  }

  public void refreshEvents( final String name )
  {
    final Event[] events = loadEvents();
    m_viewer.setInput( events );

    final Event toSelect = findEvent( events, name );

    if( toSelect == null )
      m_viewer.setSelection( StructuredSelection.EMPTY );
    else
      m_viewer.setSelection( new StructuredSelection( toSelect ) );
  }

  private static Event findEvent( final Event[] events, final String name )
  {
    if( name == null )
      return null;

    for( final Event event : events )
    {
      if( event.getName().equals( name ) )
        return event;
    }

    return null;
  }

  public Control getControl( )
  {
    return m_viewer.getControl();
  }

  protected Event[] loadEvents( )
  {
    try
    {
      final List<Event> events = GetPdbList.getList( m_session, Event.class );
      return events.toArray( new Event[events.size()] );
    }
    catch( final PdbConnectException e )
    {
      e.printStackTrace();
      return new Event[] {};
    }
  }

  public TableViewer getViewer( )
  {
    return m_viewer;
  }

  public Event[] getExistingEvents( )
  {
    return (Event[]) m_viewer.getInput();
  }
}