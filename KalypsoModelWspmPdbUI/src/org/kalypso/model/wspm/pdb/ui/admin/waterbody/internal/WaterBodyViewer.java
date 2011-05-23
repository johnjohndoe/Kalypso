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
package org.kalypso.model.wspm.pdb.ui.admin.waterbody.internal;

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
import org.kalypso.contribs.eclipse.jface.viewers.table.ColumnsResizeControlListener;
import org.kalypso.contribs.eclipse.swt.widgets.ColumnViewerSorter;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBodies;

/**
 * @author Gernot Belger
 */
public class WaterBodyViewer
{
  private TableViewer m_viewer;

  private WritableList m_tableInput;

  private final IPdbConnection m_connection;

  public WaterBodyViewer( final IPdbConnection connection )
  {
    m_connection = connection;
  }

  public TableViewer createTableViewer( final Composite parent )
  {
    m_viewer = new TableViewer( parent, SWT.BORDER | SWT.FULL_SELECTION | SWT.H_SCROLL );
    final Table table = m_viewer.getTable();
    table.setHeaderVisible( true );

    // TODO Should be added to the parent of the table.
    // TODO TableViewerUtilities has a function for adding this type of listener.
    table.addControlListener( new ColumnsResizeControlListener() );

    final TableViewerColumn gknColumn = new TableViewerColumn( m_viewer, SWT.LEFT );
    gknColumn.getColumn().setText( WaterBodyStrings.STR_GEWÄSSERKENNZIFFER );
    gknColumn.getColumn().setResizable( false );
    gknColumn.getColumn().setData( ColumnsResizeControlListener.DATA_MIN_COL_WIDTH, ColumnsResizeControlListener.MIN_COL_WIDTH_PACK );

    ColumnViewerSorter.registerSorter( gknColumn, new ViewerSorter() );

    final TableViewerColumn nameColumn = new TableViewerColumn( m_viewer, SWT.LEFT );
    nameColumn.getColumn().setText( WaterBodyStrings.STR_NAME );
    nameColumn.getColumn().setResizable( false );
    nameColumn.getColumn().setData( ColumnsResizeControlListener.DATA_MIN_COL_WIDTH, ColumnsResizeControlListener.MIN_COL_WIDTH_PACK );
    ColumnViewerSorter.registerSorter( nameColumn, new ViewerSorter() );

    m_tableInput = new WritableList( new ArrayList<WaterBodies>(), WaterBodies.class );

    refreshWaterBodies( null );

    final IValueProperty nameProperty = BeanProperties.value( WaterBodies.class, WaterBodies.PROPERTY_NAME );
    final IValueProperty gknProperty = BeanProperties.value( WaterBodies.class, WaterBodies.PROPERTY_WATERBODY );

    final IValueProperty[] labelProperties = new IValueProperty[] { gknProperty, nameProperty };
    ViewerSupport.bind( m_viewer, m_tableInput, labelProperties );

    return m_viewer;
  }

  public void refreshWaterBodies( final WaterBodies select )
  {
    m_tableInput.clear();
    m_tableInput.addAll( Arrays.asList( loadWaterbodies() ) );

    if( select != null )
      m_viewer.setSelection( new StructuredSelection( select ) );
  }

  public Control getControl( )
  {
    return m_viewer.getControl();
  }

  protected WaterBodies[] loadWaterbodies( )
  {
    try
    {
      final List<WaterBodies> waterBodies = m_connection.getWaterBodies();
      return waterBodies.toArray( new WaterBodies[waterBodies.size()] );
    }
    catch( final PdbConnectException e )
    {
      e.printStackTrace();
      return new WaterBodies[] {};
    }
  }

  public TableViewer getViewer( )
  {
    return m_viewer;
  }

  public WaterBodies[] getExistingWaterbodies( )
  {
    return (WaterBodies[]) m_tableInput.toArray( new WaterBodies[m_tableInput.size()] );
  }
}