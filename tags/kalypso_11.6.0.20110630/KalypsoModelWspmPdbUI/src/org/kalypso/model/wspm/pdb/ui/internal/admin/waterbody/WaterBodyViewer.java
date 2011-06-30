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
package org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody;

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
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Table;
import org.hibernate.Session;
import org.kalypso.contribs.eclipse.jface.viewers.table.ColumnsResizeControlListener;
import org.kalypso.contribs.eclipse.swt.widgets.ColumnViewerSorter;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.connect.command.GetPdbList;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.ui.internal.admin.state.PdbNameComparator;

/**
 * @author Gernot Belger
 */
public class WaterBodyViewer
{
  private TableViewer m_viewer;

  private WritableList m_tableInput;

  private final Session m_session;

  public WaterBodyViewer( final Session session )
  {
    m_session = session;
  }

  public TableViewer createTableViewer( final Composite parent )
  {
    m_viewer = new TableViewer( parent, SWT.BORDER | SWT.FULL_SELECTION );

    configureViewer( m_viewer, false );

    m_tableInput = new WritableList( new ArrayList<WaterBody>(), WaterBody.class );

    refreshWaterBody( null );

    final IValueProperty labelProperty = BeanProperties.value( WaterBody.class, WaterBody.PROPERTY_LABEL );
    final IValueProperty gknProperty = BeanProperties.value( WaterBody.class, WaterBody.PROPERTY_NAME );

    final IValueProperty[] labelProperties = new IValueProperty[] { gknProperty, labelProperty };
    ViewerSupport.bind( m_viewer, m_tableInput, labelProperties );

    return m_viewer;
  }

  public static void configureViewer( final TableViewer viewer, final boolean setLabelProvider )
  {
    viewer.setUseHashlookup( true );

    final Table table = viewer.getTable();
    table.setHeaderVisible( true );

    table.addControlListener( new ColumnsResizeControlListener() );

    final TableViewerColumn gknColumn = new TableViewerColumn( viewer, SWT.LEFT );
    gknColumn.getColumn().setText( WaterBodyStrings.STR_GEWÄSSERKENNZIFFER );
    gknColumn.getColumn().setResizable( false );
    ColumnsResizeControlListener.setMinimumPackWidth( gknColumn.getColumn() );
    if( setLabelProvider )
      gknColumn.setLabelProvider( new WaterBodyCodeLabelProvider() );
    ColumnViewerSorter.registerSorter( gknColumn, new PdbGknComparator() );

    final TableViewerColumn labelColumn = new TableViewerColumn( viewer, SWT.LEFT );
    labelColumn.getColumn().setText( WaterBodyStrings.STR_NAME );
    labelColumn.getColumn().setResizable( false );
    ColumnsResizeControlListener.setMinimumPackWidth( labelColumn.getColumn() );
    if( setLabelProvider )
      labelColumn.setLabelProvider( new WaterBodyLabelLabelProvider() );
    ColumnViewerSorter.registerSorter( labelColumn, new PdbNameComparator() );
  }

  public void refreshWaterBody( final String name )
  {
    m_tableInput.clear();
    final List<WaterBody> waterBodies = Arrays.asList( loadWaterbodies() );
    m_tableInput.addAll( waterBodies );

    final WaterBody toSelect = findWaterBody( waterBodies, name );

    if( toSelect == null )
      m_viewer.setSelection( StructuredSelection.EMPTY );
    else
      m_viewer.setSelection( new StructuredSelection( toSelect ) );
  }

  private static WaterBody findWaterBody( final List<WaterBody> waterBodies, final String name )
  {
    if( name == null )
      return null;

    for( final WaterBody waterBody : waterBodies )
    {
      if( waterBody.getName().equals( name ) )
        return waterBody;
    }

    return null;
  }

  public Control getControl( )
  {
    return m_viewer.getControl();
  }

  protected WaterBody[] loadWaterbodies( )
  {
    try
    {
      final List<WaterBody> waterBodies = GetPdbList.getList( m_session, WaterBody.class );
      return waterBodies.toArray( new WaterBody[waterBodies.size()] );
    }
    catch( final PdbConnectException e )
    {
      e.printStackTrace();
      return new WaterBody[] {};
    }
  }

  public TableViewer getViewer( )
  {
    return m_viewer;
  }

  public WaterBody[] getExistingWaterbodies( )
  {
    return (WaterBody[]) m_tableInput.toArray( new WaterBody[m_tableInput.size()] );
  }
}