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
package org.kalypso.ogc.gml.om.table;

import java.util.HashSet;
import java.util.Set;

import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.kalypso.contribs.eclipse.jface.viewers.DefaultTableViewer;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.ITupleResultChangedListener;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.table.handlers.ComponentUiFirstColumnHandler;
import org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandler;

/**
 * @author Marc Schlienger
 */
public class TupleResultContentProvider implements IStructuredContentProvider, ITupleResultChangedListener
{
  private DefaultTableViewer m_tableViewer;

  private TupleResult m_result;

  private final IComponentUiHandler[] m_componentHandlers;

  public TupleResultContentProvider( final IComponentUiHandler[] componentHandlers )
  {
    m_componentHandlers = new IComponentUiHandler[componentHandlers.length + 1];

    m_componentHandlers[0] = new ComponentUiFirstColumnHandler();
    System.arraycopy( componentHandlers, 0, m_componentHandlers, 1, componentHandlers.length );
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#dispose()
   */
  public void dispose( )
  {
    // empty
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object,
   *      java.lang.Object)
   */
  public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput )
  {
    final DefaultTableViewer tableViewer = (DefaultTableViewer) viewer;
    m_tableViewer = tableViewer;

    if( oldInput instanceof TupleResult )
      ((TupleResult) oldInput).removeChangeListener( this );

    m_result = (TupleResult) newInput;
    if( m_result != null )
    {
      // Only remove columns if input non null, because input==null may happen while disposing
      refreshColumns();
      m_result.addChangeListener( this );
    }
  }

  private void refreshColumns( )
  {
    m_tableViewer.removeAllColumns();

    final CellEditor[] cellEditors = new CellEditor[m_componentHandlers.length];
    for( int i = 0; i < m_componentHandlers.length; i++ )
    {
      final IComponentUiHandler handler = m_componentHandlers[i];

      final IComponent component = handler.getComponent();

      final int columnWidth = handler.getColumnWidth();
      final int columnWidthPercent = handler.getColumnWidthPercent();
      final int columnStyle = handler.getColumnStyle();
      final boolean editable = handler.isEditable();
      final boolean resizeable = handler.isResizeable();

      final String label = handler.getColumnLabel();
      final String columnLabel = label == null ? component.getName() : label;
      final String tooltip = component == null ? null : component.getDescription();

      m_tableViewer.addColumn( "" + i, columnLabel, tooltip, columnWidth, columnWidthPercent, editable, columnStyle, resizeable );

      cellEditors[i] = handler.createCellEditor( m_tableViewer.getTable() );
    }

    m_tableViewer.refreshColumnProperties();
    m_tableViewer.setCellEditors( cellEditors );
  }

  /**
   * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
   */
  public Object[] getElements( final Object inputElement )
  {
    if( (inputElement != null) && (inputElement instanceof TupleResult) )
    {
      final TupleResult result = (TupleResult) inputElement;

      return result.toArray();
    }

    return null;
  }

  public TupleResult getResult( )
  {
    return m_result;
  }

  /**
   * @see org.kalypso.observation.result.ITupleResultChangedListener#valuesChanged(org.kalypso.observation.result.ITupleResultChangedListener.ValueChange[])
   */
  public void valuesChanged( final ValueChange[] changes )
  {
    final IRecord[] records = new IRecord[changes.length];
    final IComponent[] components = m_result.getComponents();

    final Set<String> properties = new HashSet<String>();
    final Set<IRecord> emptyRecords = new HashSet<IRecord>();
    for( int i = 0; i < changes.length; i++ )
    {
      final ValueChange change = changes[i];

      final IRecord record = change.getRecord();

      records[i] = record;

      final String id = change.getComponent().getName();
      properties.add( id );

      /* set changed value */
      record.setValue( change.getComponent(), change.getNewValue() );

      /* check if we have an empty record */
      boolean isEmpty = true;
      for( final IComponent component : components )
      {
        final Object value = record.getValue( component );
        if( value != null )
        {
          isEmpty = false;
          break;
        }
      }

      if( isEmpty )
        emptyRecords.add( record );
    }

    if( emptyRecords.size() > 0 )
      m_result.removeAll( emptyRecords );
    else
    {
      final String[] props = properties.toArray( new String[properties.size()] );
      m_tableViewer.update( records, props );
    }
  }

  /**
   * @see org.kalypso.observation.result.ITupleResultChangedListener#recordsChanged(org.kalypso.observation.result.IRecord[],
   *      org.kalypso.observation.result.ITupleResultChangedListener.TYPE)
   */
  public void recordsChanged( final IRecord[] records, final TYPE type )
  {
    m_tableViewer.refresh();
  }

  /**
   * @see org.kalypso.observation.result.ITupleResultChangedListener#componentsChanged(org.kalypso.observation.result.IComponent[],
   *      org.kalypso.observation.result.ITupleResultChangedListener.TYPE)
   */
  public void componentsChanged( final IComponent[] components, final TYPE type )
  {
// refreshColumns( );
// m_tableViewer.refresh();
  }

  public IComponentUiHandler[] getHandlers( )
  {
    return m_componentHandlers;
  }

  public IComponentUiHandler getHandler( final String id )
  {
    final int index = Integer.parseInt( id );
    return m_componentHandlers[index];
  }

}
