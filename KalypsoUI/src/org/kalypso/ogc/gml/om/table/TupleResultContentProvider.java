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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.TableItem;
import org.kalypso.contribs.eclipse.jface.viewers.DefaultTableViewer;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

/**
 * @author schlienger
 */
public class TupleResultContentProvider implements IStructuredContentProvider
{
  private final Map<String, IComponent> m_componentMap = new HashMap<String, IComponent>();

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
    if( oldInput != null )
      tableViewer.removeAllColumns();

    m_componentMap.clear();

    if( !(newInput instanceof TupleResult) )
      return;

    final TupleResult result = (TupleResult) newInput;

    final IComponent[] components = result.getComponents();
    final List<CellEditor> cellEditors = new ArrayList<CellEditor>( components.length );
    for( final IComponent component : components )
    {
      final String id = component.getName();
      tableViewer.addColumn( id, component.getName(), 100, true );
      m_componentMap.put( id, component );
      cellEditors.add( new TextCellEditor( tableViewer.getTable(), SWT.NONE ) );
    }
    // TODO: make member
    final ICellModifier modifier = new ICellModifier()
    {
      public boolean canModify( final Object element, String property )
      {
        return true;
      }

      public Object getValue( final Object element, final String property )
      {
        final IRecord record = (IRecord) element;
        final IComponent component = getComponent( property );

        final Object value = record.getValue( component );

        return "" + value;
      }

      public void modify( final Object element, final String property, final Object value )
      {
        final TableItem item = (TableItem) element;
        final IRecord record = (IRecord) item.getData();
        final IComponent component = getComponent( property );

        final Double valueToSet = new Double( value.toString() );

        record.setValue( component, valueToSet );

        tableViewer.update( record, new String[] { property } );
        // TODO: propagate changes to gml-model
      }
    };

    tableViewer.setCellModifier( modifier );
    tableViewer.refreshColumnProperties();
    tableViewer.setCellEditors( cellEditors.toArray( new CellEditor[cellEditors.size()] ) );
  }

  /**
   * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
   */
  public Object[] getElements( final Object inputElement )
  {
    if( inputElement != null && inputElement instanceof TupleResult )
    {
      final TupleResult result = (TupleResult) inputElement;

      return result.toArray();
    }

    return null;
  }

  protected IComponent getComponent( final String property )
  {
    return m_componentMap.get( property );
  }
}
