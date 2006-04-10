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
package org.kalypso.tuple;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * @author schlienger
 */
public final class TupleModelUtils
{
  private TupleModelUtils()
  {
    // not intended to be instanciated
  }
  
  public static <R extends IKey, C extends IColumnKey> Map<C , Object> getRow( final ITupleModel<R, C> model, final R rowKey )
  {
    final Set<C> columnKeySet = model.getColumnKeySet();

    final Map<C, Object> row = new HashMap<C, Object>( columnKeySet.size() );
    for( final C colKey : columnKeySet )
      row.put( colKey, model.getValue( rowKey, colKey ) );

    return row;
  }

  public <R extends IKey, C extends IColumnKey> Map<R, Object> getColumn( final ITupleModel<R, C> model, final C columnKey )
  {
    final Set<R> rowKeySet = model.getRowKeySet();

    final Map<R, Object> column = new HashMap<R, Object>( rowKeySet.size() );
    for( final R rowKey : rowKeySet )
      column.put( rowKey, model.getValue( rowKey, columnKey ) );

    return column;
  }

  public <R extends IKey, C extends IColumnKey> void addColumn( final ITupleModel<R, C> model, final C columnKey, final Map<R, Object> column )
  {
    for( final Map.Entry<R, Object> entry : column.entrySet() )
      model.setValue( entry.getValue(), entry.getKey(), columnKey );
  }
  
  public <R extends IKey, C extends IColumnKey> void addColumn( final ITupleModel<R, C> model, final C columnKey, final R[] rowKeys, final Object[] column )
  {
    if( rowKeys.length != column.length )
      throw new IllegalArgumentException( "Array Length Not Equal: " + rowKeys.length + " != " + column.length );
    
    for( int i = 0; i < rowKeys.length; i++ )
      model.setValue( column[i], rowKeys[i], columnKey );
  }

  public <R extends IKey, C extends IColumnKey> void addModel( final ITupleModel<R, C> model, final ITupleModel<R, C> destModel )
  {
    final Set<R> rowKeySet = model.getRowKeySet();
    final Set<C> columnKeySet = model.getColumnKeySet();
    
    for( R rowKey : rowKeySet )
      for( C colKey : columnKeySet )
        destModel.setValue( model.getValue( rowKey, colKey ), rowKey, colKey );
  }
  
  public <R extends IKey, C extends IColumnKey> String dump( final ITupleModel<R, C> model )
  {
    final Set<R> rowKeySet = model.getRowKeySet();
    final Set<C> columnKeySet = model.getColumnKeySet();

    final StringBuffer sw = new StringBuffer();
    
    for( R rowKey : rowKeySet )
    {
      for( C colKey : columnKeySet )
      {
        model.getValue( rowKey, colKey ).toString();
        
        sw.append( '\t' );
      }
      
      sw.append( '\n' );
    }
    
    return sw.toString();
  }
}
