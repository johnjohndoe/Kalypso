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
package org.kalypso.observation.table;

import java.util.HashSet;
import java.util.Set;
import java.util.TreeSet;

import org.kalypso.commons.tuple.impl.AbstractTupleModel;
import org.kalypso.core.i18n.Messages;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

/**
 * It is a table over a list of columns build from TupleResults. Each key-component of the TupleResults are merged into
 * one big key-column in this model.
 * <p>
 * <b>Column creation is not supported:</b> this model is designed to allow a viewer/editor over a list of TupleResults
 * columns. Using this kind of model you cannot insert a new column in an existing TupleResult, you can just add an
 * existing column of a TupleResult into it using <code>addColumn</code>.
 * <p>
 * <b>Row creation is supported:</b> with this model you can add new rows to it. It automatically propagates the row as
 * records into the underlying TupleResults.
 * 
 * @author schlienger
 */
public class MultiTupleResultModel extends AbstractTupleModel<MTRMRow, MTRMColumn>
{
  /** the "merging" column */
  private KeyColumn m_keyColumn = null;

  /** all TupleResult based columns */
  private final Set<TupleResultColumn> m_columns = new HashSet<TupleResultColumn>();

  /**
   * Adds a column build upon a TupleResult to this model. First the column will be checked if it is compatible with the
   * already present ones: the key component of the column has to be compliant with the existing ones.
   * 
   * @throws IllegalArgumentException
   *             if the column is not compatible
   */
  public void addColumn( final TupleResultColumn col )
  {
    if( m_keyColumn == null )
    {
      m_keyColumn = new KeyColumn( 0, col.getKeyComponent() );

      fireColumnAdded( m_keyColumn );
    }
    else if( !m_keyColumn.isCompatible( col.getKeyComponent() ) )
      throw new IllegalArgumentException( Messages.getString("org.kalypso.observation.table.MultiTupleResultModel.0") ); //$NON-NLS-1$

    if( containsColumn( col ) )
      return;

    final TupleResult result = col.getTupleResult();
    for( final IRecord record : result )
    {
      final Object keyValue = record.getValue( col.getKeyComponent() );

      final MTRMRow rowKey = m_keyColumn.resolve( keyValue );

      col.setMapping( rowKey, record );
    }

    m_columns.add( col );

    fireColumnAdded( col );
  }

  private boolean containsColumn( final TupleResultColumn col )
  {
    final TupleResultColumnComparator comp = TupleResultColumnComparator.getInstance();
    for( final TupleResultColumn trc : m_columns )
    {
      if( comp.compare( trc, col ) == 0 )
        return true;
    }

    return false;
  }

  /**
   * Clears the contents of this model
   */
  public void clear( )
  {
    for( final TupleResultColumn col : m_columns )
      col.clear();

    m_columns.clear();

    if( m_keyColumn != null )
    {
      m_keyColumn.clear();
      m_keyColumn = null;
    }
  }

  /**
   * @return the key component if present, else null
   */
  public IComponent getKeyComponent( )
  {
    if( m_keyColumn == null )
      return null;

    return m_keyColumn.getKeyComponent();
  }

  /**
   * The row count is the size of the key column, i.e. the set of the key values of the key-components of all
   * TupleResults.
   * 
   * @see org.kalypso.commons.tuple.ITupleModel#getRowCount()
   */
  public int getRowCount( )
  {
    if( m_keyColumn == null )
      return 0;

    return m_keyColumn.getCount();
  }

  /**
   * The column count is the count of all columns, including the generated one (merging column)
   * 
   * @see org.kalypso.commons.tuple.ITupleModel#getColumnCount()
   */
  public int getColumnCount( )
  {
    if( m_keyColumn == null )
      return 0;

    return m_columns.size() + 1;
  }

  /**
   * @see org.kalypso.commons.tuple.ITupleModel#getRowKeySet()
   */
  public Set<MTRMRow> getRowKeySet( )
  {
    if( m_keyColumn == null )
      return new HashSet<MTRMRow>();

    return m_keyColumn.valueSet();
  }

  /**
   * @see org.kalypso.commons.tuple.ITupleModel#getColumnKeySet()
   */
  public Set<MTRMColumn> getColumnKeySet( )
  {
    final Set<MTRMColumn> cols = new TreeSet<MTRMColumn>( MTRMColumnPositionComparator.getInstance() );

    if( m_keyColumn == null )
      return cols;

    cols.add( m_keyColumn );
    cols.addAll( m_columns );

    return cols;
  }

  /**
   * @see org.kalypso.commons.tuple.ITupleModel#hasRowKey(R)
   */
  public boolean hasRowKey( final MTRMRow rowKey )
  {
    if( m_keyColumn == null )
      return false;

    return m_keyColumn.hasRowKey( rowKey );
  }

  /**
   * @see org.kalypso.commons.tuple.ITupleModel#hasColumnKey(C)
   */
  public boolean hasColumnKey( final MTRMColumn columnKey )
  {
    if( m_keyColumn == columnKey )
      return true;

    return m_columns.contains( columnKey );
  }

  /**
   * @see org.kalypso.commons.tuple.ITupleModel#getValue(R, C)
   */
  public Object getValue( final MTRMRow rowKey, final MTRMColumn columnKey )
  {
    if( columnKey == m_keyColumn )
      return rowKey.getValue();

    if( !(columnKey instanceof TupleResultColumn) )
      throw new IllegalArgumentException( Messages.getString("org.kalypso.observation.table.MultiTupleResultModel.1") ); //$NON-NLS-1$

    final TupleResultColumn col = (TupleResultColumn) columnKey;

    return col.getRecordFor( rowKey ).getValue( col.getValueComponent() );
  }

  /**
   * Removing a column does not delete it from the underlying TupleResult. It just removes the column from this model.
   */
  @Override
  public void removeColumnIntern( final MTRMColumn columnKey )
  {
    // removing the key column also removes all the other columns
    if( columnKey == m_keyColumn )
    {
      m_columns.clear();
      m_keyColumn = null;
    }
    else
      m_columns.remove( columnKey );
  }

  /**
   * Row removal is propagated to all TupleResults: each corresponding record will be removed.
   */
  @Override
  public void removeRowIntern( final MTRMRow rowKey )
  {
    for( final TupleResultColumn col : m_columns )
    {
      col.getTupleResult().remove( col.getRecordFor( rowKey ) );
      col.removeKey( rowKey );
    }

    if( m_keyColumn != null )
      m_keyColumn.removeRowKey( rowKey );
  }

  /**
   * @see org.kalypso.commons.tuple.impl.AbstractTupleModel#setValueIntern(java.lang.Object, R, C)
   */
  @Override
  protected void setValueIntern( final Object value, final MTRMRow rowKey, final MTRMColumn columnKey )
  {
    if( !(columnKey instanceof TupleResultColumn) )
      throw new IllegalArgumentException( Messages.getString("org.kalypso.observation.table.MultiTupleResultModel.2") ); //$NON-NLS-1$

    final TupleResultColumn col = (TupleResultColumn) columnKey;
    final TupleResult tr = col.getTupleResult();
    IRecord record = col.getRecordFor( rowKey );
    if( record == null )
    {
      record = tr.createRecord();
      tr.add( record );

      col.setMapping( rowKey, record );
    }

    record.setValue( col.getValueComponent(), value );
  }
}
