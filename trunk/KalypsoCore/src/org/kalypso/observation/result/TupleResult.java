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
package org.kalypso.observation.result;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Set;

/**
 * @author schlienger
 */
public class TupleResult implements List<IRecord>
{
  private final List<IRecord> m_records = new ArrayList<IRecord>();

  private Set<IComponent> m_components = new HashSet<IComponent>();

  public TupleResult()
  {
    // default constructor
  }
  
  public TupleResult( final IComponent[] comps )
  {
    for( int i = 0; i < comps.length; i++ )
      addComponent( comps[i] );
  }
  
  /**
   * @see java.util.List#add(int, E)
   */
  public void add( int index, IRecord element )
  {
    checkRecord( element );

    m_records.add( index, element );
  }

  /**
   * @see java.util.List#add(E)
   */
  public boolean add( IRecord o )
  {
    checkRecord( o );

    return m_records.add( o );
  }

  /**
   * @see java.util.List#addAll(java.util.Collection)
   */
  public boolean addAll( Collection< ? extends IRecord> c )
  {
    checkRecords( c );

    return m_records.addAll( c );
  }

  /**
   * @see java.util.List#addAll(int, java.util.Collection)
   */
  public boolean addAll( int index, Collection< ? extends IRecord> c )
  {
    checkRecords( c );

    return m_records.addAll( index, c );
  }

  /**
   * @see java.util.List#clear()
   */
  public void clear( )
  {
    m_records.clear();
  }

  /**
   * @see java.util.List#contains(java.lang.Object)
   */
  public boolean contains( Object o )
  {
    return m_records.contains( o );
  }

  /**
   * @see java.util.List#containsAll(java.util.Collection)
   */
  public boolean containsAll( Collection< ? > c )
  {
    return m_records.containsAll( c );
  }

  /**
   * @see java.util.List#get(int)
   */
  public IRecord get( int index )
  {
    return m_records.get( index );
  }

  /**
   * @see java.util.List#indexOf(java.lang.Object)
   */
  public int indexOf( Object o )
  {
    return m_records.indexOf( o );
  }

  /**
   * @see java.util.List#isEmpty()
   */
  public boolean isEmpty( )
  {
    return m_records.isEmpty();
  }

  /**
   * @see java.util.List#iterator()
   */
  public Iterator<IRecord> iterator( )
  {
    return m_records.iterator();
  }

  /**
   * @see java.util.List#lastIndexOf(java.lang.Object)
   */
  public int lastIndexOf( Object o )
  {
    return m_records.lastIndexOf( o );
  }

  /**
   * @see java.util.List#listIterator()
   */
  public ListIterator<IRecord> listIterator( )
  {
    return m_records.listIterator();
  }

  /**
   * @see java.util.List#listIterator(int)
   */
  public ListIterator<IRecord> listIterator( int index )
  {
    return m_records.listIterator( index );
  }

  /**
   * @see java.util.List#remove(int)
   */
  public IRecord remove( int index )
  {
    return m_records.remove( index );
  }

  /**
   * @see java.util.List#remove(java.lang.Object)
   */
  public boolean remove( Object o )
  {
    return m_records.remove( o );
  }

  /**
   * @see java.util.List#removeAll(java.util.Collection)
   */
  public boolean removeAll( Collection< ? > c )
  {
    return m_records.removeAll( c );
  }

  /**
   * @see java.util.List#retainAll(java.util.Collection)
   */
  public boolean retainAll( Collection< ? > c )
  {
    return m_records.retainAll( c );
  }

  /**
   * @see java.util.List#set(int, E)
   */
  public IRecord set( int index, IRecord element )
  {
    return m_records.set( index, element );
  }

  /**
   * @see java.util.List#size()
   */
  public int size( )
  {
    return m_records.size();
  }

  /**
   * @see java.util.List#subList(int, int)
   */
  public List<IRecord> subList( int fromIndex, int toIndex )
  {
    return m_records.subList( fromIndex, toIndex );
  }

  /**
   * @see java.util.List#toArray()
   */
  public Object[] toArray( )
  {
    return m_records.toArray();
  }

  /**
   * @see java.util.List#toArray(T[])
   */
  public <T> T[] toArray( T[] a )
  {
    return m_records.toArray( a );
  }

  private void checkRecord( final IRecord record )
  {
    final Record r = (Record) record;
    if( r.getTableResult() != this )
      throw new IllegalArgumentException( "Illegal record." );

    r.checkComponents( m_components );
  }

  private void checkRecords( final Collection< ? extends IRecord> c )
  {
    for( final IRecord record : c )
      checkRecord( record );
  }

  public IComponent[] getComponents( )
  {
    return m_components.toArray( new IComponent[m_components.size()] );
  }

  public final void addComponent( final IComponent comp )
  {
    m_components.add( comp );
  }

  public boolean removeComponent( final IComponent comp )
  {
    final boolean b = m_components.remove( comp );
    if( b )
    {
      for( final IRecord record : m_records )
      {
        final Record r = (Record) record;
        r.remove( comp );
      }
    }

    return b;
  }

  public Object getValue( final IRecord record, final IComponent comp )
  {
    return record.getValue( comp );
  }

  public void setValue( final IRecord record, final IComponent comp, final Object value )
  {
    record.setValue( comp, value );
  }

  public IRecord createRecord( )
  {
    return new Record( this, m_components );
  }

  public boolean hasComponent( final IComponent comp )
  {
    return m_components.contains( comp );
  }
}
