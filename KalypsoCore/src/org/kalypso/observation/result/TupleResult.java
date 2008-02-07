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
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.observation.result.ITupleResultChangedListener.TYPE;
import org.kalypso.observation.result.ITupleResultChangedListener.ValueChange;

/**
 * @author Marc Schlienger
 */
public class TupleResult implements List<IRecord>
{
  private final List<IRecord> m_records = new ArrayList<IRecord>();

  private final List<IComponent> m_components = new ArrayList<IComponent>();

  /** This tuple result gets sorted by these components. */
  // TODO: this is wrong: instead we need a list of the indices. BUT: the schema does not support that...
  private final Set<IComponent> m_sortComponents = new LinkedHashSet<IComponent>();

  private final Set<ITupleResultChangedListener> m_listeners = new HashSet<ITupleResultChangedListener>();

  /** Internal sort state. Initially <code>true</code> as the empty list is sorted. */
  private boolean m_isSorted = true;

  private final Comparator<IRecord> m_sortComparator = new Comparator<IRecord>()
  {
    public int compare( final IRecord o1, final IRecord o2 )
    {
      IComponent[] sortComponents = getSortComponents();
      for( final IComponent component : sortComponents )
      {
        final int index = indexOfComponent( component );
        if( index == -1 )
          continue;

        final Object v1 = o1.getValue( index );
        final Object v2 = o2.getValue( index );

        final int result = component.compare( v1, v2 );

        /* obj differs? no further sorting needed! */
        if( result != 0 )
          return result;
      }

      /* return equals */
      return 0;
    }
  };

  public TupleResult( )
  {
    // default constructor
  }

  public TupleResult( final IComponent[] comps )
  {
    for( final IComponent element : comps )
      addComponent( element );
  }

  public void setSortComponents( final IComponent[] comps )
  {
    m_sortComponents.clear();
    for( final IComponent component : comps )
      m_sortComponents.add( component );

    m_isSorted = false;

    // TODO: check: should be rather fireSortComponentChanged(TYPE.SORT) ???
    fireRecordsChanged( null, TYPE.CHANGED );
  }

  public IComponent[] getSortComponents( )
  {
    return m_sortComponents.toArray( new IComponent[m_sortComponents.size()] );
  }

  /**
   * Invalidates the sort state.<br>
   * The next access to any data results in sorting.<br>
   * Not intended to be called by client other than {@link Record}.
   */
  /* default */boolean invalidateSort( final int index )
  {
    final IComponent component = m_components.get( index );
    if( m_sortComponents.contains( component ) )
    {
      m_isSorted = false;
      return true;
    }

    return false;
  }

  /**
   * Sorts this tuple result by its sort components.<br>
   * This method should only be called by the {@link Record} class or this.
   */
  /* default */void sort( )
  {
    if( m_isSorted == true )
      return;

    if( m_sortComponents.size() == 0 )
      return;

    Collections.sort( m_records, m_sortComparator );

    m_isSorted = true;
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    return "TupleResult: " + getComponents();
  }

  /**
   * @see java.util.List#add(int, E)
   */
  public void add( final int index, final IRecord element )
  {
    checkRecord( element );

    m_records.add( index, element );

    fireRecordsChanged( new IRecord[] { element }, TYPE.ADDED );
  }

  /**
   * @see java.util.List#add(E)
   */
  public boolean add( final IRecord o )
  {
    checkRecord( o );

    final boolean result = m_records.add( o );

    fireRecordsChanged( new IRecord[] { o }, TYPE.ADDED );

    return result;
  }

  /**
   * @see java.util.List#addAll(java.util.Collection)
   */
  public boolean addAll( final Collection< ? extends IRecord> c )
  {
    checkRecords( c );

    final boolean result = m_records.addAll( c );

    fireRecordsChanged( c.toArray( new IRecord[c.size()] ), TYPE.ADDED );

    return result;
  }

  /**
   * @see java.util.List#addAll(int, java.util.Collection)
   */
  public boolean addAll( final int index, final Collection< ? extends IRecord> c )
  {
    checkRecords( c );

    final boolean result = m_records.addAll( index, c );

    fireRecordsChanged( c.toArray( new IRecord[c.size()] ), TYPE.ADDED );

    return result;
  }

  /**
   * @see java.util.List#clear()
   */
  public void clear( )
  {
    final IRecord[] oldRecords = m_records.toArray( new IRecord[m_records.size()] );

    m_records.clear();
    m_isSorted = true; // empty list is sorted

    fireRecordsChanged( oldRecords, TYPE.REMOVED );
  }

  /**
   * @see java.util.List#contains(java.lang.Object)
   */
  public boolean contains( final Object o )
  {
    return m_records.contains( o );
  }

  /**
   * @see java.util.List#containsAll(java.util.Collection)
   */
  public boolean containsAll( final Collection< ? > c )
  {
    return m_records.containsAll( c );
  }

  /**
   * @see java.util.List#get(int)
   */
  public IRecord get( final int index )
  {
    sort();

    return m_records.get( index );
  }

  /**
   * @see java.util.List#indexOf(java.lang.Object)
   */
  public int indexOf( final Object o )
  {
    sort();

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
   * IMPORTANT: removing via this iterator does not inform the listeners.
   * 
   * @see java.util.List#iterator()
   */
  public Iterator<IRecord> iterator( )
  {
    sort();

    return m_records.iterator();
  }

  /**
   * @see java.util.List#lastIndexOf(java.lang.Object)
   */
  public int lastIndexOf( final Object o )
  {
    sort();

    return m_records.lastIndexOf( o );
  }

  /**
   * IMPORTANT: removing via this iterator does not inform the listeners.
   * 
   * @see java.util.List#listIterator()
   */
  public ListIterator<IRecord> listIterator( )
  {
    sort();

    return m_records.listIterator();
  }

  /**
   * IMPORTANT: removing via this iterator does not inform the listeners.
   * 
   * @see java.util.List#listIterator(int)
   */
  public ListIterator<IRecord> listIterator( final int index )
  {
    sort();

    return m_records.listIterator( index );
  }

  /**
   * @see java.util.List#remove(int)
   */
  public IRecord remove( final int index )
  {
    sort();

    final IRecord result = m_records.remove( index );

    fireRecordsChanged( new IRecord[] { result }, TYPE.REMOVED );

    return result;
  }

  /**
   * @see java.util.List#remove(java.lang.Object)
   */
  public boolean remove( final Object o )
  {
    final boolean result = m_records.remove( o );

    if( result )
      fireRecordsChanged( new IRecord[] { (IRecord) o }, TYPE.REMOVED );

    return result;
  }

  /**
   * @see java.util.List#removeAll(java.util.Collection)
   */
  public boolean removeAll( final Collection< ? > c )
  {
    final boolean removeAll = m_records.removeAll( c );

    final Object[] objects = c.toArray();
    final IRecord[] removedRecords = new IRecord[objects.length];
    for( int i = 0; i < objects.length; i++ )
      removedRecords[i] = (IRecord) objects[i];

    fireRecordsChanged( removedRecords, TYPE.REMOVED );

    return removeAll;
  }

  /**
   * @see java.util.List#retainAll(java.util.Collection)
   */
  public boolean retainAll( final Collection< ? > c )
  {
    fireRecordsChanged( null, TYPE.REMOVED );

    return m_records.retainAll( c );
  }

  /**
   * @see java.util.List#set(int, E)
   */
  public IRecord set( final int index, final IRecord element )
  {
    final IRecord result = m_records.set( index, element );

    m_isSorted = false;

    fireRecordsChanged( new IRecord[] { element }, TYPE.CHANGED );

    return result;
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
  public List<IRecord> subList( final int fromIndex, final int toIndex )
  {
    // TODO: problem:
    // - listeners do not get informed
    // - sorting is not maintained
    // TODO: implement a special sub-list

    sort();

    return m_records.subList( fromIndex, toIndex );
  }

  /**
   * @see java.util.List#toArray()
   */
  public Object[] toArray( )
  {
    sort();

    return m_records.toArray();
  }

  /**
   * @see java.util.List#toArray(T[])
   */
  public <T> T[] toArray( final T[] a )
  {
    sort();

    return m_records.toArray( a );
  }

  private void checkRecord( final IRecord record )
  {
    final Record r = (Record) record;

// Assert.isTrue( r.getCount() == m_components.size(), "Number of records values not equal to number of components" );
    Assert.isTrue( r.getOwner() == this, "Illegal record. Record was not created on this tuple result: " + record );
  }

  private void checkRecords( final Collection< ? extends IRecord> c )
  {
    for( final IRecord record : c )
      checkRecord( record );
  }

  //
  // COMPONENTS
  //

  public IComponent[] getComponents( )
  {
    return m_components.toArray( new IComponent[] {} );
  }

  /**
   * Adds a component to this tuple result. Does nothing if an equal component was already added.
   */
  public final boolean addComponent( final IComponent comp )
  {
    final boolean added = m_components.add( comp );

    for( final IRecord record : this )
    {
      final Record r = (Record) record;
      r.set( m_components.size() - 1, comp.getDefaultValue() );
    }

    if( m_sortComponents.contains( comp ) )
      m_isSorted = false;

    fireComponentsChanged( new IComponent[] { comp }, TYPE.ADDED );
    return added;
  }

  public boolean removeComponent( final IComponent comp )
  {
    final boolean b = m_components.remove( comp );

    final Map<IComponent, Integer> remove = new IdentityHashMap<IComponent, Integer>();
    for( int i = 0; i < m_components.size(); i++ )
    {
      final IComponent component = m_components.get( i );
      if( component.equals( comp ) )
        remove.put( component, i );
    }

    final Set<Entry<IComponent, Integer>> entrySet = remove.entrySet();
    for( final Entry<IComponent, Integer> entry : entrySet )
    {
      m_components.remove( entry.getValue() );

      for( final IRecord record : this )
      {
        final Record r = (Record) record;
        r.remove( entry.getValue() );
      }
    }

    if( m_sortComponents.contains( comp ) )
      m_isSorted = false;

    fireComponentsChanged( new IComponent[] { comp }, TYPE.REMOVED );

    return b;
  }

  // TODO: add other component methods:
  // - addComponent( index, comp )
  // - removeComponent( index )
  // - setComponent( index, comp )

  /** This method creates, but DOES NOT adds a record. */
  // TODO its very confusing - create should add
  public IRecord createRecord( )
  {
    return new Record( this, getComponents() );
  }

  public boolean hasComponent( final IComponent comp )
  {
    return m_components.contains( comp );
  }

  /**
   * Add a listener to the list of listeners which will be informed of changes to tuples. Has no effect if the same
   * listener is already registered.
   */
  public void addChangeListener( final ITupleResultChangedListener l )
  {
    m_listeners.add( l );
  }

  public void removeChangeListener( final ITupleResultChangedListener l )
  {
    m_listeners.remove( l );
  }

  /* default */void fireValuesChanged( final ValueChange[] changes )
  {
    final ITupleResultChangedListener[] listeners = m_listeners.toArray( new ITupleResultChangedListener[m_listeners.size()] );
    for( final ITupleResultChangedListener l : listeners )
    {
      try
      {
        l.valuesChanged( changes );
      }
      catch( final Throwable e )
      {
        final IStatus status = StatusUtilities.statusFromThrowable( e, "Exception while propagating tuple-change" );
        KalypsoCorePlugin.getDefault().getLog().log( status );
      }
    }
  }

  /* default */void fireRecordsChanged( final IRecord[] records, final TYPE type )
  {
    final ITupleResultChangedListener[] listeners = m_listeners.toArray( new ITupleResultChangedListener[m_listeners.size()] );
    for( final ITupleResultChangedListener l : listeners )
    {
      try
      {
        l.recordsChanged( records, type );
      }
      catch( final Throwable e )
      {
        final IStatus status = StatusUtilities.statusFromThrowable( e, "Exception while propagating tuple-change" );
        KalypsoCorePlugin.getDefault().getLog().log( status );
      }
    }
  }

  private void fireComponentsChanged( final IComponent[] components, final TYPE type )
  {
    final ITupleResultChangedListener[] listeners = m_listeners.toArray( new ITupleResultChangedListener[m_listeners.size()] );
    for( final ITupleResultChangedListener l : listeners )
    {
      try
      {
        l.componentsChanged( components, type );
      }
      catch( final Throwable e )
      {
        final IStatus status = StatusUtilities.statusFromThrowable( e, "Exception while propagating tuple-change" );
        KalypsoCorePlugin.getDefault().getLog().log( status );
      }
    }
  }

  public int indexOfComponent( final IComponent comp )
  {
    return m_components.indexOf( comp );
  }

  public IComponent getComponent( final int index ) throws IndexOutOfBoundsException
  {
    return m_components.get( index );
  }
}
