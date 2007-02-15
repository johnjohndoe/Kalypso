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
import java.util.LinkedHashSet;
import java.util.List;
import java.util.ListIterator;
import java.util.Set;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.observation.result.ITupleResultChangedListener.TYPE;
import org.kalypso.observation.result.ITupleResultChangedListener.ValueChange;

/**
 * TODO: propagate all events to listeners
 * 
 * @author Marc Schlienger
 */
public class TupleResult implements List<IRecord>
{
  private final List<IRecord> m_records = new ArrayList<IRecord>();

  private final Set<IComponent> m_components = new LinkedHashSet<IComponent>();

  private final Set<ITupleResultChangedListener> m_listeners = new HashSet<ITupleResultChangedListener>();

  public TupleResult( )
  {
    // default constructor
  }

  public TupleResult( final IComponent[] comps )
  {
    for( int i = 0; i < comps.length; i++ )
      addComponent( comps[i] );
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

    fireRecordsChanged( oldRecords, TYPE.REMOVED );
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
   * Carefull: removing via this iterator does not inform the listeners.
   * 
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
   * Carefull: removing via this iterator does not inform the listeners.
   * 
   * @see java.util.List#listIterator()
   */
  public ListIterator<IRecord> listIterator( )
  {
    return m_records.listIterator();
  }

  /**
   * Carefull: removing via this iterator does not inform the listeners.
   * 
   * @see java.util.List#listIterator(int)
   */
  public ListIterator<IRecord> listIterator( int index )
  {
    return m_records.listIterator( index );
  }

  /**
   * @see java.util.List#remove(int)
   */
  public IRecord remove( final int index )
  {
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
    // TODO: inform listeners

    return m_records.retainAll( c );
  }

  /**
   * @see java.util.List#set(int, E)
   */
  public IRecord set( final int index, final IRecord element )
  {
    // TODO: inform listeners

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

  /**
   * Adds a component to this tuple result. Does nothing if an equal component was already added.
   */
  public final boolean addComponent( final IComponent comp )
  {
    final boolean added = m_components.add( comp );
    fireComponentsChanged( new IComponent[] { comp }, TYPE.ADDED );
    return added;
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

    fireComponentsChanged( new IComponent[] { comp }, TYPE.REMOVED );

    return b;
  }

  public Object getValue( final IRecord record, final IComponent comp )
  {
    return record.getValue( comp );
  }

  /**
   * Sets the value of a record and informs the listeners if the value has really changed.
   * <p>
   * In order to change several records you should call {@link IRecord#setValue(IComponent, Object)} directly and then
   * call {@link #fireTupleChanged(IRecord, IComponent, Object)}.
   * </p>
   */
  public void setValue( final IRecord record, final IComponent comp, final Object value )
  {
    final Object oldValue = record.getValue( comp );
    if( oldValue == null )
    {
      if( value == null )
        return;
    }
    else if( oldValue.equals( value ) )
      return;

    record.setValue( comp, value );

    final ValueChange[] changes = new ValueChange[] { new ValueChange( record, comp, value ) };
    fireValuesChanged( changes );
  }

  /** This method creates, but DOES NOT adds a record. */
  public IRecord createRecord( )
  {
    return new Record( this, m_components );
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

  public void fireValuesChanged( final ValueChange[] changes )
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

  public void fireRecordsChanged( final IRecord[] records, final TYPE type )
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

  public void fireComponentsChanged( final IComponent[] components, final TYPE type )
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
}
