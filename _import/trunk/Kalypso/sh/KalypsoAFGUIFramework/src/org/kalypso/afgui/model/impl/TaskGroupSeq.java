package org.kalypso.afgui.model.impl;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

import org.kalypso.afgui.model.ISubTaskGroup;
import org.kalypso.afgui.model.ITaskGroup;
import org.kalypso.afgui.model.ITaskGroupSeq;

import com.hp.hpl.jena.rdf.model.NodeIterator;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Seq;

//TODO use a sequence subclass and  generics for TaskGroupSeq

/**
 * @author Patrice Congo
 */
public class TaskGroupSeq implements ITaskGroupSeq
{
  private Seq seq;

  public TaskGroupSeq( Seq seq )
  {
    assertNotNull( seq );
    this.seq = seq;
  }

  final static private int toSeqIndex( int index )
  {
    return index + 1;
  }

  final static private void assertIndexInBounds( int index, int size )
  {
    if( index >= size || index < 0 )
    {
      throw new ArrayIndexOutOfBoundsException();
    }

  }

  final static private void assertNotNull( Object o )
  {
    if( o == null )
    {
      throw new NullPointerException();
    }
  }

  /**
   * @see java.util.List#add(java.lang.Object)
   */
  public boolean add( ITaskGroup o )
  {
    assertNotNull( o );
    Resource res = (Resource) o.getModelObject();
    if( res == null )
    {
      throw new IllegalArgumentException();
    }
    else
    {
      seq.add( res );
      return true;
    }
  }

  /**
   * @see java.util.List#add(int, java.lang.Object)
   */
  public void add( int index, ITaskGroup element )
  {
    assertIndexInBounds( index, size() );
    assertNotNull( element );
    Resource res = (Resource) element.getModelObject();
    if( res == null )
    {
      throw new IllegalArgumentException();
    }
    else
    {
      seq.add( toSeqIndex( index ), res );
    }
  }

  /**
   * @see java.util.List#addAll(java.util.Collection)
   */
  public boolean addAll( Collection< ? extends ITaskGroup> c )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see java.util.List#addAll(int, java.util.Collection)
   */
  public boolean addAll( int index, Collection< ? extends ITaskGroup> c )
  {
    throw new UnsupportedOperationException();
  }

  /*
   * (non-Javadoc)
   * 
   * @see java.util.List#clear()
   */
  public void clear( )
  {
    for( int i = size(); i > 0; i-- )
    {
      seq.remove( i );
    }

  }

  /*
   * (non-Javadoc)
   * 
   * @see java.util.List#contains(java.lang.Object)
   */
  public boolean contains( Object o )
  {
    assertNotNull( o );
    if( o instanceof ITaskGroup )
    {
      try
      {
        return seq.contains( (Resource) ((ITaskGroup) o).getModelObject() );
      }
      catch( Throwable th )
      {
        return false;
      }
    }
    else
    {
      return false;
    }
  }

  /*
   * (non-Javadoc)
   * 
   * @see java.util.List#containsAll(java.util.Collection)
   */
  public boolean containsAll( Collection< ? > c )
  {
    throw new UnsupportedOperationException();
  }

  /*
   * (non-Javadoc)
   * 
   * @see java.util.List#get(int)
   */
  public ITaskGroup get( int index )
  {
    assertIndexInBounds( index, size() );
    TaskGroup tg = new TaskGroup( seq.getResource( toSeqIndex( index ) ) );
    return tg;
  }

  /*
   * (non-Javadoc)
   * 
   * @see java.util.List#indexOf(java.lang.Object)
   */
  public int indexOf( Object o )
  {
    assertNotNull( o );
    if( o instanceof ITaskGroup )
    {
      Resource res = (Resource) ((ITaskGroup) o).getModelObject();
      if( res == null )
      {
        throw new IllegalArgumentException();
      }
      else
      {
        return seq.indexOf( res ) - 1;
      }
    }
    else
    {
      return -1;
    }
  }

  /*
   * (non-Javadoc)
   * 
   * @see java.util.List#isEmpty()
   */
  public boolean isEmpty( )
  {
    return seq.size() == 0;
  }

  /*
   * (non-Javadoc)
   * 
   * @see java.util.List#iterator()
   */
  public Iterator<ITaskGroup> iterator( )
  {
    return new Iterator<ITaskGroup>()
    {
      NodeIterator nit = seq.iterator();

      public boolean hasNext( )
      {
        return nit.hasNext();
      }

      public ITaskGroup next( )
      {
        return new TaskGroup( (Resource) nit.next() );
      }

      public void remove( )
      {
        nit.remove();
      }

    };
  }

  /**
   * @see java.util.List#lastIndexOf(java.lang.Object)
   */
  public int lastIndexOf( Object o )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see java.util.List#listIterator()
   */
  public ListIterator<ITaskGroup> listIterator( )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see java.util.List#listIterator(int)
   */
  public ListIterator<ITaskGroup> listIterator( int index )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see java.util.List#remove(java.lang.Object)
   */
  public boolean remove( Object o )
  {
    assertNotNull( o );
    if( o instanceof ITaskGroup )
    {
      Resource res = (Resource) ((ITaskGroup) o).getModelObject();
      if( res == null )
      {
        throw new IllegalArgumentException();
      }
      else
      {
        int seqIndex = seq.indexOf( res );
        if( seqIndex > 0 )
        {
          seq.remove( seqIndex );
          return true;
        }
        else
        {
          return false;
        }
      }
    }
    else
    {
      return false;
    }
  }

  /**
   * @see java.util.List#remove(int)
   */
  public ITaskGroup remove( int index )
  {
    assertIndexInBounds( index, size() );
    int seqIndex = toSeqIndex( index );
    ITaskGroup tg = new TaskGroup( seq.getResource( seqIndex ) );
    seq.remove( seqIndex );
    return tg;
  }

  /**
   * @see java.util.List#removeAll(java.util.Collection)
   */
  public boolean removeAll( Collection< ? > c )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see java.util.List#retainAll(java.util.Collection)
   */
  public boolean retainAll( Collection< ? > c )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see java.util.List#set(int, java.lang.Object)
   */
  public ITaskGroup set( int index, ITaskGroup element )
  {
    assertIndexInBounds( index, size() );
    assertNotNull( element );
    Resource res = (Resource) element.getModelObject();
    if( res == null )
    {
      throw new IllegalArgumentException();
    }
    else
    {
      int seqIndex = toSeqIndex( index );
      ITaskGroup tg = new TaskGroup( seq.getResource( seqIndex ) );
      seq.set( seqIndex, res );
      return tg;
    }
  }

  /**
   * @see java.util.List#size()
   */
  public int size( )
  {
    return seq.size();
  }

  /**
   * @see java.util.List#subList(int, int)
   */
  public List<ITaskGroup> subList( int fromIndex, int toIndex )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see java.util.List#toArray()
   */
  public Object[] toArray( )
  {
    Object[] objs = new Object[size()];

    return toArray( objs );
  }

  /**
   * @see java.util.List#toArray(T[])
   */
  public <T> T[] toArray( T[] a )
  {
    if( a == null )
    {
      throw new NullPointerException();
    }
    else
    {
      if( size() > a.length )
      {
        throw new IllegalArgumentException();
      }
      for( int i = 0; i < a.length; i++ )
      {
        a[i] = (T) get( i );
      }
      return a;
    }
  }

  @Override
  public boolean equals( Object obj )
  {
    if( obj instanceof List )
    {
      List l = (List) obj;
      final int SIZE = size();
      if( SIZE == l.size() )
      {
        for( int i = 0; i < SIZE; i++ )
        {
          if( !get( i ).equals( l.get( i ) ) )
          {
            return false;
          }
        }
        return true;
      }
      else
      {
        return false;
      }
    }
    else
    {
      return false;
    }
  }

}
