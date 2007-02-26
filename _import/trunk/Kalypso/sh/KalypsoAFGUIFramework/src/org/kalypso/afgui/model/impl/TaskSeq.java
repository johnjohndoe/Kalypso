package org.kalypso.afgui.model.impl;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

import org.kalypso.afgui.model.ISubTaskGroup;
import org.kalypso.afgui.model.ITask;
import org.kalypso.afgui.model.ITaskSeq;

import com.hp.hpl.jena.rdf.model.NodeIterator;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Seq;

public class TaskSeq implements ITaskSeq
{
  final private Seq seq;

  public TaskSeq( Seq seq )
  {
    if( seq == null )
    {
      throw new IllegalArgumentException();
    }
    this.seq = seq;
  }

  public TaskSeq( Resource resource )
  {
    if( resource instanceof Seq )
    {
      this.seq = (Seq) resource;
    }
    else
    {
      throw new IllegalArgumentException();
    }
  }

  public boolean add( ITask o )
  {
    if( o == null )
    {
      return false;
    }
    else
    {
      Resource res = (Resource) o.getModelObject();
      if( res == null )
      {
        return false;
      }
      else
      {
        seq.add( res );
        return true;
      }
    }
  }

  public void add( int index, ITask element )
  {
    assertIndexInBound( index, size() );

    if( element == null )
    {
      throw new IllegalArgumentException();
    }
    else
    {
      Resource resource = (Resource) element.getModelObject();
      if( resource == null )
      {
        throw new IllegalArgumentException();
      }
      else
      {
        seq.add( toSeqIndex( index ), element );
      }
    }

  }

  public boolean addAll( Collection< ? extends ITask> c )
  {
    throw new UnsupportedOperationException();
  }

  public boolean addAll( int index, Collection< ? extends ITask> c )
  {
    throw new UnsupportedOperationException();
  }

  public void clear( )
  {
    for( int i = size(); i > 1; i-- )
    {
      seq.remove( i );
    }
  }

  public boolean contains( Object o )
  {
    if( o == null )
    {
      return false;
    }
    else if( o instanceof ITask )
    {
      return seq.contains( ((ITask) o).getModelObject() );
    }
    else
    {
      return false;
    }
  }

  public boolean containsAll( Collection< ? > c )
  {
    if( c == null )
    {
      return false;
    }
    else
    {
      for( Object o : c )
      {
        if( !contains( o ) )
        {
          return false;
        }
      }
      return true;
    }
  }

  public ITask get( int index )
  {
    assertIndexInBound( index, size() );
    try
    {
      return new Task( seq.getResource( toSeqIndex( index ) ) );
    }
    catch( Throwable th )
    {
      throw new RuntimeException( "bad type in sequence" );
    }
  }

  public int indexOf( Object o )
  {
    if( o instanceof ITask )
    {
      Resource res = (Resource) ((ITask) o).getModelObject();
      if( res == null )
      {
        return -1;
      }
      else
      {
        int seqIndex = seq.indexOf( res );
        return seqIndex - 1;
      }
    }
    else
    {
      return -1;
    }
  }

  public boolean isEmpty( )
  {
    return seq.size() == 0;
  }

  public Iterator<ITask> iterator( )
  {
    return new Iterator<ITask>()
    {
      NodeIterator it = seq.iterator();

      public boolean hasNext( )
      {
        return it.hasNext();
      }

      public ITask next( )
      {
        return new Task( (Resource) it.nextNode() );
      }

      public void remove( )
      {
        throw new UnsupportedOperationException();
      }

    };
  }

  public int lastIndexOf( Object o )
  {
    throw new UnsupportedOperationException();
  }

  public ListIterator<ITask> listIterator( )
  {
    throw new UnsupportedOperationException();
  }

  public ListIterator<ITask> listIterator( int index )
  {
    throw new UnsupportedOperationException();
  }

  public boolean remove( Object o )
  {
    if( o instanceof ITask )
    {
      Resource res = (Resource) ((ITask) o).getModelObject();
      if( res == null )
      {
        throw new IllegalArgumentException();
      }
      else
      {
        int i = seq.indexOf( res );
        if( i != 0 )
        {
          return false;
        }
        else
        {
          seq.remove( i );
          return true;
        }
      }
    }
    else
    {
      return false;
    }
  }

  public ITask remove( int index )
  {
    assertIndexInBound( index, size() );
    Resource res = seq.getResource( toSeqIndex( index ) );
    return new Task( res );
  }

  public boolean removeAll( Collection< ? > c )
  {
    throw new UnsupportedOperationException();
  }

  public boolean retainAll( Collection< ? > c )
  {
    throw new UnsupportedOperationException();
  }

  public ITask set( int index, ITask element )
  {
    assertIndexInBound( index, size() );
    ITask oldTask = new Task( seq.getResource( index ) );
    seq.set( index + 1, element.getModelObject() );
    return oldTask;
  }

  public int size( )
  {
    return seq.size();
  }

  public List<ITask> subList( int fromIndex, int toIndex )
  {
    throw new UnsupportedOperationException();
  }

  public Object[] toArray( )
  {
    int size = size();
    Object[] array = new Object[size];
    toArray( array );
    return array;
  }

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
    if( obj instanceof TaskSeq )
    {
      try
      {
        return seq.equals( ((TaskSeq) obj).seq );
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

  private static final void assertIndexInBound( int index, int size )
  {
    if( index >= size || index < 0 )
    {
      throw new IndexOutOfBoundsException();
    }
  }

  private static final int toSeqIndex( int index )
  {
    return index + 1;
  }
}
