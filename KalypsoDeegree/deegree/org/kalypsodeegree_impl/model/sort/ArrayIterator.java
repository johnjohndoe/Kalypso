package org.kalypsodeegree_impl.model.sort;

import java.util.ListIterator;

/**
 * Ein einfacher ListIterator auf einem Array.
 * 
 * Alle Methoden, die die Liste verändern führen zu einer {@link java.lang.UnsupportedOperationException}.
 * 
 * @author belger
 */
public class ArrayIterator<T> implements ListIterator<T>
{
  private int m_index;

  private final T[] m_objects;

  public ArrayIterator( final int index, final T[] objects )
  {
    m_index = index;
    m_objects = objects;
  }

  /**
   * @see java.util.ListIterator#nextIndex()
   */
  public int nextIndex()
  {
    return m_index;
  }

  /**
   * @see java.util.ListIterator#previousIndex()
   */
  public int previousIndex()
  {
    return m_index - 1;
  }

  /**
   * @see java.util.Iterator#remove()
   */
  public void remove()
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see java.util.Iterator#hasNext()
   */
  public boolean hasNext()
  {
    return m_index < m_objects.length;
  }

  /**
   * @see java.util.ListIterator#hasPrevious()
   */
  public boolean hasPrevious()
  {
    return m_index > 0;
  }

  /**
   * @see java.util.Iterator#next()
   */
  public T next()
  {
    return m_objects[m_index++];
  }

  /**
   * @see java.util.ListIterator#previous()
   */
  public T previous()
  {
    m_index--;
    return m_objects[m_index];
  }

  /**
   * @see java.util.ListIterator#add(java.lang.Object)
   */
  public void add( T o )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see java.util.ListIterator#set(java.lang.Object)
   */
  public void set( T o )
  {
    throw new UnsupportedOperationException();
  }
}
