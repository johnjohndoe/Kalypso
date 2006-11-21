package org.kalypsodeegree_impl.model.sort;

import java.util.ListIterator;

/**
 * Ein einfacher ListIterator auf einem Array.
 * 
 * Alle Methoden, die die Liste verändern führen zu einer {@link java.lang.UnsupportedOperationException}.
 * 
 * @author belger
 */
public class ArrayIterator implements ListIterator
{

  private int m_index;

  private final Object[] m_objects;

  public ArrayIterator( final int index, final Object[] objects )
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
  public Object next()
  {
    return m_objects[m_index++];
  }

  /**
   * @see java.util.ListIterator#previous()
   */
  public Object previous()
  {
    m_index--;
    return m_objects[m_index];
  }

  /**
   * @see java.util.ListIterator#add(java.lang.Object)
   */
  public void add( Object o )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see java.util.ListIterator#set(java.lang.Object)
   */
  public void set( Object o )
  {
    throw new UnsupportedOperationException();
  }
}
