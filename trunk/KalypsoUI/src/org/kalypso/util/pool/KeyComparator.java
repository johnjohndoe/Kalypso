package org.kalypso.util.pool;

import java.util.Comparator;

import org.kalypso.loader.ILoader;
import org.kalypso.util.factory.FactoryException;


class KeyComparator implements Comparator
{
  private final ResourcePool m_pool;

  /**
   * @param pool
   */
  KeyComparator( ResourcePool pool )
  {
    this.m_pool = pool;
    // TODO Auto-generated constructor stub
  }

  /**
   * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
   */
  public int compare( final Object o1, final Object o2 )
  {
    final IPoolableObjectType k1 = (IPoolableObjectType)o1;
    final IPoolableObjectType k2 = (IPoolableObjectType)o2;

    final int typeCompare = k1.getType().compareToIgnoreCase( k2.getType() );
    if( typeCompare != 0 )
      return typeCompare;

    try
    {
      final ILoader loader = this.m_pool.getLoader( k1.getType() );
      return loader
          .compareKeys( k1.getSource(), k1.getContext(), k2.getSource(), k2.getContext() );
    }
    catch( FactoryException e )
    {
      e.printStackTrace();
    }

    return 0;
  }
}