package org.kalypso.util.pool;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Comparator;

import org.kalypso.util.url.UrlResolver;


class KeyComparator implements Comparator
{
  private final UrlResolver m_urlResolver = new UrlResolver();

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
        final URL sourceURL1 = m_urlResolver.resolveURL( k1.getContext(), k1.getLocation() );
        final URL sourceURL2 = m_urlResolver.resolveURL( k2.getContext(), k2.getLocation() );

        return sourceURL1.hashCode() - sourceURL2.hashCode();
    }
    catch( MalformedURLException e )
    {
      e.printStackTrace();
    }

    return 0;
  }
}