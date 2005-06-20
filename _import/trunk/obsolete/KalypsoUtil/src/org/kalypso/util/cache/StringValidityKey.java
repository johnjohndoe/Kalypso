package org.kalypso.util.cache;

import java.util.Comparator;
import java.util.Date;

/**
 * StringValidityKey
 * 
 * @author schlienger
 */
public class StringValidityKey
{
  private final String m_string;

  private final Date m_validity;

  public StringValidityKey( final String string, final Date validity )
  {
    m_string = string;
    m_validity = validity;
  }

  public Date getValidity()
  {
    return m_validity;
  }

  public String getString()
  {
    return m_string;
  }

  public static Comparator createComparatorForStringCompareOnly()
  {
    return new StringComparator();
  }

  private static class StringComparator implements Comparator
  {
    public int compare( final Object o1, final Object o2 )
    {
      final StringValidityKey k1 = (StringValidityKey)o1;
      final StringValidityKey k2 = (StringValidityKey)o2;

      return k1.getString().compareTo( k2.getString() );
    }
  }
}
