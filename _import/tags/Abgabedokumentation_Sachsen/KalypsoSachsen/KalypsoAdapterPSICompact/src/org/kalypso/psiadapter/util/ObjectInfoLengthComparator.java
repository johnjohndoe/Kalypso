package org.kalypso.psiadapter.util;

import java.util.Comparator;

import de.psi.go.lhwz.PSICompact.ObjectInfo;

/**
 * Helper class. Wird benutzt um ObjectInfo anhand deren ID länge zu sortieren.
 * 
 * @author schlienger
 */
public class ObjectInfoLengthComparator implements Comparator
{
  /**
   * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
   */
  public int compare( final Object arg0, final Object arg1 )
  {
    final String l1 = ( (ObjectInfo)arg0 ).getId();
    final String l2 = ( (ObjectInfo)arg1 ).getId();

    return l1.compareTo( l2 );
  }
}
