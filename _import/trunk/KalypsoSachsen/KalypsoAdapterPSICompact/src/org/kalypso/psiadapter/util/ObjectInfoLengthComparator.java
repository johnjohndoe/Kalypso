package org.kalypso.psiadapter.util;

import java.util.Comparator;

import de.psi.go.lhwz.PSICompact.ObjectInfo;

/**
 * Helper class. Wird benutzt um ObjectInfo anhand deren ID l�nge zu sortieren.
 * 
 * @author schlienger
 */
public class ObjectInfoLengthComparator implements Comparator
{
  /**
   * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
   */
  public int compare( Object arg0, Object arg1 )
  {
    int l1 = ( (ObjectInfo)arg0 ).getId().length();
    int l2 = ( (ObjectInfo)arg1 ).getId().length();

    if( l1 > l2 )
      return -1;
    else if( l1 < l2 )
      return 1;
    else
      return 0;
  }
}
