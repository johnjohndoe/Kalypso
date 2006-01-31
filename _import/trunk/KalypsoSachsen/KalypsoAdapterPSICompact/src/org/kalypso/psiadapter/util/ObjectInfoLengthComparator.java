package org.kalypso.psiadapter.util;

import java.util.Comparator;

import de.psi.go.lhwz.PSICompact.ObjectInfo;

/**
 * Helper class. Wird benutzt um ObjectInfo anhand deren ID länge zu sortieren.
 * 
 * @author schlienger
 */
public class ObjectInfoLengthComparator implements Comparator<ObjectInfo>
{
  /**
   * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
   */
  public int compare( final ObjectInfo arg0, final ObjectInfo arg1 )
  {
    final String l1 = arg0.getId();
    final String l2 = arg1.getId();

    return l1.compareTo( l2 );
  }
}
