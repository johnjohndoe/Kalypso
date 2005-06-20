package org.kalypso.psiadapter.util;

import java.util.Comparator;
import java.util.Date;

import de.psi.go.lhwz.PSICompact.ArchiveData;

/**
 * @author schlienger
 *  
 */
public class ArchiveDataDateComparator implements Comparator
{
  /**
   * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
   */
  public int compare( Object o1, Object o2 )
  {
    Date d1 = null;
    Date d2 = null;

    if( o1 instanceof ArchiveData )
      d1 = ( (ArchiveData)o1 ).getTimestamp();
    else if( o1 instanceof Date )
      d1 = (Date)o1;
    else
      throw new IllegalArgumentException( "Cannot compare objects: not a Date nor an ArchiveData" );

    if( o2 instanceof ArchiveData )
      d2 = ( (ArchiveData)o2 ).getTimestamp();
    else if( o2 instanceof Date )
      d2 = (Date)o2;
    else
      throw new IllegalArgumentException( "Cannot compare objects: not a Date nor an ArchiveData" );

    if( d1.before( d2 ) )
      return -1;
    else if( d1.after( d2 ) )
      return 1;
    else
      return 0;
  }
}
