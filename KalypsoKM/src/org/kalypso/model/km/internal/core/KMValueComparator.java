package org.kalypso.model.km.internal.core;

import java.util.Comparator;

/**
 * @author belger
 *
 */
final class KMValueComparator implements Comparator<IKMValue>
{
  @Override
  public int compare( final IKMValue km1, final IKMValue km2 )
  {
    return Double.compare( km1.getQSum(), km2.getQSum() );
  }
}