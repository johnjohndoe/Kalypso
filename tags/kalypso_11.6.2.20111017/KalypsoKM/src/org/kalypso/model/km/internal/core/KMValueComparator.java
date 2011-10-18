package org.kalypso.model.km.internal.core;

import java.util.Comparator;

/**
 * @author Gernot Belger
 */
final class KMValueComparator implements Comparator<IKMValue>
{
  @Override
  public int compare( final IKMValue km1, final IKMValue km2 )
  {
    return Double.compare( km1.getLowerQ(), km2.getLowerQ() );
  }
}