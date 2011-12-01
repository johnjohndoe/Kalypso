package org.kalypso.model.km.internal.core;

import java.util.Comparator;

/**
 * @author Gernot Belger
 */
final class ProfileDataComparator implements Comparator<ProfileData>
{
  @Override
  public int compare( final ProfileData p1, final ProfileData p2 )
  {
    return Double.compare( p1.getPosition(), p2.getPosition() );
  }
}