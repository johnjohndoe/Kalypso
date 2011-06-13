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
    return p1.getStation().compareTo( p2.getStation() );
  }
}