package org.kalypso.model.wspm.tuhh.core.profile.pointfilter;

import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfilePointMarker;
import org.kalypso.model.wspm.core.profil.filter.AbstractProfilePointFilter;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;

public class BvRechtsProfilePointFilter extends AbstractProfilePointFilter
{
  @Override
  public boolean accept( final IProfile profil, final IProfileRecord point )
  {
    final IProfilePointMarker[] tfMarkers = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    if( tfMarkers.length != 2 )
      return false;

    final IProfilePointMarker[] bvMarkers = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_BORDVOLL );
    if( bvMarkers.length != 2 )
      return false;

    return isBetweenMarkers( profil, point, bvMarkers[1], tfMarkers[1] );
  }
}
