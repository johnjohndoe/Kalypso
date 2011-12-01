package org.kalypso.model.wspm.tuhh.core.profile.pointfilter;

import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.filter.AbstractProfilePointFilter;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.observation.result.IRecord;

public class BvRechtsProfilePointFilter extends AbstractProfilePointFilter
{
  @Override
  public boolean accept( final IProfil profil, final IRecord point )
  {
    final IProfilPointMarker[] tfMarkers = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    if( tfMarkers.length != 2 )
      return false;

    final IProfilPointMarker[] bvMarkers = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_BORDVOLL );
    if( bvMarkers.length != 2 )
      return false;

    return isBetweenMarkers( profil, point, bvMarkers[1], tfMarkers[1] );
  }
}
