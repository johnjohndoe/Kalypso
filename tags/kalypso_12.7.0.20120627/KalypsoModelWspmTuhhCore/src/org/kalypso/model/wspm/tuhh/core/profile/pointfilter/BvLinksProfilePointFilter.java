package org.kalypso.model.wspm.tuhh.core.profile.pointfilter;

import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.filter.AbstractProfilePointFilter;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;

public class BvLinksProfilePointFilter extends AbstractProfilePointFilter
{
  @Override
  public boolean accept( final IProfil profil, final IProfileRecord point )
  {
    final IProfilPointMarker[] tfMarkers = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    if( tfMarkers.length != 2 )
      return false;
    final IProfilPointMarker[] bvMarkers = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_BORDVOLL );
    if( bvMarkers.length != 2 )
      return false;

    return isBetweenMarkers( profil, point, bvMarkers[0], tfMarkers[0] );
  }
}
