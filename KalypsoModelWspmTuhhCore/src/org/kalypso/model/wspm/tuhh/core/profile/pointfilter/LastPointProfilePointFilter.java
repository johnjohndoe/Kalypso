package org.kalypso.model.wspm.tuhh.core.profile.pointfilter;

import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.filter.AbstractProfilePointFilter;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.observation.result.IRecord;

/**
 * A special profile point filter only for the last profile point (i.e. point on right DB)<br/>
 * This is a special case, that handles the problem that the roughness on the last point must be set (although it should
 * not be really needed).<br/>
 * This filter is NOT registered via extension point, but directly used by the roughness assignment stuff (because the
 * problem exists only there).
 * 
 * @author Gernot Belger
 */
public class LastPointProfilePointFilter extends AbstractProfilePointFilter
{
  @Override
  public boolean accept( final IProfil profil, final IRecord point )
  {
    final IProfilPointMarker[] dbMarkers = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );

    final IRecord rightDBMarker = getRelevantPoint( profil, dbMarkers );
    if( rightDBMarker == null )
      return false;

    final int index = profil.indexOfPoint( point );
    final int markerIndex = profil.indexOfPoint( rightDBMarker );
    return index == markerIndex;
  }

  private IRecord getRelevantPoint( final IProfil profil, final IProfilPointMarker[] dbMarkers )
  {
    final IRecord[] points = profil.getPoints();
    if( points.length == 0 )
      return null;

    switch( dbMarkers.length )
    {
      case 0:
        return points[points.length - 1];

      case 2:
        return dbMarkers[1].getPoint();

        /* Other cases: something's wrong */
      case 1:
      default:
        return null;
    }
  }
}
