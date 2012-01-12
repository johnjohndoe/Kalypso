package org.kalypso.model.wspm.tuhh.core.profile.pointfilter;

import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.filter.AbstractProfilePointFilter;
import org.kalypso.observation.result.IRecord;

/**
 * A special profile point filter only for the last profile point <br/>
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
    final int index = profil.indexOfPoint( point );
    if( index == profil.getResult().size() - 1 )
      return true;

    return false;
  }
}
