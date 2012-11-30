/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.wspm.tuhh.ui.export.wspwin;

import java.util.SortedMap;
import java.util.TreeMap;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.core.profil.IProfile;

/**
 * @author kimwerner
 */

public class ProfileFeatureSorter
{

  public static IProfile[] extractProfiles( final Object[] profilFeatures, final IProgressMonitor monitor )
  {
    final SortedMap<Double, IProfile> profiles = new TreeMap<>();

    for( final Object objProfileFeature : profilFeatures )
    {
      ProgressUtilities.worked( monitor, 1 );
      if( !(objProfileFeature instanceof IProfileFeature) )
      {
        continue;
      }
      final IProfileFeature profileFeature = (IProfileFeature) objProfileFeature;

      final IProfile profil = profileFeature.getProfile();
      final double station = profil.getStation();
      profiles.put( station, profil );
    }

    final IProfile[] sortedProfiles = profiles.values().toArray( new IProfile[profiles.size()] );

    // Sort according to flow direction (i.e. we always start upstreams)
    final WspmWaterBody waterBody = ((IProfileFeature) profilFeatures[0]).getWater();
    final boolean direction = waterBody == null ? true : waterBody.isDirectionUpstreams();
    if( !direction )
      ArrayUtils.reverse( sortedProfiles );

    return sortedProfiles;
  }

}
