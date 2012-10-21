/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.tuhh.ui.chart.themes;

import java.util.ArrayList;
import java.util.Collection;

import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.energyloss.IEnergylossProfileObject;
import org.kalypso.model.wspm.tuhh.core.profile.sinuositaet.ISinuositaetProfileObject;
import org.kalypso.model.wspm.tuhh.ui.chart.layers.ProfileObjectsLayer;
import org.kalypso.model.wspm.ui.view.chart.AbstractProfilTheme;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer;

import de.openali.odysseus.chart.framework.model.mapper.ICoordinateMapper;

/**
 * The cascading theme holding the
 *
 * @author Gernot Belger
 */
public class ProfileObjectsTheme extends AbstractProfilTheme
{
  private static final String STR_TITLE = "Teilprofile";

  public ProfileObjectsTheme( final IProfile profile, final ICoordinateMapper cm )
  {
    super( profile, IWspmTuhhConstants.LAYER_PROFILE_OBJECTS, STR_TITLE, buildLayers( profile ), cm );
  }

  private static IProfilChartLayer[] buildLayers( final IProfile profile )
  {
    final IProfileObject[] profileObjects = profile.getProfileObjects();

    final Collection<IProfilChartLayer> subLayers = new ArrayList<>( profileObjects.length );

    int count = 0;
    for( final IProfileObject profileObject : profileObjects )
    {
      if( shouldHaveLayer( profileObject ) )
      {
        final String id = profileObject.getType() + count++;
        subLayers.add( new ProfileObjectsLayer( id, profile, profileObject ) );
      }
    }

    return subLayers.toArray( new IProfilChartLayer[subLayers.size()] );
  }

  private static boolean shouldHaveLayer( final IProfileObject profileObject )
  {
    // ignore ojects that have own specialized layers and will never have own records

    if( profileObject instanceof ISinuositaetProfileObject )
      return false;

    if( profileObject instanceof IEnergylossProfileObject )
      return false;

    return true;
  }
}
