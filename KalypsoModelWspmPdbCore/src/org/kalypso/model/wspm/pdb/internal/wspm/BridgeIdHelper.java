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
package org.kalypso.model.wspm.pdb.internal.wspm;

import java.util.HashSet;
import java.util.Locale;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.impl.GenericProfileHorizon;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingBruecke;

/**
 * @author Holger Albert
 */
public class BridgeIdHelper
{
  private BridgeIdHelper( )
  {
  }

  public static Set<String> findUsedIds( final IProfileObject[] profileObjects )
  {
    final Set<String> usedIds = new HashSet<>();

    for( final IProfileObject profileObject : profileObjects )
    {
      if( profileObject instanceof BuildingBruecke )
      {
        final String brueckeId = ((BuildingBruecke)profileObject).getBrueckeId();
        if( !StringUtils.isEmpty( brueckeId ) )
          usedIds.add( brueckeId );
      }

      if( profileObject instanceof GenericProfileHorizon )
      {
        if( BuildingBruecke.ID_OK.equals( profileObject.getType() ) )
        {
          final String brueckeId = ((GenericProfileHorizon)profileObject).getValue( BuildingBruecke.KEY_BRUECKE_ID, null );
          if( !StringUtils.isEmpty( brueckeId ) )
            usedIds.add( brueckeId );
        }
      }
    }

    return usedIds;
  }

  public static String findFreeId( final BuildingBruecke bridge, final Set<String> usedIds )
  {
    final String brueckeId = bridge.getBrueckeId();
    if( !StringUtils.isEmpty( brueckeId ) )
      return brueckeId;

    int cnt = 1;
    String freeId = String.format( Locale.PRC, "bridge_%d", cnt++ ); //$NON-NLS-1$
    while( usedIds.contains( freeId ) )
      freeId = String.format( Locale.PRC, "bridge_%d", cnt++ ); //$NON-NLS-1$

    return freeId;
  }

  public static IProfileObject findOkProfileObjectWithoutId( final IProfileObject[] profileObjects )
  {
    for( final IProfileObject profileObject : profileObjects )
    {
      if( !(profileObject instanceof GenericProfileHorizon) )
        continue;

      if( !BuildingBruecke.ID_OK.equals( profileObject.getType() ) )
        continue;

      final String bridgeId = profileObject.getValue( BuildingBruecke.KEY_BRUECKE_ID, null );
      if( bridgeId == null || bridgeId.length() == 0 )
        return profileObject;
    }

    return null;
  }
}