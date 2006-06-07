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
package org.kalypso.model.wspm.core.profil;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;

import org.kalypso.model.wspm.core.profil.IProfilDevider.DEVIDER_TYP;

/**
 * Helper class to retrieve deviders of a profile by points.
 * <p>
 * The class is initialized on construction and NOT will not reflect later changes of the profile.
 * </p>
 * 
 * @author belger
 */
public class ProfilDeviderMap extends HashMap<IProfilPoint, Collection<IProfilDevider>>
{
  public ProfilDeviderMap( final IProfil profil )
  {
    for( final DEVIDER_TYP deviderTyp : DEVIDER_TYP.values() )
    {
      final IProfilDevider[] deviders = profil.getDevider( deviderTyp );
      if( deviders == null )
        continue;

      for( final IProfilDevider devider : deviders )
      {
        final IProfilPoint point = devider.getPoint();
        if( !containsKey( point ) )
          put( point, new HashSet<IProfilDevider>( 1 ) );
        final Collection<IProfilDevider> list = get( point );
        list.add( devider );
      }
    }
  }

  /**
   * @see java.util.HashMap#get(java.lang.Object)
   */
  @Override
  public Collection<IProfilDevider> get( final Object key )
  {
    final Collection<IProfilDevider> collection = super.get( key );
    if( collection == null )
      return Collections.emptyList();
    
    return collection;
  }

}
