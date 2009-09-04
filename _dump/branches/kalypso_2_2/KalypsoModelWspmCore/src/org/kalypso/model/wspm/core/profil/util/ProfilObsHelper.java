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
package org.kalypso.model.wspm.core.profil.util;

import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

/**
 * Profile Observation Helper<br>
 * Functions around new observation stuff of IProfile (extends IObservation)
 * 
 * @author kimwerner, kuch
 */

public class ProfilObsHelper
{
  /**
   * @deprecated Use {@link IProfil#hasPointProperty(String)} instead.
   */
  @Deprecated
  public static IComponent getPropertyFromId( final IRecord point, final String id )
  {
    if( id == null )
      return null;

    final TupleResult result = point.getOwner();

    final IComponent[] components = result.getComponents();
    for( final IComponent component : components )
    {
      if( id.equals( component.getId() ) )
        return component;
    }

    return null;
  }

  /**
   * @deprecated Use {@link IProfil#hasPointProperty(String)} instead.
   */
  @Deprecated
  public static IComponent getPropertyFromId( final IProfil profil, final String id )
  {
    if( id == null )
      return null;

    final IComponent[] properties = profil.getPointProperties();

    for( final IComponent property : properties )
    {
      if( id.equals( property.getId() ) )
        return property;
    }

    return null;
  }

  

 
  
  
}
