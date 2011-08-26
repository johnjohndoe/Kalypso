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
package org.kalypso.model.wspm.tuhh.ui.panel.vegetation.utils;

import java.util.LinkedHashSet;
import java.util.Set;

import org.kalypso.commons.java.lang.Objects;
import org.kalypso.model.wspm.core.IWspmPointProperties;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyRemove;
import org.kalypso.model.wspm.core.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.core.profil.operation.ProfilOperationJob;
import org.kalypso.observation.result.IComponent;

/**
 * @author Dirk Kuch
 */
public final class VegetationPanelHelper
{
  private VegetationPanelHelper( )
  {
  }

  public static IComponent[] fromProfile( final IProfil profile )
  {
    final Set<IComponent> found = new LinkedHashSet<IComponent>();

    final IComponent[] properties = profile.getPointProperties();
    for( final IComponent property : properties )
    {
      if( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_AX.equals( property.getId() ) )
        found.add( property );
      else if( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_AY.equals( property.getId() ) )
        found.add( property );
      else if( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_DP.equals( property.getId() ) )
        found.add( property );
    }

    return found.toArray( new IComponent[] {} );
  }

  public static void removeVegetationTypes( final IProfil profile )
  {
    final IComponent ax = profile.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_AX );
    final IComponent ay = profile.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_AY );
    final IComponent dx = profile.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_DP );

    final ProfilOperation operation = new ProfilOperation( "Adding roughness type", profile, true );
    if( Objects.isNotNull( ax ) )
      operation.addChange( new PointPropertyRemove( profile, ax ) );
    if( Objects.isNotNull( ay ) )
      operation.addChange( new PointPropertyRemove( profile, ay ) );
    if( Objects.isNotNull( dx ) )
      operation.addChange( new PointPropertyRemove( profile, dx ) );

    new ProfilOperationJob( operation ).schedule();
  }
}
