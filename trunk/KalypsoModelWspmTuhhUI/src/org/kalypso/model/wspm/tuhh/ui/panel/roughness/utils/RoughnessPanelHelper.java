/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.wspm.tuhh.ui.panel.roughness.utils;

import java.util.LinkedHashSet;
import java.util.Set;

import org.kalypso.commons.java.lang.Objects;
import org.kalypso.model.wspm.core.IWspmPointProperties;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyAdd;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyRemove;
import org.kalypso.model.wspm.core.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.core.profil.operation.ProfilOperationJob;
import org.kalypso.observation.result.IComponent;

/**
 * @author Dirk Kuch
 */
public final class RoughnessPanelHelper
{
  private RoughnessPanelHelper( )
  {
  }

  public static IComponent[] fromProfile( final IProfil profile )
  {
    final Set<IComponent> found = new LinkedHashSet<IComponent>();

    final IComponent[] properties = profile.getPointProperties();
    for( final IComponent property : properties )
    {
      if( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KS.equals( property.getId() ) )
        found.add( property );
      else if( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KST.equals( property.getId() ) )
        found.add( property );
      else if( IWspmPointProperties.POINT_PROPERTY_ROUGHNESS_CLASS.equals( property.getId() ) )
        found.add( property );
      else if( IWspmPointProperties.POINT_PROPERTY_ROUGHNESS_FACTOR.equals( property.getId() ) )
        found.add( property );
    }

    return found.toArray( new IComponent[] {} );
  }

  public static String[] findMissing( final IProfil profile )
  {
    final Set<String> missing = new LinkedHashSet<String>();
    if( Objects.isNull( profile.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KS ) ) )
      missing.add( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KS );

    if( Objects.isNull( profile.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KST ) ) )
      missing.add( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KST );

    if( Objects.isNull( profile.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_ROUGHNESS_CLASS ) ) )
      missing.add( IWspmPointProperties.POINT_PROPERTY_ROUGHNESS_CLASS );

    if( Objects.isNull( profile.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_ROUGHNESS_FACTOR ) ) )
      missing.add( IWspmPointProperties.POINT_PROPERTY_ROUGHNESS_FACTOR );

    return missing.toArray( new String[] {} );
  }

  public static void addRoughness( final IProfil profile, final String componentId )
  {
    final IProfilPointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( profile.getType() );
    final IComponent component = provider.getPointProperty( componentId );

    final ProfilOperation operation = new ProfilOperation( "Adding roughness type", profile, true );
    operation.addChange( new PointPropertyAdd( profile, component ) );
    new ProfilOperationJob( operation ).schedule();
  }

  public static void removeRoughness( final IProfil profile, final String componentId )
  {
    final IProfilPointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( profile.getType() );
    final IComponent component = provider.getPointProperty( componentId );

    final ProfilOperation operation = new ProfilOperation( "Adding roughness type", profile, true );
    operation.addChange( new PointPropertyRemove( profile, component ) );
    new ProfilOperationJob( operation ).schedule();
  }
}
