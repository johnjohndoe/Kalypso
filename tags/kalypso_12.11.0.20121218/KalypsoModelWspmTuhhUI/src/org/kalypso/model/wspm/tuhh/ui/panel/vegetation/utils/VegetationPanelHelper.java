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
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfilePointPropertyProvider;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyAdd;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyRemove;
import org.kalypso.model.wspm.core.profil.operation.ProfileOperation;
import org.kalypso.model.wspm.core.profil.operation.ProfileOperationJob;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.observation.result.IComponent;

/**
 * @author Dirk Kuch
 */
public final class VegetationPanelHelper
{
  private VegetationPanelHelper( )
  {
  }

  public static IComponent[] fromProfile( final IProfile profile )
  {
    final Set<IComponent> found = new LinkedHashSet<>();

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

  public static void removeVegetationTypes( final IProfile profile )
  {
    final IComponent ax = profile.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_AX );
    final IComponent ay = profile.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_AY );
    final IComponent dx = profile.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_DP );

    final ProfileOperation operation = new ProfileOperation( Messages.getString("VegetationPanelHelper.0"), profile, true ); //$NON-NLS-1$
    if( Objects.isNotNull( ax ) )
      operation.addChange( new PointPropertyRemove( profile, ax ) );
    if( Objects.isNotNull( ay ) )
      operation.addChange( new PointPropertyRemove( profile, ay ) );
    if( Objects.isNotNull( dx ) )
      operation.addChange( new PointPropertyRemove( profile, dx ) );

    new ProfileOperationJob( operation ).schedule();
  }

  public static void addVegetationTypes( final IProfile profile )
  {
    final ProfileOperation operation = new ProfileOperation( Messages.getString("VegetationPanelHelper.1"), profile, true ); //$NON-NLS-1$

    final IProfilePointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( profile.getType() );
    final IComponent ax = provider.getPointProperty( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_AX );
    final IComponent ay = provider.getPointProperty( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_AY );
    final IComponent dp = provider.getPointProperty( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_DP );

    operation.addChange( new PointPropertyAdd( profile, ax ) );
    operation.addChange( new PointPropertyAdd( profile, ay ) );
    operation.addChange( new PointPropertyAdd( profile, dp ) );

    new ProfileOperationJob( operation ).schedule();
  }

  public static void addVegetationClass( final IProfile profile )
  {
    final ProfileOperation operation = new ProfileOperation( Messages.getString("VegetationPanelHelper.2"), profile, true ); //$NON-NLS-1$

    final IProfilePointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( profile.getType() );
    final IComponent clazz = provider.getPointProperty( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_CLASS );

    operation.addChange( new PointPropertyAdd( profile, clazz ) );

    new ProfileOperationJob( operation ).schedule();

  }
}
