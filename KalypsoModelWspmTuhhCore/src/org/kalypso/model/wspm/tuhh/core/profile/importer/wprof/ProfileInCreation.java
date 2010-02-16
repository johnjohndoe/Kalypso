/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.wspm.tuhh.core.profile.importer.wprof;

import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.ProfileFeatureFactory;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.building.BuildingBruecke;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

/**
 * @author Gernot Belger
 */
public class ProfileInCreation implements IWspmTuhhConstants
{
  private final IProfileFeature m_profileFeature;

  private IProfil m_profile;

  public ProfileInCreation( final IProfileFeature profileFeature )
  {
    m_profileFeature = profileFeature;
  }

  public void cleanupProfile( )
  {
    final IProfil profil = getProfil();

    cleanupHeights( profil );

    cleanupBridges( profil );

    createDurchstroemteBereiche( profil );
  }

  private void cleanupHeights( final IProfil profil )
  {
    final int heightComponent = profil.indexOfProperty( POINT_PROPERTY_HOEHE );
    ProfilUtil.interpolateProperty( profil, heightComponent );
  }

  private void cleanupBridges( final IProfil profil )
  {
    final boolean hasUK = profil.hasPointProperty( POINT_PROPERTY_UNTERKANTEBRUECKE ) != null;
    final boolean hasOK = profil.hasPointProperty( POINT_PROPERTY_OBERKANTEBRUECKE ) != null;

    if( !hasUK && !hasOK )
      return;

    final IProfilPointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( profil.getType() );

    final IComponent heightComponent = provider.getPointProperty( POINT_PROPERTY_HOEHE );
    final IComponent ukComponent = provider.getPointProperty( POINT_PROPERTY_UNTERKANTEBRUECKE );
    final IComponent okComponent = provider.getPointProperty( POINT_PROPERTY_OBERKANTEBRUECKE );
    if( hasUK )
    {
      final int ukIndex = profil.indexOfProperty( POINT_PROPERTY_UNTERKANTEBRUECKE );
      ProfilUtil.interpolateProperty( profil, ukIndex );
    }
    else
      profil.addPointProperty( ukComponent, heightComponent );

    if( hasOK )
    {
      final int okIndex = profil.indexOfProperty( POINT_PROPERTY_OBERKANTEBRUECKE );
      ProfilUtil.interpolateProperty( profil, okIndex );
    }
    else
      profil.addPointProperty( okComponent, ukComponent );

    // FIXME: remove bv-höhe?
    // FIXME: auto-adjust trennflächen?

    profil.addProfileObjects( new IProfileObject[] { new BuildingBruecke( profil ) } );
  }

  private void createDurchstroemteBereiche( final IProfil profil )
  {
    final IRecord[] points = profil.getPoints();

    final IProfilPointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( profil.getType() );

    if( points.length > 1 )
    {
      final Object defaultValue = provider.getDefaultValue( MARKER_TYP_DURCHSTROEMTE );
      profil.createPointMarker( MARKER_TYP_DURCHSTROEMTE, points[0] ).setValue( defaultValue );
      profil.createPointMarker( MARKER_TYP_DURCHSTROEMTE, points[points.length - 1] ).setValue( defaultValue );
    }
  }

  public void finish( )
  {
    ProfileFeatureFactory.toFeature( getProfil(), m_profileFeature );
  }

  public IProfil getProfil( )
  {
    if( m_profile == null )
      m_profile = m_profileFeature.getProfil();

    return m_profile;
  }

  public IProfileFeature getFeature( )
  {
    return m_profileFeature;
  }
}
