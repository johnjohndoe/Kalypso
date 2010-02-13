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
package org.kalypso.model.wspm.tuhh.core.profile.importer.wprof;

import java.util.ArrayList;
import java.util.List;

import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.ProfileFeatureFactory;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.building.BuildingBruecke;
import org.kalypso.observation.result.IRecord;

/**
 * @author belger
 *
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
    // final int heightIndex = profil.indexOfProperty( POINT_PROPERTY_HOEHE );

    final List<Integer> indicesToCheck = new ArrayList<Integer>();
    indicesToCheck.add( profil.indexOfProperty( POINT_PROPERTY_HOEHE ) );
    // indicesToCheck.add( profil.indexOfProperty( POINT_PROPERTY_UNTERKANTEBRUECKE ) );
    // indicesToCheck.add( profil.indexOfProperty( POINT_PROPERTY_OBERKANTEBRUECKE ) );

    if( profil.hasPointProperty( POINT_PROPERTY_OBERKANTEBRUECKE ) != null || profil.hasPointProperty( POINT_PROPERTY_UNTERKANTEBRUECKE ) != null )
      profil.addProfileObjects( new IProfileObject[] { new BuildingBruecke( profil ) } );

    final IRecord[] points = profil.getPoints();

    final IProfilPointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( profil.getType() );

    // Set missing UK and OK points to normal height
    for( final int compIndex : indicesToCheck )
    {
      if( compIndex != -1 )
      {
        // /* Set head to height */
        // for( final IRecord record : points )
        // {
        // final Double height = (Double) record.getValue( heightIndex );
        // if( record.getValue( compIndex ) == null )
        // record.setValue( compIndex, height );
        // else
        // break;
        // }
        //
        // /* Set tail to height */
        // for( int i = points.length; i > 0; i-- )
        // {
        // final IRecord record = points[i - 1];
        // final Double height = (Double) record.getValue( heightIndex );
        // if( record.getValue( compIndex ) == null )
        // record.setValue( compIndex, height );
        // else
        // break;
        // }
        // ProfilUtil.interpolateProperty( profil, compIndex );
      }
    }

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
