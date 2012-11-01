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
package org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building;

import org.kalypso.model.wspm.core.gml.ProfileObjectBinding;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.IProfileObjectProvider;
import org.kalypso.model.wspm.core.profil.ProfileObjectHelper;
import org.kalypso.model.wspm.core.profil.util.ProfileUtil;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Dirk Kuch
 * @author Holger Albert
 */
public class BuildingEiProvider implements IProfileObjectProvider
{
  @Override
  public BuildingEi createProfileObject( final IProfile profile )
  {
    return new BuildingEi();
  }

  @Override
  public IProfileObject buildProfileObject( final IProfile profile, final Feature profileObjectFeature )
  {
    /* Create the profile object. */
    final BuildingEi profileObject = createProfileObject( profile );

    if( !(profileObjectFeature instanceof ProfileObjectBinding) )
    {
      /* REMARK: Handle feature as observation (old style). */
      final IObservation<TupleResult> profileObjectObservation = ObservationFeatureFactory.toObservation( profileObjectFeature );

      /* Fill the records and the metadata. */
      fillMetadata( profileObjectObservation, profileObject );

      return profileObject;
    }

    /* REMARK: Handle feature as profile object binding (new style). */
    final ProfileObjectBinding profileObjectBinding = (ProfileObjectBinding)profileObjectFeature;

    /* Fill the records and the metadata. */
    ProfileObjectHelper.fillRecords( profileObjectBinding, profileObject );
    ProfileObjectHelper.fillMetadata( profileObjectBinding, profileObject );

    return profileObject;
  }

  private void fillMetadata( final IObservation<TupleResult> profileObjectObservation, final BuildingEi profileObject )
  {
    final TupleResult result = profileObjectObservation.getResult();
    if( result.size() != 1 )
      throw new IllegalStateException( "Only one record is allowed in profile object observations..." ); //$NON-NLS-1$

    final IComponent bezugspunktXComponent = ProfileUtil.getFeatureComponent( BuildingsCompatibilityConstants.BUILDING_PROPERTY_BEZUGSPUNKT_X );
    final IComponent bezugspunktYComponent = ProfileUtil.getFeatureComponent( BuildingsCompatibilityConstants.BUILDING_PROPERTY_BEZUGSPUNKT_Y );
    final IComponent hoeheComponent = ProfileUtil.getFeatureComponent( BuildingsCompatibilityConstants.BUILDING_PROPERTY_HOEHE );
    final IComponent breiteComponent = ProfileUtil.getFeatureComponent( BuildingsCompatibilityConstants.BUILDING_PROPERTY_BREITE );
    final IComponent sohlgefaelleComponent = ProfileUtil.getFeatureComponent( BuildingsCompatibilityConstants.BUILDING_PROPERTY_SOHLGEFAELLE );
    final IComponent rauheitComponent = ProfileUtil.getFeatureComponent( BuildingsCompatibilityConstants.BUILDING_PROPERTY_RAUHEIT );

    final int bezugspunktXIndex = result.indexOfComponent( bezugspunktXComponent );
    final int bezugspunktYIndex = result.indexOfComponent( bezugspunktYComponent );
    final int hoeheIndex = result.indexOfComponent( hoeheComponent );
    final int breiteIndex = result.indexOfComponent( breiteComponent );
    final int sohlgefaelleIndex = result.indexOfComponent( sohlgefaelleComponent );
    final int rauheitIndex = result.indexOfComponent( rauheitComponent );

    final IRecord record = result.get( 0 );

    final Double bezugspunktX = (Double)record.getValue( bezugspunktXIndex );
    final Double bezugspunktY = (Double)record.getValue( bezugspunktYIndex );
    final Double hoehe = (Double)record.getValue( hoeheIndex );
    final Double breite = (Double)record.getValue( breiteIndex );
    final Double sohlgefaelle = (Double)record.getValue( sohlgefaelleIndex );
    final Double rauheit = (Double)record.getValue( rauheitIndex );

    profileObject.setBezugspunktX( bezugspunktX );
    profileObject.setBezugspunktY( bezugspunktY );
    profileObject.setHoehe( hoehe );
    profileObject.setBreite( breite );
    profileObject.setSohlgefaelle( sohlgefaelle );
    profileObject.setRauheit( rauheit );
  }
}