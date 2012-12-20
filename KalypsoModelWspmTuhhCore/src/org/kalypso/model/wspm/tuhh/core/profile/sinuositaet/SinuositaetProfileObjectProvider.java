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
package org.kalypso.model.wspm.tuhh.core.profile.sinuositaet;

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
public class SinuositaetProfileObjectProvider implements IProfileObjectProvider
{
  private static String PROPERTY_KENNUNG = "urn:ogc:gml:dict:kalypso:model:wspm:tuhh:core:sinuositaetComponents#KENNUNG"; //$NON-NLS-1$

  private static String PROPERTY_SN = "urn:ogc:gml:dict:kalypso:model:wspm:tuhh:core:sinuositaetComponents#SN"; //$NON-NLS-1$

  private static String PROPERTY_GERINNE_ART = "urn:ogc:gml:dict:kalypso:model:wspm:tuhh:core:sinuositaetComponents#GERINNE_ART"; //$NON-NLS-1$

  private static String PROPERTY_LF = "urn:ogc:gml:dict:kalypso:model:wspm:tuhh:core:sinuositaetComponents#LF"; //$NON-NLS-1$

  @Override
  public SinuositaetProfileObject createProfileObject( final IProfile profile )
  {
    return new SinuositaetProfileObject();
  }

  @Override
  public IProfileObject buildProfileObject( final IProfile profile, final Feature profileObjectFeature )
  {
    /* Create the profile object. */
    final SinuositaetProfileObject profileObject = createProfileObject( profile );

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

  private void fillMetadata( final IObservation<TupleResult> profileObjectObservation, final SinuositaetProfileObject profileObject )
  {
    final TupleResult result = profileObjectObservation.getResult();
    if( result.size() != 1 )
      throw new IllegalStateException( "Only one record is allowed in profile object observations..." ); //$NON-NLS-1$

    final IComponent kennungComponent = ProfileUtil.getFeatureComponent( PROPERTY_KENNUNG );
    final IComponent snComponent = ProfileUtil.getFeatureComponent( PROPERTY_SN );
    final IComponent gerinneArtComponent = ProfileUtil.getFeatureComponent( PROPERTY_GERINNE_ART );
    final IComponent lfComponent = ProfileUtil.getFeatureComponent( PROPERTY_LF );

    final int kennungIndex = result.indexOfComponent( kennungComponent );
    final int snIndex = result.indexOfComponent( snComponent );
    final int gerinneArtIndex = result.indexOfComponent( gerinneArtComponent );
    final int lfIndex = result.indexOfComponent( lfComponent );

    final IRecord record = result.get( 0 );

    final String kennung = (String)record.getValue( kennungIndex );
    final Double sn = (Double)record.getValue( snIndex );
    final String gerinneArt = (String)record.getValue( gerinneArtIndex );
    final Double lf = (Double)record.getValue( lfIndex );

    profileObject.setKennung( kennung );
    profileObject.setSn( sn );
    profileObject.setGerinneArt( gerinneArt );
    profileObject.setLf( lf );
  }
}