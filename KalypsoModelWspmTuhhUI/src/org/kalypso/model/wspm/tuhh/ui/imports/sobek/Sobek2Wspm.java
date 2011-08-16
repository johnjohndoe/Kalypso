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
package org.kalypso.model.wspm.tuhh.ui.imports.sobek;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.ProfileFeatureFactory;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.sobek.SobekModel;
import org.kalypso.model.wspm.core.profil.sobek.profiles.ISobekProfileDefData;
import org.kalypso.model.wspm.core.profil.sobek.profiles.SobekProfile;
import org.kalypso.model.wspm.core.profil.sobek.profiles.SobekProfileDef;
import org.kalypso.model.wspm.core.profil.sobek.profiles.SobekProfileDefYZTable;
import org.kalypso.model.wspm.core.profil.sobek.profiles.SobekYZPoint;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypsodeegree.KalypsoDeegreePlugin;


/**
 * @author Gernot Belger
 *
 */
public class Sobek2Wspm
{
  private final WspmWaterBody m_water;

  public Sobek2Wspm( final WspmWaterBody water )
  {
    m_water = water;
  }

  public void convert( final SobekModel model ) throws CoreException
  {
    // FIXME: collect stati instead?
    final SobekProfile[] profiles = model.getProfiles();
    for( final SobekProfile sobekProfile : profiles )
      convert( sobekProfile );
  }

  public void convert( final SobekProfile sobekProfile ) throws CoreException
  {
    final SobekProfileDef profileDef = sobekProfile.getProfileDef();
    // FIXME: error?
    if( profileDef == null )
      return;

    final IProfileFeature newProfile = m_water.createNewProfile();
    newProfile.setName( profileDef.getId() );
    newProfile.setDescription( profileDef.getNm() );
    newProfile.setProfileType( IWspmTuhhConstants.PROFIL_TYPE_PASCHE );
    // FIXME: ask from user
    newProfile.setSrsName( KalypsoDeegreePlugin.getDefault().getCoordinateSystem() );

    final ISobekProfileDefData data = profileDef.getData();
    final IProfil profil = newProfile.getProfil();
    convertData( profil, data );

    ProfileFeatureFactory.toFeature( profil, newProfile );
  }

  private void convertData( final IProfil profil, final ISobekProfileDefData data ) throws CoreException
  {
    final int type = data.getType();
    switch( type )
    {
      case 10:
        convertYZTable( profil, (SobekProfileDefYZTable) data );
        break;

      default:
        // FIXME: error handling or collect warning
        return;
// final String message = String.format( "Unknown SOBEK cross section type: %d", type );
// final IStatus status = new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getID(), message );
// throw new CoreException( status );
    }
  }

  private void convertYZTable( final IProfil profil, final SobekProfileDefYZTable data )
  {
    final TupleResult result = profil.getResult();
    result.addComponent( ProfilUtil.getFeatureComponent( IWspmConstants.POINT_PROPERTY_BREITE ) );
    result.addComponent( ProfilUtil.getFeatureComponent( IWspmConstants.POINT_PROPERTY_HOEHE ) );

    final int yIndex = result.indexOfComponent( IWspmConstants.POINT_PROPERTY_BREITE );
    final int zIndex = result.indexOfComponent( IWspmConstants.POINT_PROPERTY_HOEHE );

    final SobekYZPoint[] points = data.getPoints();
    for( final SobekYZPoint point : points )
    {
      final IRecord record = result.createRecord();

      record.setValue( yIndex, point.getY().doubleValue() );
      record.setValue( zIndex, point.getZ().doubleValue() );

      result.add( record );
    }
  }
}
