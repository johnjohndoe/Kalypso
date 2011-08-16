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

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.ProfileFeatureFactory;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.sobek.SobekModel;
import org.kalypso.model.wspm.core.profil.sobek.profiles.ISobekProfileDefData;
import org.kalypso.model.wspm.core.profil.sobek.profiles.SobekProfile;
import org.kalypso.model.wspm.core.profil.sobek.profiles.SobekProfileDat;
import org.kalypso.model.wspm.core.profil.sobek.profiles.SobekProfileDef;
import org.kalypso.model.wspm.core.profil.sobek.profiles.SobekProfileDefYZTable;
import org.kalypso.model.wspm.core.profil.sobek.profiles.SobekYZPoint;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;


/**
 * @author Gernot Belger
 */
public class Sobek2Wspm
{
  private final Collection<Feature> m_newFeatures = new ArrayList<Feature>();

  private final IStatusCollector m_stati = new StatusCollector( KalypsoModelWspmTuhhUIPlugin.getID() );

  private final WspmWaterBody m_water;


  public Sobek2Wspm( final WspmWaterBody water )
  {
    m_water = water;
  }

  public void convert( final SobekModel model )
  {
    final SobekProfile[] profiles = model.getProfiles();
    for( final SobekProfile sobekProfile : profiles )
      convert( sobekProfile );
  }

  public void convert( final SobekProfile sobekProfile )
  {
    final SobekProfileDef profileDef = sobekProfile.getProfileDef();
    final SobekProfileDat profileDat = sobekProfile.getProfileDat();

    if( profileDef == null && profileDat == null )
      return;

    if( profileDef == null )
    {
      final String id = profileDat.getId();
      m_stati.add( IStatus.ERROR, "Missing definition for cross section with id '%s'", null, id );
      return;
    }

    final IProfileFeature newProfile = m_water.createNewProfile();
    newProfile.setName( profileDef.getId() );
    newProfile.setDescription( profileDef.getNm() );
    newProfile.setProfileType( IWspmTuhhConstants.PROFIL_TYPE_PASCHE );
    // FIXME: ask from user
    newProfile.setSrsName( KalypsoDeegreePlugin.getDefault().getCoordinateSystem() );

    final ISobekProfileDefData data = profileDef.getData();
    final IProfil profil = newProfile.getProfil();

    try
    {
      convertData( profil, data );
      ProfileFeatureFactory.toFeature( profil, newProfile );
      m_newFeatures.add( newProfile );
    }
    catch( final CoreException e )
    {
      m_water.getProfiles().remove( newProfile );
      m_stati.add( e.getStatus() );
      return;
    }
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
        final String message = String.format( "Cross section '%s' has unknown type: %d", profil.getName(), type );
        final IStatus status = new Status( IStatus.WARNING, KalypsoModelWspmTuhhUIPlugin.getID(), message );
        throw new CoreException( status );
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

  public IStatus getStatus( )
  {
    return m_stati.asMultiStatusOrOK( "Problems during SOBEK conversion", "Conversion sucessfully terminated" );
  }

  public Feature[] getNewFeatures( )
  {
    return m_newFeatures.toArray( new Feature[m_newFeatures.size()] );
  }
}
