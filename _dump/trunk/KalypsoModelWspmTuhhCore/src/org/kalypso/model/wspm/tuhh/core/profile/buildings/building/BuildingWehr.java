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
package org.kalypso.model.wspm.tuhh.core.profile.buildings.building;

import java.util.ArrayList;

import org.kalypso.commons.metadata.MetadataObject;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.AbstractObservationBuilding;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.Observation;
import org.kalypso.observation.phenomenon.DictionaryPhenomenon;
import org.kalypso.observation.result.Component;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

import sun.reflect.generics.reflectiveObjects.NotImplementedException;

/**
 * @author kimwerner
 */
public class BuildingWehr extends AbstractObservationBuilding
{
  public static final String ID = IWspmTuhhConstants.BUILDING_TYP_WEHR;

  public BuildingWehr( final IProfil profil )
  {
    final TupleResult result = new TupleResult();
    result.addComponent( createComponent( IWspmTuhhConstants.BUILDING_PROPERTY_WEHRART ) );
    result.addComponent( createComponent( IWspmTuhhConstants.BUILDING_PROPERTY_FORMBEIWERT ) );
    final IRecord emptyRecord = result.createRecord();
    result.add( emptyRecord );

    final Observation<TupleResult> observation = new Observation<TupleResult>( ID, ID, result, new ArrayList<MetadataObject>() );

    init( profil, observation );

  }

  public BuildingWehr( final IProfil profil, final IObservation<TupleResult> observation )
  {
    init( profil, observation );
  }

  private IComponent createComponent( final String type )
  {
    /* building observation properties */
    if( IWspmTuhhConstants.BUILDING_PROPERTY_WEHRART.equals( type ) )
      return new Component( IWspmTuhhConstants.BUILDING_PROPERTY_WEHRART, "Wehrart", "Wehrart", "", "", IWspmTuhhConstants.Q_STRING, IWspmTuhhConstants.WEHR_TYP_BEIWERT, new DictionaryPhenomenon( IWspmTuhhConstants.BUILDING_PROPERTY_WEHRART, "Wehrart", "Wehrart" ) );

    else if( IWspmTuhhConstants.BUILDING_PROPERTY_FORMBEIWERT.equals( type ) )
      return new Component( IWspmTuhhConstants.BUILDING_PROPERTY_FORMBEIWERT, "Formbeiwert", "Formbeiwert", "", "", IWspmConstants.Q_DOUBLE, 0.0, new DictionaryPhenomenon( IWspmTuhhConstants.BUILDING_PROPERTY_FORMBEIWERT, "Formbeiwert", "Formbeiwert" ) );

    /* profile observation properties */
    else if( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR.equals( type ) )
      return new Component( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR, "Oberkante Wehr", "Oberkante Wehr", "", "", IWspmConstants.Q_DOUBLE, 0.0, new DictionaryPhenomenon( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR, "Oberkante Wehr", "Oberkante Wehr" ) );

    throw new NotImplementedException();

  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.buildings.AbstractObservationBuilding#getPointProperty(java.lang.String)
   */
  @Override
  protected IComponent getPointProperty( final String id )
  {
    return createComponent( id );
  }

// public IProfilChange getWehrartProfileChange( final WEHRART type )
// {
//
// switch( type )
// {
// case eBeiwert:
// return new ProfileObjectEdit( this, ProfilObsHelper.getPropertyFromId( this,
// IWspmTuhhConstants.BUILDING_PROPERTY_WEHRART ), WEHRART.eBeiwert.name() );
// case eBreitkronig:
// return new ProfileObjectEdit( this, ProfilObsHelper.getPropertyFromId( this,
// IWspmTuhhConstants.BUILDING_PROPERTY_WEHRART ), WEHRART.eBreitkronig.name() );
// case eRundkronig:
// return new ProfileObjectEdit( this, ProfilObsHelper.getPropertyFromId( this,
// IWspmTuhhConstants.BUILDING_PROPERTY_WEHRART ), WEHRART.eRundkronig.name() );
// case eScharfkantig:
// return new ProfileObjectEdit( this, ProfilObsHelper.getPropertyFromId( this,
// IWspmTuhhConstants.BUILDING_PROPERTY_WEHRART ), WEHRART.eScharfkantig.name() );
//
// default:
// throw new NotImplementedException();
// }
// }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.buildings.AbstractObservationBuilding#getProfileProperties()
   */
  @Override
  protected String[] getProfileProperties( )
  {
    return new String[] { IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR };
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfileObject#getId()
   */
  public String getId( )
  {
    return ID;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.buildings.AbstractObservationBuilding#init(org.kalypso.model.wspm.core.profil.IProfil,
   *      org.kalypso.observation.IObservation)
   */
  @Override
  protected void init( IProfil profil, IObservation<TupleResult> observation )
  {
     super.init( profil, observation );
     final IComponent cmpHoehe = profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_HOEHE );
     final IComponent cmpWehr = profil.hasPointProperty( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR );
     if (cmpHoehe==null || cmpWehr==null)return;
     for(final IRecord record :profil.getResult())
     {
       record.setValue( cmpWehr, record.getValue( cmpHoehe ) );
     }
  }
}
