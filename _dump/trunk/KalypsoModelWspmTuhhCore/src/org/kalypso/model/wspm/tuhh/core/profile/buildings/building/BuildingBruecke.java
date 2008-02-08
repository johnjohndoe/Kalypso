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
package org.kalypso.model.wspm.tuhh.core.profile.buildings.building;

import java.util.ArrayList;

import org.kalypso.commons.metadata.MetadataObject;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider;
import org.kalypso.model.wspm.core.profil.util.ProfilObsHelper;
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
final public class BuildingBruecke extends AbstractObservationBuilding
{
  public static final String ID = IWspmTuhhConstants.BUILDING_TYP_BRUECKE;

  public BuildingBruecke( final IProfil profil )
  {
    m_profil = profil;

    final TupleResult result = new TupleResult();
    result.addComponent( new Component( IWspmTuhhConstants.BUILDING_PROPERTY_BREITE, "Breite", "Breite", "", "", IWspmConstants.Q_DOUBLE, Double.NaN, new DictionaryPhenomenon( IWspmTuhhConstants.BUILDING_PROPERTY_BREITE, "Breite", "Breite" ) ) );
    result.addComponent( new Component( IWspmTuhhConstants.BUILDING_PROPERTY_UNTERWASSER, "Unterwasser", "Unterwasser", "", "", IWspmConstants.Q_DOUBLE, Double.NaN, new DictionaryPhenomenon( IWspmTuhhConstants.BUILDING_PROPERTY_UNTERWASSER, "Unterwasser", "Unterwasser" ) ) );
    result.addComponent( new Component( IWspmTuhhConstants.BUILDING_PROPERTY_FORMBEIWERT, "Pfeilerformbeiwert", "Pfeilerformbeiwert", "", "", IWspmConstants.Q_DOUBLE, Double.NaN, new DictionaryPhenomenon( IWspmTuhhConstants.BUILDING_PROPERTY_FORMBEIWERT, "Pfeilerformbeiwert", "Pfeilerformbeiwert" ) ) );
    result.addComponent( new Component( IWspmTuhhConstants.BUILDING_PROPERTY_RAUHEIT, "Rauheit", "Rauheit", "", "", IWspmConstants.Q_DOUBLE, Double.NaN, new DictionaryPhenomenon( IWspmTuhhConstants.BUILDING_PROPERTY_RAUHEIT, "Rauheit", "Rauheit" ) ) );
    final IRecord emptyRecord = result.createRecord();
    result.add( emptyRecord );
    final Observation<TupleResult> observation = new Observation<TupleResult>( ID, ID, result, new ArrayList<MetadataObject>() );
    
    m_observation = observation;
    profil.getResult().addComponent( getPointProperty( IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE ), getPointProperty( IWspmTuhhConstants.POINT_PROPERTY_HOEHE ) );
    profil.getResult().addComponent( getPointProperty( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE ), getPointProperty( IWspmTuhhConstants.POINT_PROPERTY_HOEHE ) );

  }

  public BuildingBruecke( final IProfil profil, final IObservation<TupleResult> observation )
  {
    m_profil = profil;
    m_observation = observation;
    profil.getResult().addComponent( getPointProperty( IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE ), getPointProperty( IWspmTuhhConstants.POINT_PROPERTY_HOEHE ) );
    profil.getResult().addComponent( getPointProperty( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE ), getPointProperty( IWspmTuhhConstants.POINT_PROPERTY_HOEHE ) );
  }

  @Override
  protected String[] getProfileProperties( )
  {
    return new String[] { IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE };
  }

// private IComponent createComponent( final String type )
// {
// /* building observation properties */
// final IProfilPointPropertyProvider[] providers = KalypsoModelWspmCoreExtensions.getPointPropertyProviders(
// IWspmTuhhConstants.PROFIL_TYPE_PASCHE );
// for (final IProfilPointPropertyProvider provider : providers)
// {
// if (provider.providesPointProperty( type ))
// return provider.getPointProperty( type );
// }
// 
//  
// throw new NotImplementedException();
//
// }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfileObject#getId()
   */
  public String getId( )
  {
    return ID;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.buildings.AbstractObservationBuilding#getPointProperty(java.lang.String)
   */
  @Override
  protected IComponent getPointProperty( String id )
  {
    final IProfilPointPropertyProvider[] providers = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( IWspmTuhhConstants.PROFIL_TYPE_PASCHE );
    for( final IProfilPointPropertyProvider provider : providers )
    {
      if( provider.providesPointProperty( id ) )
        return provider.getPointProperty( id );
    }
    return null;
  }

}
