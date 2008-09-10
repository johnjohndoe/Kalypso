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
package org.kalypso.model.wspm.tuhh.core.profile.buildings.durchlass;

import java.util.ArrayList;

import org.kalypso.commons.metadata.MetadataObject;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.AbstractObservationBuilding;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.Observation;
import org.kalypso.observation.result.TupleResult;

/**
 * @author kimwerner
 */
public class BuildingEi extends AbstractObservationBuilding
{


  public static final String ID = IWspmTuhhConstants.BUILDING_TYP_EI;

  public BuildingEi( final IProfil profil )
  {
    final TupleResult result = new TupleResult();
    result.addComponent( createObjectProperty( IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_X ) );
    result.addComponent( createObjectProperty( IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_Y ) );
    result.addComponent( createObjectProperty( IWspmTuhhConstants.BUILDING_PROPERTY_HOEHE ) );
    result.addComponent( createObjectProperty( IWspmTuhhConstants.BUILDING_PROPERTY_BREITE ) );
    result.addComponent( createObjectProperty( IWspmTuhhConstants.BUILDING_PROPERTY_SOHLGEFAELLE ) );
    result.addComponent( createObjectProperty( IWspmTuhhConstants.BUILDING_PROPERTY_RAUHEIT ) );

    final Observation<TupleResult> observation = new Observation<TupleResult>(  ID,"Ei", result, new ArrayList<MetadataObject>() );

    init( profil, observation );
  }

  public BuildingEi( final IProfil profil, final IObservation<TupleResult> observation )
  {
    init( profil, observation );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.buildings.AbstractObservationBuilding#getProfileProperties()
   */
  @Override
  protected String[] getProfileProperties( )
  {
    return new String[] {};
  }
//  private IComponent createComponent( final String type )
//  {
//    /* building observation properties */
//    if( IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_X.equals( type ) )
//      return new Component( IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_X, "Bezugspunkt X", "Bezugspunkt X", "", "", IWspmConstants.Q_DOUBLE, Double.NaN, new DictionaryPhenomenon( IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_X, "Bezugspunkt X", "Bezugspunkt X" ) );
//    else if( IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_Y.equals( type ) )
//      return new Component( IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_Y, "Bezugspunkt Y", "Bezugspunkt Y", "", "", IWspmConstants.Q_DOUBLE, Double.NaN, new DictionaryPhenomenon( IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_Y, "Bezugspunkt Y", "Bezugspunkt Y" ) );
//    if( IWspmTuhhConstants.BUILDING_PROPERTY_HOEHE.equals( type ) )
//      return new Component( IWspmTuhhConstants.BUILDING_PROPERTY_HOEHE, "Höhe", "Höhe", "", "", IWspmConstants.Q_DOUBLE, Double.NaN, new DictionaryPhenomenon( IWspmTuhhConstants.BUILDING_PROPERTY_HOEHE, "Höhe", "Höhe" ) );
//    else if( IWspmTuhhConstants.BUILDING_PROPERTY_BREITE.equals( type ) )
//      return new Component( IWspmTuhhConstants.BUILDING_PROPERTY_BREITE, "Breite", "Breite", "", "", IWspmConstants.Q_DOUBLE, Double.NaN, new DictionaryPhenomenon( IWspmTuhhConstants.BUILDING_PROPERTY_BREITE, "Breite", "Breite" ) );
//    if( IWspmTuhhConstants.BUILDING_PROPERTY_SOHLGEFAELLE.equals( type ) )
//      return new Component( IWspmTuhhConstants.BUILDING_PROPERTY_SOHLGEFAELLE, "Sohlgefälle", "Sohlgefälle", "", "", IWspmConstants.Q_DOUBLE, Double.NaN, new DictionaryPhenomenon( IWspmTuhhConstants.BUILDING_PROPERTY_SOHLGEFAELLE, "Sohlgefälle", "Sohlgefälle" ) );
//    else if( IWspmTuhhConstants.BUILDING_PROPERTY_RAUHEIT.equals( type ) )
//      return new Component( IWspmTuhhConstants.BUILDING_PROPERTY_RAUHEIT, "Rauheit", "Rauheit", "", "", IWspmConstants.Q_DOUBLE, Double.NaN, new DictionaryPhenomenon( IWspmTuhhConstants.BUILDING_PROPERTY_RAUHEIT, "Rauheit", "Rauheit" ) );
//
//    throw new NotImplementedException();
//
//  }
  /**
   * @see org.kalypso.model.wspm.core.profil.IProfileObject#getId()
   */
  public String getId( )
  {
    return ID;
  }
}
