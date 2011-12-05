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
package org.kalypso.model.wspm.tuhh.core.profile.buildings.durchlass;

import org.kalypso.model.wspm.core.profil.AbstractProfileObject;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.IProfileBuilding;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.Observation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.TupleResult;

/**
 * @author kimwerner
 */
public class BuildingMaul extends AbstractProfileObject implements IProfileBuilding
{
  public static final String ID = IWspmTuhhConstants.BUILDING_TYP_MAUL;

  public BuildingMaul( )
  {
    this( buildObservation() );
  }

  private static IObservation<TupleResult> buildObservation( )
  {
    final TupleResult result = new TupleResult();
    result.addComponent( getObjectComponent( IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_X ) );
    result.addComponent( getObjectComponent( IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_Y ) );
    result.addComponent( getObjectComponent( IWspmTuhhConstants.BUILDING_PROPERTY_HOEHE ) );
    result.addComponent( getObjectComponent( IWspmTuhhConstants.BUILDING_PROPERTY_BREITE ) );
    result.addComponent( getObjectComponent( IWspmTuhhConstants.BUILDING_PROPERTY_SOHLGEFAELLE ) );
    result.addComponent( getObjectComponent( IWspmTuhhConstants.BUILDING_PROPERTY_RAUHEIT ) );

    final IComponent buildingComponent = getObjectComponent( ID );
    final Observation<TupleResult> observation = new Observation<TupleResult>( ID, buildingComponent==null?ID:buildingComponent.getName(), result ); //$NON-NLS-1$

    return observation;
  }

  public BuildingMaul( final IObservation<TupleResult> observation )
  {
    super( observation );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.buildings.AbstractObservationBuilding#getProfileProperties()
   */
  @Override
  protected String[] getProfileProperties( )
  {
    return new String[] {};
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfileObject#getId()
   */
  @Override
  public String getId( )
  {
    return ID;
  }
}
