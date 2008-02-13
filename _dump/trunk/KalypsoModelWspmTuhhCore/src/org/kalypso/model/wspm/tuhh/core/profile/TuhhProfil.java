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
package org.kalypso.model.wspm.tuhh.core.profile;

import java.util.ArrayList;
import java.util.List;

import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.impl.AbstractProfil;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.TuhhBuildingHelper;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.phenomenon.DictionaryPhenomenon;
import org.kalypso.observation.result.Component;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

/**
 * @author kuch
 * @author kimwerner
 */
public class TuhhProfil extends AbstractProfil
{

  public static final String PROFIL_TYPE = "org.kalypso.model.wspm.tuhh.profiletype";

  public TuhhProfil( )
  {
    super( PROFIL_TYPE );

    final IComponent hoehe = new Component( IWspmConstants.POINT_PROPERTY_HOEHE, "Höhe", "Höhe", "", "", IWspmConstants.Q_DOUBLE, Double.NaN, new DictionaryPhenomenon( IWspmConstants.POINT_PROPERTY_HOEHE, "Höhe", "Höhe" ) );
    final IComponent breite = new Component( IWspmConstants.POINT_PROPERTY_BREITE, "Breite", "Breite", "", "", IWspmConstants.Q_DOUBLE, Double.NaN, new DictionaryPhenomenon( IWspmConstants.POINT_PROPERTY_BREITE, "Breite", "Breite" ) );

    final TupleResult result = new TupleResult( new IComponent[] { hoehe, breite } );

    setResult( result );
  }

  public TuhhProfil( final TupleResult result )
  {
    super( PROFIL_TYPE );

    setResult( result );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#setProfileObject(org.kalypso.model.wspm.core.profil.IProfileObject[])
   * @note for tuhh-profiles only ONE ProfileObject is allowed at same time, all other objects will be removed
   * @see #removeProfileObject(IProfileObject)
   * @throws IllegalStateException
   */
  @Override
  public IProfileObject[] addProfileObjects( final IProfileObject[] profileObjects )
  {
    if( profileObjects == null || profileObjects.length > 1 )
      throw new IllegalStateException( "only one profileObject allowed" );
    setProperty( PROFILE_OBJECTS, null );
    if (profileObjects.length > 0)
    return super.addProfileObjects( profileObjects );
    return profileObjects;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#createProfileObjects(org.kalypso.observation.IObservation<org.kalypso.observation.result.TupleResult>[])
   */
  @Override
  public void createProfileObjects( final IObservation<TupleResult>[] profileObjects )
  {
    for( final IObservation<TupleResult> observation : profileObjects )
    {
      final IProfileObject profileObject = TuhhBuildingHelper.createProfileObject( this, observation );
      addProfileObjects( new IProfileObject[] { profileObject } );
    }
  }

  /**
   * @return false if the point is captured by a marker and will NOT remove the point from pointList
   */
  @Override
  public boolean removePoint( final IRecord point )
  {
    final IProfilPointMarker[] markers = getPointMarkerFor( point );
    if( markers.length == 0 )
      return super.removePoint( point );
    return false;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.impl.AbstractProfil#getPointMarkerFor(org.kalypso.observation.result.IRecord)
   */
  @Override
  public IProfilPointMarker[] getPointMarkerFor( final IRecord record )
  {
    final ArrayList<IProfilPointMarker> pointMarkers = new ArrayList<IProfilPointMarker>();
    final IComponent[] markers = getPointMarkerTypes();
    for( final IComponent marker : markers )
    {
      if( record.getValue( marker ) != null )
        pointMarkers.add( new ProfilDevider( marker, record ) );
    }
    return pointMarkers.toArray( new IProfilPointMarker[] {} );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.impl.AbstractProfil#getPointMarkerFor(org.kalypso.observation.result.IComponent)
   */
  @Override
  public IProfilPointMarker[] getPointMarkerFor( final IComponent markerColumn )
  {
    if( markerColumn == null )
      return new IProfilPointMarker[] {};

    final List<IProfilPointMarker> markers = new ArrayList<IProfilPointMarker>();

    final TupleResult result = getResult();
    for( final IRecord record : result )
    {
      final Object value = record.getValue( markerColumn );
      if( value != null )
      {
        markers.add( new ProfilDevider( markerColumn, record ) );
      }
    }

    return markers.toArray( new IProfilPointMarker[] {} );
  }
}
