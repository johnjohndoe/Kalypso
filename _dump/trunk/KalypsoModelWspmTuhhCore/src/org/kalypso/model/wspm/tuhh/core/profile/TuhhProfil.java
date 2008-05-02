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

import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.impl.AbstractProfil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.building.BuildingBruecke;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.building.BuildingWehr;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.durchlass.BuildingEi;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.durchlass.BuildingKreis;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.durchlass.BuildingMaul;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.durchlass.BuildingTrapez;
import org.kalypso.observation.IObservation;
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

  public TuhhProfil( final TupleResult result )
  {
    super( PROFIL_TYPE, result );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#setProfileObject(org.kalypso.model.wspm.core.profil.IProfileObject[])
   * @note for tuhh-profiles only ONE ProfileObject is allowed at same time
   * @throws IllegalStateException
   */
  @Override
  public IProfileObject[] addProfileObjects( final IProfileObject[] profileObjects )
  {
    if( profileObjects == null || profileObjects.length > 1 )
      throw new IllegalStateException( "only one profileObject allowed" );
    setProperty( PROFILE_OBJECTS, null );
    if( profileObjects.length > 0 )
      return super.addProfileObjects( profileObjects );
    
    return profileObjects;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#createProfileObjects(org.kalypso.observation.IObservation<org.kalypso.observation.result.TupleResult>[])
   * @note for tuhh-profiles only ONE ProfileObject is allowed at same time
   * @throws IllegalStateException
   */
  @Override
  public void createProfileObjects( final IObservation<TupleResult>[] profileObjects )
  {
    if( profileObjects == null || profileObjects.length > 1 )
      throw new IllegalStateException( "only one profileObject allowed" );
    final IProfileObject profileObject = createProfileObjectInternal(profileObjects[0] );
    addProfileObjects( new IProfileObject[] { profileObject } );
  }

  private IProfileObject createProfileObjectInternal( final IObservation<TupleResult> observation )
  {
    final String id = observation.getName();

    if( BuildingBruecke.ID.equals( id ) )
      return new BuildingBruecke( this, observation );
    else if( BuildingWehr.ID.equals( id ) )
      return new BuildingWehr( this, observation );
    else if( BuildingEi.ID.equals( id ) )
      return new BuildingEi( this, observation );
    else if( BuildingKreis.ID.equals( id ) )
      return new BuildingKreis( this, observation );
    else if( BuildingMaul.ID.equals( id ) )
      return new BuildingMaul( this, observation );
    else if( BuildingTrapez.ID.equals( id ) )
      return new BuildingTrapez( this, observation );
    return null;
  }
  public IProfilPointMarker createPointMarker( String markerID, IRecord point )
  {
    final IProfilPointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( getType() );
    if( provider==null )
      throw new IllegalStateException( "no ProfilPointProvider founf for: "+ getType() );

    final IComponent marker = getPointPropertyFor( markerID );
    /* first check, if provider provides markerType */
    if( !provider.isMarker( markerID ) )
      throw new IllegalStateException( "ProfilPointProvider doesn'tprovides - " + marker.getName() + " as Marker" );
    /* point has component already defined? */
    if( !hasPointProperty( marker) )
    {
      /* else create a new profile component */
      addPointProperty(  marker );
    }
    /* create a new profile point marker */
    return new ProfilDevider( marker, point );
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
    final List<IProfilPointMarker> pointMarkers = new ArrayList<IProfilPointMarker>();
    final IComponent[] markers = getPointMarkerTypes();
    for( final IComponent component : markers )
    {
      final IProfilPointMarker marker = getMarker( component, record );
      if( marker != null )
        pointMarkers.add( marker );
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
      final IProfilPointMarker marker = getMarker( markerColumn, record );
      if( marker != null )
        markers.add( marker );
    }

    return markers.toArray( new IProfilPointMarker[] {} );
  }

  private IProfilPointMarker getMarker( final IComponent component, final IRecord record )
  {
    final int index = indexOfProperty( component );
    if( index < 0 )
      return null;
    final Object value = record.getValue( index );

    if( value == null )
      return null;

    final String id = component.getId();
    if( id.equals( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) && value.equals( "none" ) )
      return null;

    if( id.equals( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE ) && value.equals( Boolean.FALSE ) )
      return null;

    if( id.equals( IWspmTuhhConstants.MARKER_TYP_BORDVOLL ) && value.equals( Boolean.FALSE ) )
      return null;

    if( id.equals( IWspmTuhhConstants.MARKER_TYP_WEHR ) && (value instanceof Double) && ((Double) value).isNaN() )
      return null;

    return new ProfilDevider( component, record );
  }
}
