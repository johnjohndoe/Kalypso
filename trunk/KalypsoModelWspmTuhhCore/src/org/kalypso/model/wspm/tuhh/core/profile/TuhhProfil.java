/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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

import org.kalypso.commons.java.lang.Arrays;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.impl.AbstractProfil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.IProfileBuilding;
import org.kalypso.observation.IObservationVisitor;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

/**
 * @author Dirk Kuch
 * @author kimwerner
 */
public class TuhhProfil extends AbstractProfil
{
  public static final String PROFIL_TYPE = "org.kalypso.model.wspm.tuhh.profiletype"; //$NON-NLS-1$

  public TuhhProfil( final TupleResult result, final Object source )
  {
    super( PROFIL_TYPE, result, source );
    result.setInterpolationHandler( new TUHHInterpolationHandler() );
  }

  @Override
  public IProfileObject[] addProfileObjects( final IProfileObject... profileObjects )
  {
    // TODO: this restriction only exists for buildings! Other objects may occur several times...
    final IProfileObject[] objects = getProfileObjects( IProfileBuilding.class );
    for( final IProfileObject object : objects )
    {
      removeProfileObject( object );
    }

    return super.addProfileObjects( profileObjects );
  }

  /**
   * FIXME: this creates a marker (virtually) but does not really change the profile, except maybe add the
   * marker-component to it.<br/>
   * This is very confusing! Instead, we should directly set the value and return the real marker.
   */
  @Override
  public IProfilPointMarker createPointMarker( final String markerID, final IRecord point )
  {
    final IProfilPointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( getType() );
    if( provider == null )
      throw new IllegalStateException( Messages.getString( "org.kalypso.model.wspm.tuhh.core.profile.TuhhProfil.3", getType() ) ); //$NON-NLS-1$

    final IComponent marker = getPointPropertyFor( markerID );
    /* first check, if provider provides markerType */
    if( !provider.isMarker( markerID ) )
      throw new IllegalStateException( Messages.getString( "org.kalypso.model.wspm.tuhh.core.profile.TuhhProfil.4", marker.getName() ) ); //$NON-NLS-1$
    /* point has component already defined? */
    if( !hasPointProperty( marker ) )
    {
      /* else create a new profile component */
      addPointProperty( marker );
    }
    /* create a new profile point marker */
    return new ProfilDevider( marker, point );
  }

  @Override
  public boolean removePoint( final IRecord point )
  {
    final IProfilPointMarker[] markers = getPointMarkerFor( point );
    if( Arrays.isEmpty( markers ) )
      return super.removePoint( point );

    return false;
  }

  @Override
  public IProfilPointMarker[] getPointMarkerFor( final IRecord record )
  {
    final List<IProfilPointMarker> pointMarkers = new ArrayList<IProfilPointMarker>();
    final IComponent[] markers = getPointMarkerTypes();
    for( final IComponent component : markers )
    {
      final IProfilPointMarker marker = getMarker( component, record );
      if( marker != null )
      {
        pointMarkers.add( marker );
      }
    }
    return pointMarkers.toArray( new IProfilPointMarker[] {} );
  }

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
      {
        markers.add( marker );
      }
    }

    return markers.toArray( new IProfilPointMarker[] {} );
  }

  private IProfilPointMarker getMarker( final IComponent component, final IRecord record )
  {
    final int index = indexOfProperty( component );
    if( index < 0 )
      return null;

    final Object value = record.getValue( index );
    if( Objects.isNull( value ) )
      return null;

    final String identifier = component.getId();
    if( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE.equals( identifier ) && "none".equals( value ) ) //$NON-NLS-1$
      return null;
    else if( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE.equals( identifier ) && Boolean.FALSE.equals( value ) )
      return null;
    else if( IWspmTuhhConstants.MARKER_TYP_BORDVOLL.equals( identifier ) && Boolean.FALSE.equals( value ) )
      return null;
    else if( IWspmTuhhConstants.MARKER_TYP_WEHR.equals( identifier ) && value instanceof Double && ((Double) value).isNaN() )
      return null;

    return new ProfilDevider( component, record );
  }

  @Override
  public void accept( final IObservationVisitor visitor )
  {
    throw new UnsupportedOperationException();
  }
}
