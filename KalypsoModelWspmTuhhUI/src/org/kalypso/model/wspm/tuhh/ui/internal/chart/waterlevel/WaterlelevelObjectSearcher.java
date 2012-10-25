/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.tuhh.ui.internal.chart.waterlevel;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;

/**
 * @author Gernot Belger
 */
public class WaterlelevelObjectSearcher
{
  private final Map<String, WaterlevelObject> m_waterlevels = new HashMap<>();

  private final Collection<WaterlevelObject> m_namelessWaterlevels = new ArrayList<>();

  public WaterlelevelObjectSearcher( final IProfile profile )
  {
    if( profile == null )
      return;

    final IProfileObject[] objects = profile.getProfileObjects();
    for( final IProfileObject object : objects )
      addWaterlevel( object );
  }

  public WaterlevelObject getWaterlevel( final String id )
  {
    return m_waterlevels.get( id );
  }

  public WaterlevelObject[] getNamelessWaterlevels( )
  {
    return m_namelessWaterlevels.toArray( new WaterlevelObject[m_namelessWaterlevels.size()] );
  }

  private void addWaterlevel( final IProfileObject object )
  {
    final String type = object.getType();

    /* is really a waterlevel ? */
    if( !IWspmTuhhConstants.OBJECT_TYPE_WATERLEVEL_POINTS.equals( type ) && !IWspmTuhhConstants.OBJECT_TYPE_WATERLEVEL_POINTS.equals( type ) )
      return;

    /* add object to the level with same event name */
    final String eventName = object.getValue( IWspmTuhhConstants.PROFIL_PROPERTY_EVENT, null );
    if( StringUtils.isBlank( eventName ) )
      addNamelessWaterlevel( object );
    else
    {
      final WaterlevelObject waterlevel = createOrGetWaterlevel( eventName );
      waterlevel.addObject( object );
    }
  }

  private WaterlevelObject createOrGetWaterlevel( final String eventName )
  {
    if( !m_waterlevels.containsKey( eventName ) )
      m_waterlevels.put( eventName, new WaterlevelObject() );

    return m_waterlevels.get( eventName );
  }

  private void addNamelessWaterlevel( final IProfileObject object )
  {
    final WaterlevelObject namelessWaterlevel = new WaterlevelObject();
    namelessWaterlevel.addObject( object );

    m_namelessWaterlevels.add( namelessWaterlevel );
  }
}