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
package org.kalypso.model.wspm.tuhh.ui.chart;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfile;

/**
 * Singleton for remembering second profiles confgiured for a profile reach.
 *
 * @author Gernot Belger
 */
public class SecondProfileDataManager
{
  private static SecondProfileDataManager INSTANCE = null;

  public synchronized static SecondProfileDataManager instance( )
  {
    if( INSTANCE == null )
      INSTANCE = new SecondProfileDataManager();

    return INSTANCE;
  }

  private final Map<Object, Collection<SecondProfileData>> m_data = new HashMap<>();

  private SecondProfileDataManager( )
  {
    // TODO: init from whre? dialog settings?
  }

  public SecondProfileData[] findData( final IProfile profile )
  {
    final Object key = findKey( profile );
    if( key == null )
      return new SecondProfileData[0];

    return findData( key );
  }

  private Object findKey( final IProfile profile )
  {
    if( profile == null )
      return null;

    final IProfileFeature profileFeature = profile.getSource();
    if( profileFeature == null )
      return null;

    // FIXME: instead we would like to know the strand where we are in...
    return profileFeature.getWater();
  }

  private SecondProfileData[] findData( final Object key )
  {
    final Collection<SecondProfileData> data = getData( key );
    return data.toArray( new SecondProfileData[data.size()] );
  }

  private Collection<SecondProfileData> getData( final Object key )
  {
    if( !m_data.containsKey( key ) )
      m_data.put( key, new ArrayList<SecondProfileData>() );

    return m_data.get( key );
  }

  public void addData( final IProfile profil )
  {
    final Object key = findKey( profil );
    if( key == null )
      return;

    final SecondProfileData data = new SecondProfileData();

    final Collection<SecondProfileData> dataCollection = getData( key );
    dataCollection.add( data );
  }

  public void removeData( final IProfile profil, final SecondProfileData data )
  {
    final Object key = findKey( profil );
    if( key == null )
      return;

    final Collection<SecondProfileData> dataCollection = getData( key );
    dataCollection.remove( data );
  }
}