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
package org.kalypso.model.wspm.core.gml;

import java.math.BigDecimal;

import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.ProfilFactory;
import org.kalypso.model.wspm.core.profil.util.ProfilObsHelper;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypsodeegree.model.feature.Feature;

/**
 * WSPMUi View/Editor Parts always tries to create new IProfile's ({@link ProfileFeatureFactory.toProfile()}). With
 * this little hack few IProfile's will be cached and not newly created. <br>
 * 
 * @author Dirk Kuch
 */
public class ProfileFeatureManager
{
  private class CachedFeature
  {
    public Feature feature;

    public IProfil profil;

    public CachedFeature( final Feature f, final IProfil p )
    {
      feature = f;
      profil = p;
    }
  }

  private static final int CACHE_SIZE = 1;

  private final CachedFeature[] m_cache = new CachedFeature[CACHE_SIZE];

  private int m_pCache = 0;

  public IProfil getProfile( final Feature profileFeature )
  {
    IProfil profil = getFromCache( profileFeature );

    if( profil == null )
    {
      /* profile type */
      final String type = (String) profileFeature.getProperty( ProfileFeatureFactory.QNAME_TYPE );
      if( type == null )
        return null;

      /* observation of profile */
      final IObservation<TupleResult> observation = ObservationFeatureFactory.toObservation( profileFeature );

      profil = ProfilFactory.createProfil( type, observation );

      /* station of profile */
      final BigDecimal bigStation = (BigDecimal) profileFeature.getProperty( ProfileFeatureFactory.QNAME_STATION );
      if( bigStation != null )
      {
        final double station = bigStation.doubleValue();
        profil.setStation( station );
      }

      /* building of profile */
      // REMARK: handle buildings before table, because the setBuilding method resets the
      // corresponding table properties.
      final IObservation<TupleResult>[] profileObjects = ProfilObsHelper.getProfileObjects( profileFeature );
      if( profileObjects.length > 0 )
        profil.createProfileObjects( profileObjects );

      appendToCache( profileFeature, profil );
    }

    return profil;

  }

  private IProfil getFromCache( final Feature profileFeature )
  {
    for( final CachedFeature cache : m_cache )
    {
      if( cache == null )
        continue;

      if( cache.feature.equals( profileFeature ) )
        return cache.profil;
    }

    return null;
  }

  private void appendToCache( final Feature feature, final IProfil profil )
  {
    if( m_pCache >= CACHE_SIZE )
      m_pCache = 0;

    m_cache[m_pCache] = new CachedFeature( feature, profil );
    m_pCache++;
  }

}
