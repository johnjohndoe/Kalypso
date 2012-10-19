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
package org.kalypso.model.wspm.pdb.internal.wspm;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

import org.eclipse.core.runtime.IStatus;
import org.hibernatespatial.mgeom.MLineString;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.pdb.db.mapping.WaterlevelFixation;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;
import org.kalypso.model.wspm.pdb.wspm.ISectionProvider;
import org.opengis.geometry.MismatchedDimensionException;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

/**
 * @author Gernot Belger
 */
public class Waterlevel2dWorker
{
  private final IStatusCollector m_log = new StatusCollector( WspmPdbCorePlugin.PLUGIN_ID );

  private final Map<IProfileObject, ISectionProvider> m_waterlevels2D = new HashMap<>();

  private final Map<BigDecimal, Collection<ISectionProvider>> m_sectionsByStation;

  private final Collection<WaterlevelFixation> m_waterlevels;

  private final String m_eventName;

  public Waterlevel2dWorker( final String eventName, final Collection<WaterlevelFixation> waterlevels, final Map<BigDecimal, Collection<ISectionProvider>> sectionsByStation )
  {
    m_eventName = eventName;
    m_waterlevels = waterlevels;
    m_sectionsByStation = sectionsByStation;
  }

  public Map<IProfileObject, ISectionProvider> getWaterlevels2D( )
  {
    return Collections.unmodifiableMap( m_waterlevels2D );
  }

  public IStatus execute( )
  {
    /* hash by station */
    final Map<BigDecimal, Collection<WaterlevelFixation>> waterlevelsByStation = hashWaterlevelsByStation();

    for( final Entry<BigDecimal, Collection<WaterlevelFixation>> entry : waterlevelsByStation.entrySet() )
    {
      final BigDecimal station = entry.getKey();
      final Collection<WaterlevelFixation> waterlevels = entry.getValue();

      /* find cross section(s) for station */
      final Collection<ISectionProvider> sections = m_sectionsByStation.get( station );
      if( sections == null || sections.isEmpty() )
      {
        m_log.add( IStatus.WARNING, "No cross section for watrlevel with station %s", null, station );
        continue;
      }

      // REMARK: assign waterlevel to all sections with same station, because if 2 sections have the same station, we do not know what to do...
      for( final ISectionProvider section : sections )
      {
        final IProfileObject[] wParts = createWaterlevel( section, waterlevels );
        for( final IProfileObject wPart : wParts )
          m_waterlevels2D.put( wPart, section );
      }
    }

    return m_log.asMultiStatusOrOK( "Build 2D-Waterlevels" );
  }

  private Map<BigDecimal, Collection<WaterlevelFixation>> hashWaterlevelsByStation( )
  {
    final Map<BigDecimal, Collection<WaterlevelFixation>> hash = new TreeMap<>();

    for( final WaterlevelFixation waterlevel : m_waterlevels )
    {
      final BigDecimal station = waterlevel.getStation();

      if( !hash.containsKey( station ) )
        hash.put( station, new ArrayList<WaterlevelFixation>() );

      hash.get( station ).add( waterlevel );
    }

    return hash;
  }

  private IProfileObject[] createWaterlevel( final ISectionProvider section, final Collection<WaterlevelFixation> waterlevels )
  {
    /* gather some data */
    final BigDecimal station = section.getStation();
    final MLineString profileLine = section.getProfileLine();

    final ProjectedWaterlevels projectedWaterlevels = new ProjectedWaterlevels( m_eventName, station, profileLine, waterlevels );

    try
    {
      final IProfileObject[] parts = projectedWaterlevels.createParts();

      final IStatus status = projectedWaterlevels.getStatus();
      if( !status.isOK() )
        m_log.add( status );

      return parts;
    }
    catch( MismatchedDimensionException | FactoryException | TransformException e )
    {
      e.printStackTrace();
      m_log.add( IStatus.ERROR, station.toPlainString(), e );
      return new IProfileObject[] {};
    }
  }
}