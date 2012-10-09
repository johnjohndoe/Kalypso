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
package org.kalypso.model.wspm.tuhh.ui.imports.ewawi;

import java.math.BigDecimal;
import java.util.HashMap;
import java.util.Map;

import org.kalypso.model.wspm.core.gml.WspmFixation;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.ewawi.data.EwawiPro;
import org.kalypso.model.wspm.ewawi.data.EwawiProLine;
import org.kalypso.model.wspm.ewawi.data.EwawiSta;
import org.kalypso.model.wspm.ewawi.utils.EwawiException;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Holger Albert
 */
public class EwawiWaterLevelFixationUpdater
{
  private static final String EWAWI_WATER_LEVEL_FIXATION = Messages.getString("EwawiWaterLevelFixationUpdater.0"); //$NON-NLS-1$

  private final TuhhWspmProject m_targetProject;

  private final EwawiSta m_staIndex;

  private final EwawiPro m_proIndex;

  public EwawiWaterLevelFixationUpdater( final TuhhWspmProject targetProject, final EwawiSta staIndex, final EwawiPro proIndex )
  {
    m_targetProject = targetProject;
    m_staIndex = staIndex;
    m_proIndex = proIndex;
  }

  public void updateWaterLevelFixation( ) throws EwawiException
  {
    final Map<Long, EwawiWaterLevelPointCache> wlPointCache = cacheWaterLevelPoints();

    updateWaterLevelFixation( wlPointCache );
  }

  private Map<Long, EwawiWaterLevelPointCache> cacheWaterLevelPoints( )
  {
    final Map<Long, EwawiWaterLevelPointCache> cache = new HashMap<>();

    final EwawiProLine[] proLines = m_proIndex.getProLines();
    for( final EwawiProLine proLine : proLines )
    {
      Long gewKennzahl = proLine.getGewKennzahl();
      if( gewKennzahl == null )
        gewKennzahl = new Long( -1 );

      if( !cache.containsKey( gewKennzahl ) )
        cache.put( gewKennzahl, new EwawiWaterLevelPointCache( m_staIndex ) );

      final EwawiWaterLevelPointCache pointCache = cache.get( gewKennzahl );
      pointCache.addProLine( proLine );
    }

    return cache;
  }

  private void updateWaterLevelFixation( final Map<Long, EwawiWaterLevelPointCache> wlPointCache ) throws EwawiException
  {
    for( final Long gewKennzahl : wlPointCache.keySet() )
    {
      final EwawiWaterLevelPointCache pointCache = wlPointCache.get( gewKennzahl );

      final WspmWaterBody waterBody = m_targetProject.findWaterByRefNr( String.format( "%d", gewKennzahl ) ); //$NON-NLS-1$
      if( waterBody != null )
      {
        updateWaterLevelFixation( pointCache, waterBody );
        continue;
      }
    }
  }

  private void updateWaterLevelFixation( final EwawiWaterLevelPointCache pointCache, final WspmWaterBody waterBody ) throws EwawiException
  {
    final WspmFixation ewawiFixation = getEwawiFixation( waterBody );
    final IObservation<TupleResult> observation = ewawiFixation.toObservation();

    final BigDecimal[] stations = pointCache.getStations();
    for( final BigDecimal station : stations )
      updateEwawiFixation( observation, station, pointCache.createWaterLevel( station ) );

    ewawiFixation.saveObservation( observation );
  }

  private WspmFixation getEwawiFixation( final WspmWaterBody waterBody )
  {
    final WspmFixation ewawiFixation = (WspmFixation)waterBody.findFixationByName( m_proIndex.getSourceFile().getName() );
    if( ewawiFixation != null )
      return ewawiFixation;

    return createEwawiFixation( waterBody );
  }

  private WspmFixation createEwawiFixation( final WspmWaterBody waterBody )
  {
    final IFeatureBindingCollection<WspmFixation> wspFixations = waterBody.getWspFixations();
    final WspmFixation wspFixation = wspFixations.addNew( WspmFixation.QNAME_FEATURE_WSPM_FIXATION );

    final IObservation<TupleResult> obs = wspFixation.toObservation();
    obs.setName( m_proIndex.getSourceFile().getName() );
    obs.setDescription( EWAWI_WATER_LEVEL_FIXATION );

    wspFixation.saveObservation( obs );

    return wspFixation;
  }

  private void updateEwawiFixation( final IObservation<TupleResult> observation, final BigDecimal station, final EwawiWaterLevel waterLevel ) throws EwawiException
  {
    final TupleResult result = observation.getResult();

    final int stationComp = result.indexOfComponent( WspmFixation.COMPONENT_STATION );
    final int wspComp = result.indexOfComponent( WspmFixation.COMPONENT_WSP );
    final int commentComp = result.indexOfComponent( WspmFixation.COMPONENT_COMMENT );

    final IRecord record = result.createRecord();
    record.setValue( stationComp, station );
    record.setValue( wspComp, new BigDecimal( waterLevel.calculateMeanValue() ) );
    record.setValue( commentComp, waterLevel.getComment() );

    result.add( record );
  }
}