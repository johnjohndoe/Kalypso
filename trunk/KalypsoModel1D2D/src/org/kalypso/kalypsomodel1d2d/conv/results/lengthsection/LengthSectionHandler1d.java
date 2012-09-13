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
package org.kalypso.kalypsomodel1d2d.conv.results.lengthsection;

import java.math.BigDecimal;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.observation.util.TupleResultIndex;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author Thomas Jung
 * @author Gernot Belger
 *
 */
public class LengthSectionHandler1d
{
  private final String m_stationComponentID;

  private final Map<TupleResult, TupleResultIndex> m_indexMap = new HashMap<>();

  private final Map<ICalculationUnit1D, IObservation<TupleResult>> m_obsMap = new HashMap<>();

  private final Map<ICalculationUnit1D, GMLWorkspace> m_workspaceMap = new HashMap<>();

  private final URL m_templateUrl;

  public LengthSectionHandler1d( final String stationComponentID, final URL templateUrl )
  {
    m_stationComponentID = stationComponentID;
    m_templateUrl = templateUrl;
  }

  public void addValue( final ICalculationUnit1D calcUnit, final BigDecimal station, final BigDecimal value, final String componentID ) throws Exception
  {
    // find calcUnitObs
    final IObservation<TupleResult> lsObs = getObs( calcUnit );

    // find component
    final TupleResult tuples = lsObs.getResult();
    final IComponent[] components = tuples.getComponents();
    final IComponent component = ComponentUtilities.findComponentByID( components, componentID );

    // find record for station
    final IRecord record = findRecord( tuples, station );

    // add Data
    record.setValue( component, value );
  }

  private IRecord findRecord( final TupleResult tuples, final BigDecimal station )
  {
    final IComponent[] components = tuples.getComponents();
    final IComponent component = ComponentUtilities.findComponentByID( components, m_stationComponentID );

    final TupleResultIndex index = m_indexMap.get( tuples );

    final IRecord record = index.getRecord( station );

    if( record != null )
      return record;

    final IRecord createRecord = tuples.createRecord();
    createRecord.setValue( component, station );
    tuples.add( createRecord );

    return createRecord;
  }

  private IObservation<TupleResult> getObs( final ICalculationUnit1D calcUnit ) throws Exception
  {
    if( m_obsMap.containsKey( calcUnit ) )
      return m_obsMap.get( calcUnit );

    /* GMLWorkspace für Ergebnisse anlegen */
    final GMLWorkspace lsObsWorkspace = GmlSerializer.createGMLWorkspace( m_templateUrl, null );

    m_workspaceMap.put( calcUnit, lsObsWorkspace );

    final IObservation<TupleResult> lsObs = ObservationFeatureFactory.toObservation( lsObsWorkspace.getRootFeature() );
    // remember name of calc unit
    lsObs.setName( calcUnit.getName() );

    m_obsMap.put( calcUnit, lsObs );

    final TupleResult tuples = lsObs.getResult();
    final IComponent[] components = tuples.getComponents();
    final IComponent stationComponent = ComponentUtilities.findComponentByID( components, m_stationComponentID );

    final TupleResultIndex index = new TupleResultIndex( tuples, stationComponent );
    m_indexMap.put( tuples, index );

    return lsObs;

  }

  public ICalculationUnit1D[] getCalcUnits( )
  {
    return m_obsMap.keySet().toArray( new ICalculationUnit1D[m_obsMap.size()] );
  }

  public GMLWorkspace getWorkspace( final ICalculationUnit1D calcUnit )
  {
    return m_workspaceMap.get( calcUnit );
  }

  public IObservation<TupleResult> getObservation( final ICalculationUnit1D calcUnit )
  {
    return m_obsMap.get( calcUnit );
  }
}
