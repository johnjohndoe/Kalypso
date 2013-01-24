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
package org.kalypso.model.hydrology.internal.preprocessing.resolve;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang3.tuple.Pair;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.Grundwasserabfluss;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * Finds all incoming groundwater relations for a catchment. Uses (lazy) caching to speed up lookup.
 * 
 * @author Gernot Belger
 */
class CatchmentGroundwaterFinder
{
  private final NaModell m_model;

  private Map<Catchment, Collection<Pair<Catchment, Grundwasserabfluss>>> m_lookup = null;

  public CatchmentGroundwaterFinder( final NaModell model )
  {
    m_model = model;
  }

  public Pair<Catchment, Grundwasserabfluss>[] findIncomingGroundwaterRelations( final Catchment target )
  {
    final Map<Catchment, Collection<Pair<Catchment, Grundwasserabfluss>>> lookup = getLookup();
    final Collection<Pair<Catchment, Grundwasserabfluss>> relations = lookup.get( target );
    return relations.toArray( new Pair[relations.size()] );
  }

  private synchronized Map<Catchment, Collection<Pair<Catchment, Grundwasserabfluss>>> getLookup( )
  {
    // Create lazy, as this is a costly operation, so we do it only if this is ever needed (i.e. drwbm are present)
    if( m_lookup == null )
    {
      m_lookup = new HashMap<>();
      initializeLookup();
    }

    return m_lookup;
  }

  private void initializeLookup( )
  {
    final IFeatureBindingCollection<Catchment> catchments = m_model.getCatchments();

    /* first make sure every target has a lookup element, saves extra null checks later */
    for( final Catchment target : catchments )
      m_lookup.put( target, new ArrayList<Pair<Catchment, Grundwasserabfluss>>() );

    /* now find all relations and hash them */
    for( final Catchment source : catchments )
    {
      final IFeatureBindingCollection<Grundwasserabfluss> groundwaterElements = source.getGrundwasserAbflussCollection();
      for( final Grundwasserabfluss groundwaterElement : groundwaterElements )
      {
        final Catchment groundwaterTarget = groundwaterElement.getNgwzu();

        addLookupRelation( groundwaterTarget, Pair.of( source, groundwaterElement ) );
      }
    }
  }

  private void addLookupRelation( final Catchment target, final Pair<Catchment, Grundwasserabfluss> incomingRelation )
  {
    final Collection<Pair<Catchment, Grundwasserabfluss>> elements = m_lookup.get( target );
    elements.add( incomingRelation );
  }
}
