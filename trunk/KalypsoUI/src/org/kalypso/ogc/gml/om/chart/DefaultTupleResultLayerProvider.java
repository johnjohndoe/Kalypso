/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
package org.kalypso.ogc.gml.om.chart;

import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.swtchart.axis.registry.IAxisRegistry;
import org.kalypso.swtchart.layer.IChartLayer;
import org.kalypso.swtchart.layer.ILayerProvider;
import org.kalypso.swtchart.layer.impl.TestLayer;

/**
 * @author schlienger
 */
public class DefaultTupleResultLayerProvider implements ILayerProvider
{
  private final IObservation<TupleResult> m_obs;

  private final IComponent m_domainComponent;

  private final IComponent m_valueComponent;

  private final IAxisRegistry m_registry;

  private final String m_domAxisID;

  private final String m_valAxisID;

  public DefaultTupleResultLayerProvider( final IAxisRegistry registry, final IObservation<TupleResult> obs, final IComponent domainComponent, final IComponent valueComponent, final String domAxisID, final String valAxisID )
  {
    m_registry = registry;
    m_obs = obs;

    m_domainComponent = domainComponent;
    m_valueComponent = valueComponent;

    m_domAxisID = domAxisID;
    m_valAxisID = valAxisID;
  }

  /**
   * @see de.belger.swtchart.layer.ILayerProvider#getLayers()
   */
  public IChartLayer[] getLayers( )
  {
    final TupleResult result = m_obs.getResult();
    
    final TupleResultLineChartLayer layer = new TupleResultLineChartLayer( result, m_domainComponent, m_valueComponent, m_registry.getAxis( m_domAxisID ), m_registry.getAxis( m_valAxisID ) );
    return new IChartLayer[] { layer /*, new TestLayer(m_registry.getAxis( m_domAxisID ), m_registry.getAxis( m_valAxisID ))*/ };
  }
}
