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
package org.kalypso.model.wspm.tuhh.ui.featureview;

import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import org.kalypso.layerprovider.TupleResultLineChartLayer;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.swtchart.chart.Chart;
import org.kalypso.swtchart.chart.axis.IAxis;
import org.kalypso.swtchart.chart.layer.IChartLayer;
import org.kalypso.swtchart.chart.layer.ILayerProvider;
import org.kalypso.swtchart.configuration.parameters.impl.ParameterHelper;
import org.kalypsodeegree.model.feature.Feature;
import org.ksp.chart.configuration.AxisType;
import org.ksp.chart.configuration.LayerType;

/**
 * @author Gernot Belger
 */
public class TupleResultLayerProvider implements ILayerProvider
{
  public static Map<String, Feature> FEATURE_MAP = new HashMap<String, Feature>();

  private LayerType m_lt;

  private Chart m_chart;

  public void init( final Chart chart, final LayerType lt )
  {
    m_chart = chart;
    m_lt = lt;
  }

  /**
   * @see org.kalypso.swtchart.chart.layer.ILayerProvider#getLayers()
   */
  public IChartLayer getLayer( final URL context )
  {
    final String configLayerId = m_lt.getName();

    final ParameterHelper ph = new ParameterHelper();
    ph.addParameters( m_lt.getParameters(), configLayerId );

    final String featureKey = ph.getParameterValue( "featureKey", null );
    final boolean showPoints = Boolean.valueOf( ph.getParameterValue( "showPoints", "true" ) );
    final Boolean showLines = Boolean.valueOf( ph.getParameterValue( "showLines", "true" ) );

    final Feature feature = FEATURE_MAP.get( featureKey );

    final IObservation<TupleResult> obs = ObservationFeatureFactory.toObservation( feature );
    final TupleResult result = obs.getResult();

    final String domainAxisId = ((AxisType) m_lt.getAxes().getDomainAxisRef().getRef()).getName();
    final String valueAxisId = ((AxisType) m_lt.getAxes().getValueAxisRef().getRef()).getName();

    final IAxis domAxis = m_chart.getAxisRegistry().getAxis( domainAxisId );

    final String domainComponentId = ph.getParameterValue( "domainComponentId", "" );
    final String valueComponentId = ph.getParameterValue( "valueComponentId", "" );

    final IAxis valAxis = m_chart.getAxisRegistry().getAxis( valueAxisId );

    final TupleResultLineChartLayer icl = new TupleResultLineChartLayer( result, domainComponentId, valueComponentId, domAxis, valAxis );
    icl.setName( m_lt.getTitle() );
    icl.setVisibility( m_lt.isVisible() );
    icl.setShowPoints( showPoints );
    icl.setShowLines( showLines );

    return icl;
  }
}
