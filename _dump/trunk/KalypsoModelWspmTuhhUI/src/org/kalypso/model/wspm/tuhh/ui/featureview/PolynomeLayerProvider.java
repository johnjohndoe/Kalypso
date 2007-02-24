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
import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.swtchart.chart.Chart;
import org.kalypso.swtchart.chart.axis.IAxis;
import org.kalypso.swtchart.chart.layer.IChartLayer;
import org.kalypso.swtchart.chart.layer.ILayerProvider;
import org.kalypso.swtchart.configuration.parameters.impl.ParameterHelper;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree_impl.gml.binding.math.IPolynomial1D;
import org.ksp.chart.configuration.AxisType;
import org.ksp.chart.configuration.LayerType;

/**
 * Layer provider for the {@link PolynomeChartLayer}.
 * <p>
 * the following parameters are supported:
 * <ul>
 * <li>featureKey: String. Key, where to get the feature from.</li>
 * <li>domainId: String. If set, the polynomes are filtered by this domain-phenomenon-id</li>
 * <li>pixelsPerTick: Integer, default 5. Determines the resolution how the polynomes are rendered. 1 means: for every
 * pixel in x-diretion, a polynome value is calculated and rendered.</li>
 * </ul>
 * 
 * @author Gernot Belger
 */
public class PolynomeLayerProvider implements ILayerProvider
{
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

    final String domainId = ph.getParameterValue( "domainId", null );
    final boolean showPoints = Boolean.parseBoolean( ph.getParameterValue( "showPoints", "false" ) );
    final int pixelsPerTick = Integer.parseInt( ph.getParameterValue( "pixelsPerTick", "5" ) );

    // TODO: More generel concept to retrieve data for the layer
    final String featureKey = ph.getParameterValue( "featureKey", null );
    final Feature feature = ChartDataProvider.FEATURE_MAP.get( featureKey );

    final String domainAxisId = ((AxisType) m_lt.getAxes().getDomainAxisRef().getRef()).getName();
    final String valueAxisId = ((AxisType) m_lt.getAxes().getValueAxisRef().getRef()).getName();

    final IAxis domAxis = m_chart.getAxisRegistry().getAxis( domainAxisId );
    final IAxis valAxis = m_chart.getAxisRegistry().getAxis( valueAxisId );

    final FeatureList polygones = (FeatureList) feature.getProperty( new QName( IWspmTuhhConstants.NS_WSPM_TUHH, "polynomialMember" ) );

    /* Filter polynomes by their domainId */
    final List<IPolynomial1D> polys = new ArrayList<IPolynomial1D>();
    for( final Object object : polygones )
    {
      final Feature polyFeature = (Feature) object;
      final IPolynomial1D poly1d = (IPolynomial1D) polyFeature.getAdapter( IPolynomial1D.class );
      if( domainId == null || domainId.equals( poly1d.getDomainPhenomenon() ) )
        polys.add( poly1d );
    }

    final IPolynomial1D[] polyArray = polys.toArray( new IPolynomial1D[polys.size()] );
    final PolynomeChartLayer layer = new PolynomeChartLayer( polyArray, domAxis, valAxis, pixelsPerTick, showPoints );
    layer.setName( m_lt.getTitle() );
    layer.setVisibility( m_lt.isVisible() );

    return layer;
  }
}
