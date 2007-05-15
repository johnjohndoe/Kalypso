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
package org.kalypso.kalypsomodel1d2d.ui.chart;

import java.net.URL;

import org.kalypso.swtchart.chart.Chart;
import org.kalypso.swtchart.chart.axis.IAxis;
import org.kalypso.swtchart.chart.layer.ChartDataProvider;
import org.kalypso.swtchart.chart.layer.IChartLayer;
import org.kalypso.swtchart.chart.layer.ILayerProvider;
import org.kalypso.swtchart.configuration.parameters.impl.ParameterHelper;
import org.kalypso.swtchart.exception.LayerProviderException;
import org.kalypsodeegree.model.feature.Feature;
import org.ksp.chart.configuration.AxisType;
import org.ksp.chart.configuration.LayerType;

/**
 * @author Gernot Belger
 */
public class KingLayerProvider implements ILayerProvider
{
  private Chart m_chart;
  private LayerType m_lt;

  /**
   * @see org.kalypso.swtchart.chart.layer.ILayerProvider#init(org.kalypso.swtchart.chart.Chart, org.ksp.chart.configuration.LayerType)
   */
  public void init( final Chart chart, final LayerType lt )
  {
    m_chart = chart;
    m_lt = lt;
  }
  
  /**
   * @see org.kalypso.swtchart.chart.layer.ILayerProvider#getLayer(java.net.URL)
   */
  @SuppressWarnings("unchecked")
  public IChartLayer getLayer( final URL context ) throws LayerProviderException
  {
    final String configLayerId = m_lt.getName();

    final ParameterHelper ph = new ParameterHelper();
    ph.addParameters( m_lt.getParameters(), configLayerId );

    final String featureKey = ph.getParameterValue( "featureKey", null );
    if( featureKey == null )
      throw new LayerProviderException( "Missing parameter: featureKey" );

    final Feature kingFeature = ChartDataProvider.FEATURE_MAP.get( featureKey );
    if( kingFeature == null )
      throw new LayerProviderException( "No feature found for key: " + featureKey );

    final String domainAxisId = ((AxisType) m_lt.getAxes().getDomainAxisRef().getRef()).getName();
    final String valueAxisId = ((AxisType) m_lt.getAxes().getValueAxisRef().getRef()).getName();

    final IAxis<Number> domAxis = m_chart.getAxisRegistry().getAxis( domainAxisId );
    final IAxis<Number> valAxis = m_chart.getAxisRegistry().getAxis( valueAxisId );

    final String title = m_lt.getTitle();
    final String description = m_lt.getDescription();
    final KingLayer kingLayer = new KingLayer( kingFeature, title, description, domAxis, valAxis );
    kingLayer.setVisibility( m_lt.isVisible() );
    
    return kingLayer;
  }

}
