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


import org.kalypso.chart.factory.configuration.exception.LayerProviderException;
import org.kalypso.chart.factory.configuration.parameters.IParameterContainer;
import org.kalypso.chart.factory.provider.AbstractLayerProvider;
import org.kalypso.chart.framework.model.IChartModel;
import org.kalypso.chart.framework.model.layer.IChartLayer;
import org.kalypso.chart.framework.model.mapper.IAxis;
import org.kalypso.model.wspm.ui.featureview.ChartDataProvider;
import org.kalypsodeegree.model.feature.Feature;
import org.ksp.chart.factory.LayerType;


/**
 * @author Gernot Belger
 */
public class KingLayerProvider extends AbstractLayerProvider
{
  
  /**
   * @see org.kalypso.swtchart.chart.layer.ILayerProvider#getLayer(java.net.URL)
   */
  @SuppressWarnings("unchecked")
  public IChartLayer getLayer( final URL context ) throws LayerProviderException
  {
    
    IParameterContainer pc = getParameterContainer();
    LayerType lt = getLayerType();
    IChartModel chartModel = getChartModel();
    
    final String featureKey = pc.getParameterValue( "featureKey", null );
    if( featureKey == null )
      throw new LayerProviderException( "Missing parameter: featureKey" );

    final Feature kingFeature = ChartDataProvider.FEATURE_MAP.get( featureKey );
    if( kingFeature == null )
      throw new LayerProviderException( "No feature found for key: " + featureKey );

    final String domainAxisId = lt.getMapper().getDomainAxisRef().getRef();
    final String targetAxisId = lt.getMapper().getTargetAxisRef().getRef();

    final IAxis<Number> domAxis = chartModel.getMapperRegistry().getAxis( domainAxisId );
    final IAxis<Number> targetAxis = chartModel.getMapperRegistry().getAxis( targetAxisId );

    KingDataContainer data=new KingDataContainer(kingFeature);
    
    final KingLayer kingLayer = new KingLayer( data, domAxis, targetAxis );
    kingLayer.setVisible(  lt.getVisible() );
    
    return kingLayer;
  }

}
