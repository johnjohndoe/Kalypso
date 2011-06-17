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
package org.kalypso.model.wspm.tuhh.ui.chart;

import java.math.BigDecimal;
import java.net.URL;

import javax.xml.namespace.QName;

import org.kalypso.model.wspm.ui.featureview.ChartDataProvider;
import org.kalypsodeegree.model.feature.Feature;

import de.openali.odysseus.chart.factory.provider.AbstractLayerProvider;
import de.openali.odysseus.chart.framework.model.layer.IChartLayer;
import de.openali.odysseus.chart.framework.model.layer.IParameterContainer;
import de.openali.odysseus.chart.framework.model.style.ILineStyle;
import de.openali.odysseus.chart.framework.model.style.IPointStyle;

/**
 * This layer provider can create vertical line layers.
 * 
 * @author Holger Albert
 */
public class VerticalLineLayerProvider extends AbstractLayerProvider
{
  /**
   * @see de.openali.odysseus.chart.framework.model.layer.ILayerProvider#getLayer(java.net.URL)
   */
  @Override
  public IChartLayer getLayer( final URL context )
  {
    final IParameterContainer pc = getParameterContainer();
    final String featureKey = pc.getParameterValue( "featureKey", null );
    final String propertyNameStr = pc.getParameterValue( "propertyName", null );
    final QName propertyName = propertyNameStr == null ? null : QName.valueOf( propertyNameStr );
    if( propertyName == null )
      return null;

    final Feature baseFeature = ChartDataProvider.FEATURE_MAP.get( featureKey );
    if( baseFeature == null )
      return null;

    final BigDecimal bankfullHeight = (BigDecimal) baseFeature.getProperty( propertyName );
    if( bankfullHeight == null )
      return null;

    return new VerticalLineLayer( this, getStyleSet().getStyle( "line", ILineStyle.class ), getStyleSet().getStyle( "point", IPointStyle.class ), new BigDecimal[] { bankfullHeight } );
  }
}