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
package org.kalypso.model.wspm.ui.featureview;

import java.net.URL;

import javax.xml.namespace.QName;

import org.kalypso.chart.ext.observation.data.TupleResultDomainValueData;
import org.kalypso.chart.ext.observation.layer.TupleResultLineLayer;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

import de.openali.odysseus.chart.factory.config.parameters.IParameterContainer;
import de.openali.odysseus.chart.factory.provider.AbstractLayerProvider;
import de.openali.odysseus.chart.framework.model.layer.IChartLayer;
import de.openali.odysseus.chart.framework.model.style.ILineStyle;
import de.openali.odysseus.chart.framework.model.style.IPointStyle;

/**
 * Layer provider which provides a {@link TupleResultLineChartLayer} on a feature based observation.
 * <p>
 * The following arguments are supported:
 * <ul>
 * <li>showPoints [default=true]: If true, points are drawn even if no point style is given.</li>
 * <li>showLines [default=true]: Same as showPoints for lines.</li>
 * <li>featureKey: String. Key, where to get the feature from the ChartDataProvider.</li>
 * <li>propertyName: QName. If non null, the observation feature is found at that property of the given feature. Else
 * the given feature must be an observation itself.</li>
 * </ul>
 * 
 * @author Gernot Belger
 */
public class TupleResultLineLayerProvider extends AbstractLayerProvider
{
  /**
   * @see org.kalypso.swtchart.chart.layer.ILayerProvider#getLayers()
   */
  public IChartLayer getLayer( final URL context )
  {
    final TupleResultLineLayer icl = new TupleResultLineLayer( getDataContainer(), getStyleSet().getStyle( "line", ILineStyle.class ), getStyleSet().getStyle( "point", IPointStyle.class ) ); //$NON-NLS-1$ //$NON-NLS-2$
    return icl;
  }

  /**
   * @see de.openali.odysseus.chart.factory.provider.ILayerProvider#getDataContainer()
   */
  @SuppressWarnings( { "unchecked" })
  public TupleResultDomainValueData getDataContainer( )
  {
    final IParameterContainer pc = getParameterContainer();
    final String featureKey = pc.getParameterValue( "featureKey", null ); //$NON-NLS-1$
    final String propertyNameStr = pc.getParameterValue( "propertyName", null ); //$NON-NLS-1$
    final QName propertyName = propertyNameStr == null ? null : QName.valueOf( propertyNameStr );

    final Feature baseFeature = ChartDataProvider.FEATURE_MAP.get( featureKey );
    final Feature feature;
    if( propertyName == null )
      feature = baseFeature;
    else
      feature = FeatureHelper.getFeature( baseFeature.getWorkspace(), baseFeature.getProperty( propertyName ) );

    if( feature == null )
      return null;

    final IObservation<TupleResult> obs = ObservationFeatureFactory.toObservation( feature );
    final TupleResult result = obs.getResult();
    final String domainComponentId = pc.getParameterValue( "domainComponentId", "" ); //$NON-NLS-1$ //$NON-NLS-2$
    final String valueComponentId = pc.getParameterValue( "valueComponentId", "" ); //$NON-NLS-1$ //$NON-NLS-2$
    final TupleResultDomainValueData data = new TupleResultDomainValueData( result, domainComponentId, valueComponentId );
    return data;
  }
}
