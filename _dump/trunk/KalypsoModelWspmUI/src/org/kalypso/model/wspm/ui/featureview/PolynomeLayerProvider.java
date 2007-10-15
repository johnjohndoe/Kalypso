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
package org.kalypso.model.wspm.ui.featureview;

import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.chart.factory.configuration.parameters.IParameterContainer;
import org.kalypso.chart.factory.provider.AbstractLayerProvider;
import org.kalypso.chart.framework.model.IChartModel;
import org.kalypso.chart.framework.model.mapper.IAxis;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree_impl.gml.binding.math.IPolynomial1D;
import org.ksp.chart.factory.LayerType;

/**
 * Layer provider for the {@link PolynomeChartLayer}.
 * <p>
 * the following parameters are supported:
 * <ul>
 * <li>featureKey: String. Key, where to get the feature from the ChartDataProvider.</li>
 * <li>propertyName: QName. QName of the list-property of polynomes inside the given feature.</li>
 * <li>domainId: String. If set, the polynomes are filtered by this domain-phenomenon-id</li>
 * <li>pixelsPerTick: Integer, default 5. Determines the resolution how the polynomes are rendered. 1 means: for every
 * pixel in x-diretion, a polynome value is calculated and rendered.</li>
 * </ul>
 * 
 * @author Gernot Belger
 */
public class PolynomeLayerProvider extends AbstractLayerProvider
{

  /**
   * @see org.kalypso.swtchart.chart.layer.ILayerProvider#getLayers()
   */
  @SuppressWarnings("unchecked")
  public PolynomeChartLayer getLayer( final URL context )
  {
    LayerType lt = getLayerType();

    final IParameterContainer parameterContainer = getParameterContainer();

    final String domainId = parameterContainer.getParameterValue( "domainId", null );
    final boolean showPoints = Boolean.parseBoolean( parameterContainer.getParameterValue( "showPoints", "false" ) );
    final int pixelsPerTick = Integer.parseInt( parameterContainer.getParameterValue( "pixelsPerTick", "5" ) );

    final String featureKey = parameterContainer.getParameterValue( "featureKey", null );
    final String propertyNameStr = parameterContainer.getParameterValue( "propertyName", null );
    final QName propertyName = propertyNameStr == null ? null : QName.valueOf( propertyNameStr );

    final Feature feature = ChartDataProvider.FEATURE_MAP.get( featureKey );

    final String domainAxisId = lt.getMapper().getDomainAxisRef().getRef();
    final String targetAxisId = lt.getMapper().getTargetAxisRef().getRef();

    IChartModel chartModel = getChartModel();

    final IAxis<Number> domAxis = (IAxis<Number>) chartModel.getMapperRegistry().getAxis( domainAxisId );
    final IAxis<Number> valAxis = (IAxis<Number>) chartModel.getMapperRegistry().getAxis( targetAxisId );

    final FeatureList polygones = (FeatureList) feature.getProperty( propertyName );

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
    PolynomDataContainer data = new PolynomDataContainer( polyArray );
    final PolynomeChartLayer layer = new PolynomeChartLayer( data, domAxis, valAxis, pixelsPerTick, showPoints );
    layer.setTitle( lt.getTitle() );
    layer.setVisible( lt.getVisible() );

    return layer;
  }

}
