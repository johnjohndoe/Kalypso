/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.wspm.tuhh.ui.chart.layers;

import java.net.URL;

import org.kalypso.chart.ext.observation.data.TupleResultDomainValueData;
import org.kalypso.chart.ext.observation.layer.TupleResultLineLayer;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.ui.featureview.TupleResultLineLayerProvider;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;

import de.openali.odysseus.chart.framework.model.layer.IChartLayer;
import de.openali.odysseus.chart.framework.model.layer.IParameterContainer;
import de.openali.odysseus.chart.framework.model.style.ILineStyle;
import de.openali.odysseus.chart.framework.model.style.IPointStyle;
import de.openali.odysseus.chart.framework.model.style.IStyleSet;

/**
 * @author Gernot Belger
 */
public class LengthSectionLayerProvider extends TupleResultLineLayerProvider
{
  // FIXME: bad design!
  // There should be exactly one layer provider per layer implementation

  /**
   * @see org.kalypso.swtchart.chart.layer.ILayerProvider#getLayers()
   */
  @Override
  public IChartLayer getLayer( final URL context )
  {
    final IParameterContainer parameterContainer = getParameterContainer();

    final String targetComponentName = parameterContainer.getParameterValue( "targetComponentId", null ); //$NON-NLS-1$
    final IStyleSet styleSet = getStyleSet();

    if( IWspmConstants.LENGTH_SECTION_PROPERTY_BRIDGE_OK.equals( targetComponentName ) )
      return new LengthSectionBridgeLayer( this, getDataContainer(), styleSet.getStyle( "line", ILineStyle.class ), styleSet.getStyle( "point", IPointStyle.class ) ); //$NON-NLS-1$ //$NON-NLS-2$

    // FIXME: still used in this way?
    if( IWspmConstants.LENGTH_SECTION_THEME_BUILDINGS.equals( targetComponentName ) )
    {
      final String buildingType = parameterContainer.getParameterValue( "buildingType", null ); //$NON-NLS-1$
      final ILineStyle lineStyle = styleSet.getStyle( "buildingLine", ILineStyle.class ); //$NON-NLS-1$
      final IPointStyle pointStyle = styleSet.getStyle( "buildingPoint", IPointStyle.class ); //$NON-NLS-1$

      if( "bridge".equals( buildingType ) ) //$NON-NLS-1$
        return new LengthSectionBridgeLayer( this, getDataContainer( parameterContainer, IWspmConstants.LENGTH_SECTION_PROPERTY_BRIDGE_OK ), lineStyle, pointStyle );
      if( "weir".equals( buildingType ) ) //$NON-NLS-1$
        return new LengthSectionWeirLayer( this, getDataContainer( parameterContainer, IWspmConstants.LENGTH_SECTION_PROPERTY_WEIR_OK ), lineStyle, pointStyle );
      if( "culvert".equals( buildingType ) )//$NON-NLS-1$
        return new LengthSectionCulvertLayer( this, getDataContainer( parameterContainer, IWspmConstants.LENGTH_SECTION_PROPERTY_ROHR_DN ), lineStyle, pointStyle );

      throw new IllegalStateException( "Unknown building type: " + buildingType );//$NON-NLS-1$
    }

    if( IWspmConstants.LENGTH_SECTION_PROPERTY_WEIR_OK.equals( targetComponentName ) )
      return new LengthSectionWeirLayer( this, getDataContainer(), styleSet.getStyle( "line", ILineStyle.class ), styleSet.getStyle( "point", IPointStyle.class ) ); //$NON-NLS-1$ //$NON-NLS-2$

    if( IWspmConstants.LENGTH_SECTION_PROPERTY_ROHR_DN.equals( targetComponentName ) )
      return new LengthSectionCulvertLayer( this, getDataContainer(), styleSet.getStyle( "line", ILineStyle.class ), styleSet.getStyle( "point", IPointStyle.class ) ); //$NON-NLS-1$ //$NON-NLS-2$

    if( IWspmConstants.LENGTH_SECTION_PROPERTY_RUNOFF.equals( targetComponentName ) )
      return new LengthSectionRunOffLayer( this, getDataContainer(), styleSet.getStyle( "line", ILineStyle.class ), styleSet.getStyle( "point", IPointStyle.class ) ); //$NON-NLS-1$ //$NON-NLS-2$

    if( IWspmConstants.LENGTH_SECTION_PROPERTY_GROUND.equals( targetComponentName ) )
      return new LengthSectionSoilLayer( this, getDataContainer(), styleSet.getStyle( "line", ILineStyle.class ), styleSet.getStyle( "point", IPointStyle.class ) ); //$NON-NLS-1$ //$NON-NLS-2$

    return new TupleResultLineLayer( this, getDataContainer(), getStyleSet().getStyle( "line", ILineStyle.class ), getStyleSet().getStyle( "point", IPointStyle.class ) ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  private TupleResultDomainValueData< ? , ? > getDataContainer( )
  {
    final IParameterContainer pc = getParameterContainer();
    return getDataContainer( pc, pc.getParameterValue( "targetComponentId", null ) ); //$NON-NLS-1$
  }

  private TupleResultDomainValueData< ? , ? > getDataContainer( final IParameterContainer pc, final String targetComponentId )
  {
    final String domainComponentName = pc.getParameterValue( "domainComponentId", null ); //$NON-NLS-1$
    if( domainComponentName == null || targetComponentId == null )
      return null;

    // try to find loaded observation (from GFT)
    final IObservation<TupleResult> obs = getObservation();
    if( obs != null )
    {
      return new TupleResultDomainValueData<Object, Object>( obs, domainComponentName, targetComponentId );
    }
    else
    {
      final String href = pc.getParameterValue( "href", null ); //$NON-NLS-1$
      final String observationId = pc.getParameterValue( "observationId", null ); //$NON-NLS-1$
      if( href != null && observationId != null )
        return new TupleResultDomainValueData<Object, Object>( getContext(), href, observationId, domainComponentName, targetComponentId );
      else
        return null;
    }
  }
}
