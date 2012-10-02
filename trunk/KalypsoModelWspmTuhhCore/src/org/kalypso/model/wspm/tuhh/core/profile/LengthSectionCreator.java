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
package org.kalypso.model.wspm.tuhh.core.profile;

import java.math.BigDecimal;
import java.math.RoundingMode;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfilePointMarker;
import org.kalypso.model.wspm.core.profil.IProfileTransaction;
import org.kalypso.model.wspm.core.profil.util.ProfileUtil;
import org.kalypso.model.wspm.core.profil.visitors.ProfileVisitors;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingBruecke;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingWehr;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.ICulvertBuilding;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.IProfileBuilding;
import org.kalypso.model.wspm.tuhh.core.util.river.line.WspmSohlpunkte;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.Observation;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

/**
 * @author Gernot Belger
 */
public class LengthSectionCreator
{
  private final IProfile[] m_profiles;

  public LengthSectionCreator( final IProfile[] profiles )
  {
    m_profiles = profiles;
  }

  public final IObservation<TupleResult> toLengthSection( )
  {
    final TupleResult lsResult = new TupleResult();
    lsResult.addComponent( ProfileUtil.getFeatureComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_STATION ) );
    lsResult.addComponent( ProfileUtil.getFeatureComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_TYPE ) );
    lsResult.addComponent( ProfileUtil.getFeatureComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_GROUND ) );
    lsResult.addComponent( ProfileUtil.getFeatureComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_BOE_LI ) );
    lsResult.addComponent( ProfileUtil.getFeatureComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_BOE_RE ) );
    lsResult.addComponent( ProfileUtil.getFeatureComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_WEIR_OK ) );
    lsResult.addComponent( ProfileUtil.getFeatureComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_BRIDGE_OK ) );
    lsResult.addComponent( ProfileUtil.getFeatureComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_BRIDGE_UK ) );
    lsResult.addComponent( ProfileUtil.getFeatureComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_BRIDGE_WIDTH ) );
    lsResult.addComponent( ProfileUtil.getFeatureComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_ROHR_DN ) );
    lsResult.addComponent( ProfileUtil.getFeatureComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_TEXT ) );
    lsResult.addComponent( ProfileUtil.getFeatureComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_H_BV ) );

    final Double precision = lsResult.getComponent( 7 ).getPrecision();

    for( final IProfile profil : m_profiles )
    {
      addProfile( profil, lsResult, precision );
    }

    return new Observation<>( "LengthSectionResult", Messages.getString( "LengthSectionCreator_0" ), lsResult ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  private void addProfile( final IProfile profil, final TupleResult lsResult, final Double precision )
  {
    // final IComponent compHeight = profil.getPointPropertyFor( IWspmConstants.POINT_PROPERTY_HOEHE );
    final int indexHeight = profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_HOEHE );

    final IRecord station = lsResult.createRecord();

    // FIXME:
    // final String desc = profil.getDescription();
    final String desc = profil.getName();

    station.setValue( 10, StringUtils.isBlank( desc ) ? null : desc ); //$NON-NLS-1$
    station.setValue( 0, valueToBigDecimal( profil.getStation() ) );// Station
    // Kennung
    // TODO: IWspmConstants.LENGTH_SECTION_PROPERTY_TYPE
    final Double minHeightValue = ProfileVisitors.findLowestPoint( profil ).getBreite();
    final BigDecimal minHeightDecimal = minHeightValue == null ? null : ProfileUtil.stationToBigDecimal( minHeightValue );
    station.setValue( 2, minHeightDecimal ); // Ground
    final IProfilePointMarker[] mbv = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_BORDVOLL );
    final IProfilePointMarker[] mtf = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );

    profil.doTransaction( new IProfileTransaction()
    {
      @Override
      public IStatus execute( final IProfile profile )
      {
        // Devider
        if( mbv.length == 2 )
        {
          final BigDecimal boLi = valueToBigDecimal( mbv[0].getPoint().getValue( indexHeight ) );
          final BigDecimal boRe = valueToBigDecimal( mbv[1].getPoint().getValue( indexHeight ) );
          station.setValue( 3, boLi ); // BOE_LI
          station.setValue( 4, boRe ); // BOE_RE
          station.setValue( 11, valueToBigDecimal( Math.min( boLi.doubleValue(), boRe.doubleValue() ) ) ); // H_BV
        }
        else if( mtf.length == 2 )
        {
          final BigDecimal boLi = valueToBigDecimal( mtf[0].getPoint().getValue( indexHeight ) );
          final BigDecimal boRe = valueToBigDecimal( mtf[1].getPoint().getValue( indexHeight ) );
          station.setValue( 3, boLi );// BOE_LI
          station.setValue( 4, boRe ); // BOE_RE
          station.setValue( 11, valueToBigDecimal( Math.min( boLi.doubleValue(), boRe.doubleValue() ) ) ); // H_BV
        }

        // Profile Objects
        final IProfileBuilding building = WspmSohlpunkte.getBuilding( profil, IProfileBuilding.class );

        // FIXME: calculation of uk/ok/ok-weir values still not satisfactory...we probably need to calculate mean values
        if( building instanceof BuildingWehr )
        {
          final Double sectionMaxValue = getMaxValueFor( profil, indexHeight, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR, precision );
          final BigDecimal sectionMaxDecimal = valueToBigDecimal( sectionMaxValue );
          station.setValue( 5, sectionMaxDecimal ); // BridgeOK
        }
        else if( building instanceof BuildingBruecke )
        {
          final BuildingBruecke brueckeBuilding = (BuildingBruecke)building;

          final Double buildingWidth = brueckeBuilding.getBreite();
          final Double ukValue = getMaxValueFor( profil, indexHeight, IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE, precision );
          final Double okValue = getMinValueFor( profil, indexHeight, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE, precision );

          station.setValue( 7, valueToBigDecimal( ukValue ) ); // BridgeUK
          station.setValue( 6, valueToBigDecimal( okValue ) ); // BridgeOK
          station.setValue( 8, valueToBigDecimal( buildingWidth ) ); // BridgeWidth
        }
        else if( building instanceof ICulvertBuilding )
        {
          final Double buildingWidth = ((ICulvertBuilding)building).getBreite();
          station.setValue( 9, valueToBigDecimal( buildingWidth ) ); // ROHR_DN
        }

        return Status.OK_STATUS;
      }
    } );

    lsResult.add( station );
  }

  protected static Double getMaxValueFor( final IProfile profil, final int indexHeight, final String component, final double precision )
  {
    final Double[] values = ProfileUtil.getInterpolatedValues( profil, component );
    final IRecord[] points = profil.getPoints();

    double maxValue = Double.NEGATIVE_INFINITY;

    for( int i = 0; i < values.length; i++ )
    {
      final IRecord point = points[i];
      final Double value = values[i];

      final Object height = point.getValue( indexHeight );
      if( value != null && height instanceof Number )
      {
        final double doubleHeight = ((Number)height).doubleValue();

        /* Only consider points that are NOT same as height */
        if( Math.abs( value - doubleHeight ) > precision )
        {
          maxValue = Math.max( maxValue, value );
        }
      }
    }

    if( Double.isInfinite( maxValue ) )
      return Double.NaN;

    return maxValue;
  }

  protected static Double getMinValueFor( final IProfile profil, final int indexHeight, final String component, final double precision )
  {
    final Double[] values = ProfileUtil.getInterpolatedValues( profil, component );
    final IRecord[] points = profil.getPoints();

    double minValue = Double.POSITIVE_INFINITY;

    for( int i = 0; i < values.length; i++ )
    {
      final IRecord point = points[i];
      final Double value = values[i];
      final Object height = point.getValue( indexHeight );
      if( value != null && height instanceof Number )
      {
        final double doubleHeight = ((Number)height).doubleValue();

        /* Only consider points that are NOT same as height */
        if( Math.abs( value - doubleHeight ) > precision )
        {
          minValue = Math.min( minValue, value );
        }
      }
    }

    if( Double.isInfinite( minValue ) )
      return Double.NaN;

    return minValue;
  }

  protected static BigDecimal valueToBigDecimal( final Object value )
  {
    if( value instanceof BigDecimal )
      return (BigDecimal)value;

    if( value instanceof Number )
    {
      final double val = ((Number)value).doubleValue();
      if( Double.isNaN( val ) )
        return null;

      return new BigDecimal( val ).setScale( IProfileFeature.STATION_SCALE, RoundingMode.HALF_UP );
    }
    else
      return null;
  }

}
