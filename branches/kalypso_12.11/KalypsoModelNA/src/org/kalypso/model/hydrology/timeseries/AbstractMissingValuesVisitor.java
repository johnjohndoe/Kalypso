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
package org.kalypso.model.hydrology.timeseries;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.core.runtime.Assert;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.adapter.AbstractObservationImporter;
import org.kalypso.ogc.sensor.timeseries.AxisUtils;
import org.kalypso.ogc.sensor.timeseries.datasource.DataSourceHandler;
import org.kalypso.ogc.sensor.visitor.IObservationValueContainer;
import org.kalypso.repository.IDataSourceItem;

/**
 * @author Dirk Kuch
 */
public abstract class AbstractMissingValuesVisitor
{
  private final double m_minValue;

  private final double m_maxValue;

  /**
   * @param minValue
   *          All values smaller than this value are considered missing values. Use Double#NaN to the check.
   * @param maxValue
   *          All values bigger than this value are considered missing values. Use Double#NaN to the check.
   */
  public AbstractMissingValuesVisitor( final double minValue, final double maxValue )
  {
    m_minValue = minValue;
    m_maxValue = maxValue;
  }

  protected Integer getDataSourceIndex( final IObservationValueContainer container, final IAxis dataSourceAxis ) throws SensorException
  {
    final DataSourceHandler handler = new DataSourceHandler( container.getMetaData() );
    final Number sourceIndex = (Number) container.get( dataSourceAxis );

    final String dataSourceIdentifier = handler.getDataSourceIdentifier( sourceIndex.intValue() );
    if( dataSourceIdentifier.contains( AbstractObservationImporter.MISSING_VALUE_POSTFIX ) )
      return sourceIndex.intValue();

    if( dataSourceIdentifier.startsWith( "source://native.observation." ) ) //$NON-NLS-1$
    {
      final String missing = dataSourceIdentifier + AbstractObservationImporter.MISSING_VALUE_POSTFIX;

      return handler.addDataSource( missing, missing );
    }

    final String unknownAndMissing = IDataSourceItem.SOURCE_MISSING + AbstractObservationImporter.MISSING_VALUE_POSTFIX;

    return handler.addDataSource( unknownAndMissing, unknownAndMissing );
  }

  protected boolean isMissingValue( final IObservationValueContainer container ) throws SensorException
  {
    final IAxis valueAxis = findValueAxis( container.getAxes() );
    final String dataSource = getDataSource( container, valueAxis );
    if( dataSource.contains( AbstractObservationImporter.MISSING_VALUE_POSTFIX ) )
      return true;

    final Number valueNumber = (Number) container.get( valueAxis );

    if( Objects.isNull( valueNumber ) )
      return true;

    final double value = valueNumber.doubleValue();
    if( Double.isNaN( value ) )
      return true;

    // Sime special known values that are always missing values
    if( Math.abs( -999.0 - value ) < 0.1 )
      return true;
    if( Math.abs( -888.0 - value ) < 0.1 )
      return true;
    if( Math.abs( -777.0 - value ) < 0.1 )
      return true;

    if( !Double.isNaN( m_minValue ) && value < m_minValue )
      return true;

    if( !Double.isNaN( m_maxValue ) && value > m_maxValue )
      return true;

    return false;
  }

  private String getDataSource( final IObservationValueContainer container, final IAxis valueAxis ) throws SensorException
  {
    final IAxis dataSourceAxis = AxisUtils.findDataSourceAxis( container.getAxes(), valueAxis );
    if( Objects.isNull( dataSourceAxis ) )
      return IDataSourceItem.SOURCE_UNKNOWN;

    final DataSourceHandler handler = new DataSourceHandler( container.getMetaData() );
    final Number sourceIndex = (Number) container.get( dataSourceAxis );

    return handler.getDataSourceIdentifier( sourceIndex.intValue() );
  }

  protected IAxis findValueAxis( final IAxis[] axes )
  {
    final IAxis[] valueAxes = AxisUtils.findValueAxes( axes, true );
    Assert.isTrue( ArrayUtils.getLength( valueAxes ) == 1 );

    return valueAxes[0];
  }
}
