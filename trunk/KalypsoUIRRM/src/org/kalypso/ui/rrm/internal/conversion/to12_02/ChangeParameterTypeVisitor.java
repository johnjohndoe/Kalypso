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
package org.kalypso.ui.rrm.internal.conversion.to12_02;

import java.util.Date;
import java.util.LinkedHashMap;
import java.util.Map;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.TupleModelDataSet;
import org.kalypso.ogc.sensor.impl.DefaultAxis;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ogc.sensor.metadata.MetadataList;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.timeseries.AxisUtils;
import org.kalypso.ogc.sensor.timeseries.TimeseriesUtils;
import org.kalypso.ogc.sensor.timeseries.datasource.DataSourceHelper;
import org.kalypso.ogc.sensor.visitor.ITupleModelValueContainer;
import org.kalypso.ogc.sensor.visitor.ITupleModelVisitor;

/**
 * @author Dirk Kuch
 */
public class ChangeParameterTypeVisitor implements ITupleModelVisitor
{
  Map<Date, TupleModelDataSet> m_data = new LinkedHashMap<>();

  private final IAxis m_targetAxis;

  private final MetadataList m_metadata;

  public ChangeParameterTypeVisitor( final MetadataList metadata, final String targetType )
  {
    m_metadata = metadata;
    m_targetAxis = TimeseriesUtils.createDefaultAxis( targetType, true );
  }

  @Override
  public void visit( final ITupleModelValueContainer container ) throws SensorException
  {
    final IAxis sourceDateAxis = AxisUtils.findDateAxis( container.getAxes() );
    final IAxis sourceValueAxis = AxisUtils.findValueAxis( container.getAxes(), true );

    final Date date = (Date) container.get( sourceDateAxis );
    final TupleModelDataSet sourceDataSet = container.getDataSetFor( m_metadata, sourceValueAxis.getType() );
    final TupleModelDataSet targetDateSet = doConvert( sourceDataSet );

    m_data.put( date, targetDateSet );
  }

  private TupleModelDataSet doConvert( final TupleModelDataSet source )
  {
    return new TupleModelDataSet( m_targetAxis, source.getValue(), source.getStatus(), source.getSource() );
  }

  public Map<Date, TupleModelDataSet> getConverted( )
  {
    return m_data;
  }

  public IAxis[] getTargetAxis( )
  {
    if( m_data.isEmpty() )
      return new IAxis[] {};

    final Date ptr = m_data.keySet().iterator().next();
    final TupleModelDataSet dataset = m_data.get( ptr );

    final IAxis dateAxis = new DefaultAxis( TimeseriesUtils.getName( ITimeseriesConstants.TYPE_DATE ), ITimeseriesConstants.TYPE_DATE, TimeseriesUtils.getUnit( ITimeseriesConstants.TYPE_DATE ), TimeseriesUtils.getDataClass( ITimeseriesConstants.TYPE_DATE ), true );
    final IAxis valueAxis = dataset.getValueAxis();
    final IAxis statusAxis = KalypsoStatusUtils.createStatusAxisFor( valueAxis, true );
    final IAxis dataSourceAxis = DataSourceHelper.createSourceAxis( valueAxis, true );

    return new IAxis[] { dateAxis, valueAxis, statusAxis, dataSourceAxis };
  }

}
