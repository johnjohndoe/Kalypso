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
package org.kalypso.model.wspm.sobek.core.wizard.worker;

import java.io.StringReader;
import java.math.BigDecimal;
import java.util.Map;

import javax.xml.namespace.QName;

import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNodeLastfallCondition;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.model.wspm.sobek.core.wizard.pages.PageEditBoundaryConditionGeneral;
import org.kalypso.model.wspm.sobek.core.wizard.pages.PageEditBoundaryConditionTimeSeries;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQPair;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTable;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTableFactory;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTableSet;
import org.kalypso.ogc.sensor.zml.repository.ZmlObservationItem;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.xml.sax.InputSource;

/**
 * @author kuch
 */
public class ZmlWQRelationTimeSeriesProvider extends AbstractTimeSeriesProvider
{
  private ZmlObservationItem m_item;

  public ZmlWQRelationTimeSeriesProvider( final PageEditBoundaryConditionGeneral general, final PageEditBoundaryConditionTimeSeries timeSeries )
  {
    super( general, timeSeries );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.wizard.worker.ITimeSeriesProvider#fillTupleResult(org.kalypso.observation.result.TupleResult)
   */
  public void fillTupleResult( final TupleResult result )
  {
    try
    {
      final IObservation observation = (IObservation) m_item.getAdapter( IObservation.class );
      final String wqrelation = observation.getMetadataList().getProperty( TimeserieConstants.MD_WQTABLE );

      final StringReader reader = new StringReader( wqrelation );
      final WQTableSet set = WQTableFactory.parse( new InputSource( reader ) );
      reader.close();

      final WQTable[] tables = set.getTables();
      for( final WQTable table : tables )
      {
        final WQPair[] pairs = table.getPairs();
        for( final WQPair pair : pairs )
        {
          final IRecord record = result.createRecord();
          record.setValue( 0, BigDecimal.valueOf( pair.getW() ) ); // [0] is w
          record.setValue( 1, BigDecimal.valueOf( pair.getQ() ) ); // [1] is q

          result.add( record );
        }
      }

    }
    catch( final Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.wizard.worker.ITimeSeriesProvider#getBasicChanges()
   */
  @Override
  public Map<QName, Object> getBasicChanges( )
  {
    final Map<QName, Object> changes = super.getBasicChanges();

    /* set time series link */
    final TimeseriesLinkType lnkTimeSeries = new TimeseriesLinkType();

    m_item = getPageTS().getZmlObservationItem();
    final String identifier = m_item.getIdentifier();
    lnkTimeSeries.setHref( identifier );

    changes.put( ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_LNK_TIME_SERIES, lnkTimeSeries );

    /* type */
    changes.put( ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_TYPE, IBoundaryNodeLastfallCondition.BOUNDARY_CONDITION_TYPE.eZml.toGmlString() );

    return changes;
  }

}
