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
package org.kalypso.model.hydrology.binding.model.channels.helper;

import java.util.Set;
import java.util.TreeSet;

import org.apache.commons.lang3.StringUtils;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.model.hydrology.gml.ZmlWQVInlineTypeHandler;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTupleModel;
import org.kalypso.ogc.sensor.metadata.MetadataList;

/**
 * @author Dirk Kuch
 */
public class WvqRelationshipObservationBuilder
{
  Set<WvqRelationshipRow> m_rows = new TreeSet<>( WvqRelationshipRow.COMPARATOR );

  private IAxis m_axisWaterLevel;

  private IAxis m_axisDischarge;

  private IAxis m_axisVolume;

  public void addRow( final double waterLevel, final double discharge, final double volumne )
  {
    m_rows.add( new WvqRelationshipRow( waterLevel, discharge, volumne ) );
  }

  public IObservation getObservation( )
  {
    final SimpleTupleModel model = new SimpleTupleModel( new IAxis[] { getWaterLevelAxis(), getDischargeAxis(), getVolumeAxis() } );
    for( final WvqRelationshipRow row : m_rows )
    {
      model.addTuple( new Object[] { row.getWaterLevel(), row.getDischarge(), row.getVolumne() } );
    }

    return new SimpleObservation( StringUtils.EMPTY, StringUtils.EMPTY, new MetadataList(), model );
  }

  private IAxis getVolumeAxis( )
  {
    if( Objects.isNull( m_axisVolume ) )
      m_axisVolume = ZmlWQVInlineTypeHandler.createVolumeAxis();
    return m_axisVolume;
  }

  private IAxis getDischargeAxis( )
  {
    if( Objects.isNull( m_axisDischarge ) )
      m_axisDischarge = ZmlWQVInlineTypeHandler.createRunoffAxis();

    return m_axisDischarge;
  }

  private IAxis getWaterLevelAxis( )
  {
    if( Objects.isNull( m_axisWaterLevel ) )
      m_axisWaterLevel = ZmlWQVInlineTypeHandler.createWaterlevelAxis();

    return m_axisWaterLevel;
  }
}
