/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.model.hydrology.internal.preprocessing.timeseries;

import java.io.PrintWriter;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.kalypso.model.hydrology.internal.NATimeSettings;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.request.IRequest;
import org.kalypso.ogc.sensor.timeseries.AxisUtils;

/**
 * @author doemming
 */
public class Ext2Writer
{
  private final DateFormat m_dateFormat;

  public Ext2Writer( )
  {
    m_dateFormat = NATimeSettings.getInstance().getTimeZonedDateFormat( new SimpleDateFormat( "dd MM yyyy HH " ) ); //$NON-NLS-1$
  }

  public void write( final IObservation observation, final IRequest request, final String axisType, final PrintWriter writer ) throws SensorException
  {
    final IAxis[] axes = observation.getAxes();
    final IAxis valueAxis = AxisUtils.findAxis( axes, axisType );
    final IAxis dateAxis = AxisUtils.findDateAxis( axes );

    final ITupleModel values = observation.getValues( request );

    writer.println( "EX2" ); // header //$NON-NLS-1$

    for( int i = 0; i < values.size(); i++ )
    {
      final Date date = (Date) values.get( i, dateAxis );
      final Object value = values.get( i, valueAxis );

      final String formattedDate = m_dateFormat.format( date );
      if( formattedDate.trim().endsWith( "11" ) ) //$NON-NLS-1$
      {
        System.out.println();
      }

      writer.append( formattedDate );
      writer.println( value );
    }
  }
}
