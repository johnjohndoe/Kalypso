/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.model.hydrology.internal.preprocessing.writer;

import java.io.PrintWriter;
import java.util.logging.Logger;

import org.kalypso.contribs.java.util.FortranFormatHelper;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.internal.IDManager;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;

/**
 * @author doemming
 */
public class ZftWriter extends AbstractCoreFileWriter
{
  public static final String STD_TEMP_FILENAME = "std.tmp"; //$NON-NLS-1$

  public static final String STD_VERD_FILENAME = "std.ver"; //$NON-NLS-1$

  private final Catchment[] m_catchments;

  private final IDManager m_idManager;

  public ZftWriter( final IDManager idManager, final Logger logger, final Catchment[] catchments )
  {
    super( logger );

    m_idManager = idManager;
    m_catchments = catchments;
  }

  /**
   * @see org.kalypso.model.hydrology.internal.preprocessing.writer.AbstractWriter#writeContent(java.io.PrintWriter)
   */
  @Override
  protected void writeContent( final PrintWriter writer ) throws Exception
  {
    for( final Catchment catchment : m_catchments )
      writeCatchment( writer, catchment );
  }

  private void writeCatchment( final PrintWriter zftBuffer, final Catchment catchment ) throws Exception
  {
    final int catchmentID = m_idManager.getAsciiID( catchment );

    // Zeitflächenfunktion
    final IObservation zftProp = catchment.getZft();
    if( zftProp != null )
      writeZML( zftProp, catchmentID, zftBuffer );
  }

  /**
   * @param observation
   * @param asciiID
   * @param zftBuffer
   * @throws SensorException
   */
  private void writeZML( final IObservation observation, final int asciiID, final PrintWriter zftBuffer ) throws SensorException
  {
    zftBuffer.append( FortranFormatHelper.printf( asciiID, "*" ) + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$

    final IAxis[] axisList = observation.getAxes();
    final IAxis hoursAxis = ObservationUtilities.findAxisByType( axisList, ITimeseriesConstants.TYPE_HOURS );
    final IAxis normAreaAxis = ObservationUtilities.findAxisByType( axisList, ITimeseriesConstants.TYPE_NORM );
    final ITupleModel values = observation.getValues( null );
    final int count = values.size();
    final double t0 = ((Double) values.get( 0, hoursAxis )).doubleValue();
    final double t1 = ((Double) values.get( 1, hoursAxis )).doubleValue();
    final double dt = t1 - t0;
    zftBuffer.append( FortranFormatHelper.printf( count, "*" ) + " " + FortranFormatHelper.printf( dt, "*" ) + " 2\n" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    for( int row = 0; row < count; row++ )
    {
      final Double hoursValue = (Double) values.get( row, hoursAxis );
      final Double normAreaValue = (Double) values.get( row, normAreaAxis );
      zftBuffer.append( FortranFormatHelper.printf( hoursValue, "*" ) + " " + FortranFormatHelper.printf( normAreaValue, "*" ) + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    }
  }
}