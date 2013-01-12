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
package org.kalypso.model.hydrology.internal.preprocessing.writer;

import java.io.File;
import java.io.PrintWriter;
import java.net.URL;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.model.hydrology.binding.model.channels.Channel;
import org.kalypso.model.hydrology.binding.model.channels.StorageChannel;
import org.kalypso.model.hydrology.binding.model.nodes.INode;
import org.kalypso.model.hydrology.gml.ZmlWQVInlineTypeHandler;
import org.kalypso.model.hydrology.internal.IDManager;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.model.hydrology.internal.preprocessing.NAPreprocessorException;
import org.kalypso.model.hydrology.internal.preprocessing.preparation.NetElement;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ogc.sensor.util.ZmlLink;

/**
 * @author Dejan Antanaskovic
 */
class HRBFileWriter extends AbstractCoreFileWriter
{
  private static final double HECTO_CUBIC_METER = 1000000.0;

  // maps timeseries link path to its ascii file name
  private final Map<String, String> m_timseriesMap = new HashMap<>();

  private final File m_klimaDir;

  private final IDManager m_idManager;

  private final NetElement[] m_channels;

  public HRBFileWriter( final NetElement[] channels, final IDManager idManager, final File klimaDir )
  {
    m_channels = channels;
    m_klimaDir = klimaDir;
    m_idManager = idManager;
  }

  @Override
  protected IStatus writeContent( final PrintWriter writer ) throws NAPreprocessorException
  {
    for( final NetElement element : m_channels )
    {
      final Channel channel = element.getChannel();
      if( channel instanceof StorageChannel )
        writeChannel( writer, (StorageChannel)channel );
    }

    return Status.OK_STATUS;
  }

  private void writeChannel( final PrintWriter writer, final StorageChannel channel ) throws NAPreprocessorException
  {
    final String asciiTS = processTimeserieFile( channel );
    final int channelID = m_idManager.getAsciiID( channel );

    final int overflowNode1ID = getNodeID( channel.getOverflowNode() );
    final int overflowNode2ID = getNodeID( channel.getOutletNode1() );
    final int overflowNode3ID = getNodeID( channel.getOutletNode2() );

    writer.format( Locale.ENGLISH, "SPEICHER %7d %7d %7d %7d  %s\n", channelID, overflowNode1ID, overflowNode2ID, overflowNode3ID, asciiTS ); //$NON-NLS-1$
    writer.format( Locale.ENGLISH, "Fakt_SeeV %.2f\n", channel.getSeaEvaporationFactor() ); //$NON-NLS-1$
    writer.format( Locale.ENGLISH, "text;text\n" ); //$NON-NLS-1$

    final IObservation wvqObservation = channel.getWVQObservation();

    final String channelName = channel.getName();
    // Strange but correct: print out name as reminder what this element is
    final String channelShortName = channelName.length() > 10 ? channelName.substring( 0, 10 ) : channelName;

    if( wvqObservation == null )
    {
      final String message = String.format( Messages.getString( "HRBFileWriter_0" ), channelName ); //$NON-NLS-1$
      throw new NAPreprocessorException( message );
    }

    final WVQInfo wvqInfo = getWVQInfo( wvqObservation, channelName );
    final double initialCapacity = channel.getInitialCapacity() / HECTO_CUBIC_METER;
    final double volumeMax = channel.getVolumeMax() / HECTO_CUBIC_METER;
    final double volumeMin = channel.getVolumeMin() / HECTO_CUBIC_METER;
    writer.format( Locale.ENGLISH, "%-10s%10.6f%10.6f%10.6f %d\n", channelShortName, initialCapacity, volumeMax, volumeMin, wvqInfo.getNumberOfEntries() ); //$NON-NLS-1$
    writer.write( wvqInfo.getFormattedObservation() );
    writer.format( Locale.ENGLISH, "ENDE\n" ); //$NON-NLS-1$
  }

  private int getNodeID( final INode overflowNode1 )
  {
    if( overflowNode1 == null )
      return 0;

    return m_idManager.getAsciiID( overflowNode1 );
  }

  /**
   * Returns the ASCII timeserie file name (without path), or the empty if no link is provided or writing error occurs
   */
  private String processTimeserieFile( final StorageChannel channel ) throws NAPreprocessorException
  {
    try
    {
      final ZmlLink seaEvaporationTimeseriesLink = channel.getSeaEvaporationTimeseriesLink();
      if( !seaEvaporationTimeseriesLink.isLinkSet() )
        return StringUtils.EMPTY;

      final String zmlHref = seaEvaporationTimeseriesLink.getHref();
      if( StringUtils.isBlank( zmlHref ) )
        return StringUtils.EMPTY;

      if( !m_timseriesMap.containsKey( zmlHref ) )
      {
        final int asciiID = m_idManager.getAsciiID( channel );

        final String name = String.format( "SE_%d.%s", asciiID, ITimeseriesConstants.TYPE_EVAPORATION );//$NON-NLS-1$

        final File asciiTimeseriesFile = new File( m_klimaDir, name );
        final URL context = channel.getWorkspace().getContext();
        TsFileWriter.writeGrapTimeseries( asciiTimeseriesFile, seaEvaporationTimeseriesLink, context, ITimeseriesConstants.TYPE_EVAPORATION, null );
        m_timseriesMap.put( zmlHref, name );
      }

      return m_timseriesMap.get( zmlHref );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new NAPreprocessorException( Messages.getString( "HRBFileWriter_1" ), e ); //$NON-NLS-1$
    }
  }

  private WVQInfo getWVQInfo( final IObservation observation, final String channelName ) throws NAPreprocessorException
  {
    try
    {
      final StringBuilder buffer = new StringBuilder();
      final IAxis[] axisList = observation.getAxes();
      final IAxis axisNN = ObservationUtilities.findAxisByTypeNoEx( axisList, ITimeseriesConstants.TYPE_NORMNULL );
      final IAxis axisV = ObservationUtilities.findAxisByTypeNoEx( axisList, ITimeseriesConstants.TYPE_VOLUME );
      final IAxis axisQ = ObservationUtilities.findAxisByNameNoEx( axisList, ZmlWQVInlineTypeHandler.AXIS_NAME_ABFLUSS );

      // these are optional
      final IAxis axisQ2 = ObservationUtilities.findAxisByNameNoEx( axisList, ZmlWQVInlineTypeHandler.AXIS_NAME_ABGABE_1 );
      final IAxis axisQ3 = ObservationUtilities.findAxisByNameNoEx( axisList, ZmlWQVInlineTypeHandler.AXIS_NAME_ABGABE_2 );

      final ITupleModel values = observation.getValues( null );
      final int count = values.size();
      for( int row = 0; row < count; row++ )
      {
        final double w = getAsDouble( values, axisNN, row, Double.NaN );
        final double v = getAsDouble( values, axisV, row, Double.NaN ) / HECTO_CUBIC_METER;
        final double q = getAsDouble( values, axisQ, row, Double.NaN );
        final double q2 = getAsDouble( values, axisQ2, row, 0.0 );
        final double q3 = getAsDouble( values, axisQ3, row, 0.0 );
        buffer.append( String.format( Locale.ENGLISH, "%12.2f %16.6f %13.3f %13.3f %13.3f\n", w, v, q, q2, q3 ) ); //$NON-NLS-1$
      }

      return new WVQInfo( buffer.toString(), count );
    }
    catch( final SensorException e )
    {
      e.printStackTrace();
      throw new NAPreprocessorException( String.format( Messages.getString( "HRBFileWriter_2" ), channelName ) ); //$NON-NLS-1$
    }
  }

  /**
   * @defaultValue Returned if the given axis is null. If the defaultValue NaN, is is considered as required and an
   *               exception is thrown instead.
   */
  private double getAsDouble( final ITupleModel values, final IAxis axis, final int row, final double defaultValue ) throws SensorException
  {
    if( axis == null )
    {
      if( Double.isNaN( defaultValue ) )
      {
        final String message = String.format( Messages.getString( "HRBFileWriter_3" ) ); //$NON-NLS-1$
        throw new SensorException( message );
      }

      return defaultValue;
    }

    final Object value = values.get( row, axis );
    if( value instanceof Number )
      return ((Number)value).doubleValue();

    final String message = String.format( Messages.getString( "HRBFileWriter_4" ), axis.getName(), row ); //$NON-NLS-1$
    throw new SensorException( message );
  }
}