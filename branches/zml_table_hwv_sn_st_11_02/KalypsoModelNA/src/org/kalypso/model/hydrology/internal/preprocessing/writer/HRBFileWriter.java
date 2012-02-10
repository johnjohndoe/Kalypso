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
import java.util.logging.Logger;

import org.kalypso.model.hydrology.binding.model.Node;
import org.kalypso.model.hydrology.binding.model.StorageChannel;
import org.kalypso.model.hydrology.internal.IDManager;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.zml.obslink.TimeseriesLinkType;

/**
 * @author Dejan Antanaskovic
 */
public class HRBFileWriter extends AbstractCoreFileWriter
{
  private final StorageChannel[] m_storageChannels;

  // maps timeseries link path to its ascii file name
  private final Map<String, String> m_timseriesMap = new HashMap<String, String>();

  private final File m_klimaDir;

  private final IDManager m_idManager;

  public HRBFileWriter( final StorageChannel[] storageChannels, final IDManager idManager, final File klimaDir, final Logger logger )
  {
    super( logger );

    m_storageChannels = storageChannels;
    m_klimaDir = klimaDir;
    m_idManager = idManager;
  }

  /**
   * @see org.kalypso.model.hydrology.internal.preprocessing.AbstractCoreFileWriter#write(java.io.PrintWriter)
   */
  @Override
  protected void writeContent( final PrintWriter writer ) throws Exception
  {
    for( final StorageChannel channel : m_storageChannels )
    {
      final String asciiTS = processTimeserieFile( channel );
      if( asciiTS != null )
      {
        final int channelID = m_idManager.getAsciiID( channel );
        final Node overflowNode = channel.getOverflowNode();
        final Node overflowNode2 = channel.getOverflowNode2();
        final Node overflowNode3 = channel.getOverflowNode3();
        final int overflowNode1ID = overflowNode == null ? 0 : m_idManager.getAsciiID( overflowNode );
        final int overflowNode2ID = overflowNode2 == null ? 0 : m_idManager.getAsciiID( overflowNode2 );
        final int overflowNode3ID = overflowNode3 == null ? 0 : m_idManager.getAsciiID( overflowNode3 );
        writer.format( Locale.ENGLISH, "SPEICHER %7d %7d %7d %7d  %s\n", channelID, overflowNode1ID, overflowNode2ID, overflowNode3ID, asciiTS ); //$NON-NLS-1$
        writer.format( Locale.ENGLISH, "Fakt_SeeV %.2f\n", channel.getSeaEvaporationFactor() ); //$NON-NLS-1$
        writer.format( Locale.ENGLISH, "text;text\n" ); //$NON-NLS-1$

        final IObservation wvqObservation = channel.getWVQObservation();
        if( wvqObservation == null )
        {
          // TODO check if this case is valid; throw an exception otherwise
          writer.format( Locale.ENGLISH, "%s %10.6f %9.6f %9.6f %d\n", channel.getName(), 0.0, 0.0, 0.0, 0 ); //$NON-NLS-1$
        }
        else
        {
          final WVQInfo wvqInfo = getWVQInfo( wvqObservation );
          // to discuss: shall we use the user-defined max and min, or those calculated from wvqInfo?
          final double initialCapacity = channel.getInitialCapacity() / 1000000.0;
          final double volumeMax = channel.getVolumeMax() / 1000000.0; // wvqInfo.getMaxVolume()
          final double volumeMin = channel.getVolumeMin() / 1000000.0; // wvqInfo.getMinVolume()
          writer.format( Locale.ENGLISH, "%-10s%10.6f%10.6f%10.6f %d\n", channel.getName().length() > 10 ? channel.getName().substring( 0, 10 ) : channel.getName(), initialCapacity, volumeMax, volumeMin, wvqInfo.getNumberOfEntries() ); //$NON-NLS-1$
          writer.write( wvqInfo.getFormattedObservation() );
        }
        writer.format( Locale.ENGLISH, "ENDE\n" ); //$NON-NLS-1$
      }
    }
  }

  /**
   * Returns the ASCII timeserie file name (without path), or null if no link is provided or writing error occurs
   */
  private String processTimeserieFile( final StorageChannel channel )
  {
    final TimeseriesLinkType seaEvaporationTimeseriesLink = channel.getSeaEvaporationTimeseriesLink();
    if( seaEvaporationTimeseriesLink == null )
      return null;
    final String zmlHref = seaEvaporationTimeseriesLink.getHref();
    if( !m_timseriesMap.containsKey( zmlHref ) )
    {
      final int asciiID = m_idManager.getAsciiID( channel );

      final String name = String.format( "SE_%d.%s", asciiID, ITimeseriesConstants.TYPE_EVAPORATION );//$NON-NLS-1$

      final File asciiTimeseriesFile = new File( m_klimaDir, name );
      try
      {
        final URL context = channel.getWorkspace().getContext();
        // This is not a bug, sea evaporation is the exception (needs GRAP format)!
        TsFileWriter.writeGrapTimeseries( asciiTimeseriesFile, seaEvaporationTimeseriesLink, context, ITimeseriesConstants.TYPE_EVAPORATION, null );
      }
      catch( final Exception e )
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
        return null;
      }
      m_timseriesMap.put( zmlHref, name );
    }
    return m_timseriesMap.get( zmlHref );
  }

  private WVQInfo getWVQInfo( final IObservation observation ) throws SensorException
  {
    final StringBuffer buffer = new StringBuffer();
    final IAxis[] axisList = observation.getAxes();
    final IAxis axis_NN = ObservationUtilities.findAxisByType( axisList, ITimeseriesConstants.TYPE_NORMNULL );
    final IAxis axis_V = ObservationUtilities.findAxisByType( axisList, ITimeseriesConstants.TYPE_VOLUME );
    final IAxis axis_Q = ObservationUtilities.findAxisByType( axisList, ITimeseriesConstants.TYPE_RUNOFF );

    // these are optional
    final IAxis axis_Q2 = ObservationUtilities.findAxisByTypeNoEx( axisList, ITimeseriesConstants.TYPE_RUNOFF_Q2 );
    final IAxis axis_Q3 = ObservationUtilities.findAxisByTypeNoEx( axisList, ITimeseriesConstants.TYPE_RUNOFF_Q3 );

    final ITupleModel values = observation.getValues( null );
    double vMax = -Double.MAX_VALUE;
    double vMin = Double.MAX_VALUE;
    final int count = values.size();
    if( count == 0 )
    {
      vMax = 0.0;
      vMin = 0.0;
    }
    else
    {
      for( int row = 0; row < count; row++ )
      {
        final double w = (Double) values.get( row, axis_NN );
        final double v = ((Double) values.get( row, axis_V )) / 1000000;
        final Double q = (Double) values.get( row, axis_Q );
        final Double q2 = axis_Q2 == null ? 0.0 : (Double) values.get( row, axis_Q2 );
        final Double q3 = axis_Q3 == null ? 0.0 : (Double) values.get( row, axis_Q3 );
        buffer.append( String.format( Locale.ENGLISH, "%12.2f %16.6f %13.3f %13.3f %13.3f\n", w, v, q == null ? 0.0 : q, q2 == null ? 0.0 : q2, q3 == null ? 0.0 : q3 ) );
        if( v > vMax )
          vMax = v;
        if( v < vMin )
          vMin = v;
      }
    }
    return new WVQInfo( buffer.toString(), vMax, vMin, count );
  }

}
