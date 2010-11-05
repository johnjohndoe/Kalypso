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
package org.kalypso.convert.namodel.manager;

import java.io.File;
import java.io.PrintWriter;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.logging.Logger;

import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.convert.namodel.net.NetElement;
import org.kalypso.model.hydrology.binding.model.StorageChannel;
import org.kalypso.model.hydrology.internal.preprocessing.AbstractCoreFileWriter;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.zml.obslink.TimeseriesLinkType;

/**
 * @author antanas
 */
public class HRBFileWriter extends AbstractCoreFileWriter
{
  private final StorageChannel[] m_storageChannels;

  // maps timeseries link path to its ascii file name
  private final Map<String, String> m_timseriesMap = new HashMap<String, String>();

  private final NAConfiguration m_conf;

  private class WVQInfo
  {
    private final String m_formattedObservation;

    private final double m_vMax;

    private final double m_vMin;

    private final int m_numberOfEntries;

    protected WVQInfo( final String formattedObservation, final double vMax, final double vMin, final int numberOfEntries )
    {
      m_formattedObservation = formattedObservation;
      m_vMax = vMax;
      m_vMin = vMin;
      m_numberOfEntries = numberOfEntries;
    }

    protected String getFormattedObservation( )
    {
      return m_formattedObservation;
    }

    protected double getMaxVolume( )
    {
      return m_vMax;
    }

    protected double getMinVolume( )
    {
      return m_vMin;
    }

    protected int getNumberOfEntries( )
    {
      return m_numberOfEntries;
    }
  }

  public HRBFileWriter( final StorageChannel[] storageChannels, final NAConfiguration conf, final Logger logger )
  {
    super( conf.getHRBFile(), logger );
    m_storageChannels = storageChannels;
    m_conf = conf;
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
        final IDManager idManager = m_conf.getIdManager();
        final int channelID = idManager.getAsciiID( channel );
        final int overflowNode1ID = idManager.getAsciiID( channel.getOverflowNode() );
        final int overflowNode2ID = idManager.getAsciiID( channel.getOverflowNode2() );
        final int overflowNode3ID = idManager.getAsciiID( channel.getOverflowNode3() );
        writer.format( Locale.ENGLISH, "SPEICHER %7d %7d %7d %7d %s\n", channelID, overflowNode1ID, overflowNode2ID, overflowNode3ID, asciiTS ); //$NON-NLS-1$
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
          writer.format( Locale.ENGLISH, "%s %10.6f %9.6f %9.6f %d\n", channel.getName(), 0.0, wvqInfo.getMaxVolume(), wvqInfo.getMinVolume(), wvqInfo.getNumberOfEntries() ); //$NON-NLS-1$
          writer.write( wvqInfo.getFormattedObservation() );
        }
      }
    }
    writer.format( Locale.ENGLISH, "ENDE\n" ); //$NON-NLS-1$
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
      final int asciiID = m_conf.getIdManager().getAsciiID( channel );
      final String name = String.format( "SE_%d.%s", asciiID, ITimeseriesConstants.TYPE_EVAPORATION );//$NON-NLS-1$
      final File klimaDir = new File( m_conf.getAsciiBaseDir(), "klima.dat" );//$NON-NLS-1$
      final File asciiTimeseriesFile = new File( klimaDir, name );
      try
      {
        NetElement.writeTimeseries( asciiTimeseriesFile, seaEvaporationTimeseriesLink, m_conf.getZMLContext(), ITimeseriesConstants.TYPE_EVAPORATION, null, null, null, null );
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
    final IAxis[] axisList = observation.getAxisList();
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
