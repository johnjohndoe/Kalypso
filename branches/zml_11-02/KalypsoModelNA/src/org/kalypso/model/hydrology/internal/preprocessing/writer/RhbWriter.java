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
import java.util.Locale;
import java.util.logging.Logger;

import org.kalypso.model.hydrology.binding.model.Channel;
import org.kalypso.model.hydrology.binding.model.Node;
import org.kalypso.model.hydrology.binding.model.StorageChannel;
import org.kalypso.model.hydrology.internal.IDManager;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.model.hydrology.internal.preprocessing.net.NetElement;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.simulation.core.SimulationException;

/**
 * @author doemming
 */
public class RhbWriter extends AbstractCoreFileWriter
{
  private final IDManager m_idManager;

  private final NetElement[] m_channels;

  public RhbWriter( final IDManager idManager, final NetElement[] channels, final Logger logger )
  {
    super( logger );

    m_idManager = idManager;
    m_channels = channels;
  }

  /**
   * @see org.kalypso.model.hydrology.internal.preprocessing.writer.AbstractWriter#writeContent(java.io.PrintWriter)
   */
  @Override
  protected void writeContent( final PrintWriter writer ) throws Exception
  {
    for( final NetElement element : m_channels )
      writeFeature( element.getChannel(), writer );
  }

  // FIXME: better error handling!
  private void writeFeature( final Channel channel, final PrintWriter writer ) throws SensorException, SimulationException
  {
    if( channel instanceof StorageChannel ) //$NON-NLS-1$
    {
      final StorageChannel storageChannel = (StorageChannel) channel;

      final int channelID = m_idManager.getAsciiID( channel );
      final Node downstreamNode = channel.getDownstreamNode();

      final int downstreamNodeID = m_idManager.getAsciiID( downstreamNode );
      final int overflowNodeID = getOverflowNodeID( channel, downstreamNode );

      // (txt,a8)(inum,i8)(iknot,i8)(c,f6.2-dummy)
      // RHB 5-7
      writer.format( Locale.US, "SPEICHER%8d%8d  0.00\n", channelID, overflowNodeID ); //$NON-NLS-1$//$NON-NLS-2$

      // (itext,a80)
      // RHB 8
      writer.format( Locale.US, "%-80s\n", channel.getName() ); //$NON-NLS-1$

      // (lfs,i4)_(nams,a10)(sv,f10.6)(vmax,f10.6)(vmin,f10.6)(jev,i4)(itxts,a10)
      // RHB 9-10

      final double sv = storageChannel.getInitialCapacity() / 1000000;
      final Double vmax = storageChannel.getVolumeMax() / 1000000;
      final Double vmin = storageChannel.getVolumeMin() / 1000000;

      writer.format( Locale.US, "%4d  FUNKTION %9.6f %9.6f %9.6f", downstreamNodeID, sv, vmax, vmin ); //$NON-NLS-1$

      writeWVQ( storageChannel, writer );

      // Kommentar Ende Speicher
      // RHB 12
      writer.append( "ENDE\n" ); //$NON-NLS-1$
    }
  }

  private int getOverflowNodeID( final Channel channel, final Node downstreamNode )
  {
    // Ueberlaufknoten optional
    final Node overflowNode = ((StorageChannel) channel).getOverflowNode();
    final int overflowNodeID;
    if( overflowNode == null || overflowNode == downstreamNode )
      overflowNodeID = 0;
    else
      overflowNodeID = m_idManager.getAsciiID( overflowNode );
    return overflowNodeID;
  }

  /**
   * @param observation
   * @param rhbBuffer
   * @throws SensorException
   */
  private void writeWVQ( final StorageChannel storageChannel, final PrintWriter writer ) throws SensorException, SimulationException
  {
    final String channelName = storageChannel.getName();
    final IObservation observation = storageChannel.getWVQObservation();
    if( observation == null )
      throw new SimulationException( Messages.getString( "org.kalypso.convert.namodel.manager.ChannelManager.2", channelName ) ); //$NON-NLS-1$

    final ITupleModel values = observation.getValues( null );
    final int size = values.size();
    if( size > 24 )
      throw new SimulationException( Messages.getString( "org.kalypso.convert.namodel.manager.ChannelManager.33", channelName ) );

    writer.format( Locale.US, "%4d\n", size ); //$NON-NLS-1$


    final int count = values.size();

    final IAxis[] axisList = observation.getAxes();
    final IAxis waterTableAxis = ObservationUtilities.findAxisByType( axisList, ITimeseriesConstants.TYPE_NORMNULL );
    final IAxis volumeAxis = ObservationUtilities.findAxisByType( axisList, ITimeseriesConstants.TYPE_VOLUME );
    final IAxis dischargeAxis = ObservationUtilities.findAxisByType( axisList, ITimeseriesConstants.TYPE_RUNOFF );
    for( int row = 0; row < count; row++ )
    {
      final Double w = (Double) values.get( row, waterTableAxis );
      final Double v = ((Double) values.get( row, volumeAxis )) / 1000000;
      final Double q = (Double) values.get( row, dischargeAxis );
      // ____(hv,f8.2)________(vs,f9.6)______(qd,f8.3)
      writer.format( Locale.US, "    %8.2f        %9.6f      %8.3f\n", w, v, q ); //$NON-NLS-1$ 
    }
  }
}