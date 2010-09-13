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
package org.kalypso.convert.namodel.manager;

import java.util.Arrays;

import org.kalypso.contribs.java.util.FortranFormatHelper;
import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.binding.model.Channel;
import org.kalypso.model.hydrology.binding.model.KMChannel;
import org.kalypso.model.hydrology.binding.model.KMParameter;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.binding.model.StorageChannel;
import org.kalypso.model.hydrology.binding.model.VirtualChannel;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author doemming
 */
public class ChannelManager
{
  public static final int VIRTUALCHANNEL = 0;

  private static final int KMCHANNEL = 1;

  private static final int STORAGECHANNEL = 2;

  private final NAConfiguration m_conf;

  private final ASCIIHelper m_asciiHelper;

  public ChannelManager( final NAConfiguration conf )
  {
    m_asciiHelper = new ASCIIHelper( getClass().getResource( "resources/formats/gerinne.txt" ) );
    m_conf = conf;
  }

  public void writeFile( final AsciiBuffer asciiBuffer, final GMLWorkspace workspace ) throws Exception
  {
    final NaModell naModel = (NaModell) workspace.getRootFeature();
    final IFeatureBindingCollection<Channel> channels = naModel.getChannels();
    final Channel[] allChannels = channels.toArray( new Channel[channels.size()] );

    /* Sort channels by type, so it is easier to read the .ger file */
    Arrays.sort( allChannels, new ChannelTypeComparator( m_conf.getIdManager() ) );

    for( final Channel channel : allChannels )
    {
      if( asciiBuffer.isFeatureMarkedForWrite( channel ) )
        writeFeature( asciiBuffer, channel, workspace );
    }
  }

  private void writeFeature( final AsciiBuffer asciiBuffer, final Channel channel, final GMLWorkspace workspace ) throws Exception
  {
    final IDManager idManager = m_conf.getIdManager();

    final StringBuffer channelBuffer = asciiBuffer.getChannelBuffer();
    final StringBuffer rhbBuffer = asciiBuffer.getRhbBuffer();

    channelBuffer.append( idManager.getAsciiID( channel ) + "\n" ); //$NON-NLS-1$

    if( channel instanceof VirtualChannel ) //$NON-NLS-1$
      channelBuffer.append( VIRTUALCHANNEL + "\n" ); //$NON-NLS-1$
    else if( channel instanceof KMChannel ) //$NON-NLS-1$
    {
      channelBuffer.append( KMCHANNEL + "\n" ); //$NON-NLS-1$

      final KMChannel kmChannel = (KMChannel) channel;
      final IFeatureBindingCollection<KMParameter> parameters = kmChannel.getParameters();
      for( final KMParameter kmParameter : parameters )
        channelBuffer.append( m_asciiHelper.toAscii( kmParameter, 3 ) + "\n" ); //$NON-NLS-1$
    }
    else if( channel instanceof StorageChannel ) //$NON-NLS-1$
    {
      channelBuffer.append( STORAGECHANNEL + "\n" ); //$NON-NLS-1$

      // (txt,a8)(inum,i8)(iknot,i8)(c,f6.2-dummy)
      // RHB 5-7

      rhbBuffer.append( "SPEICHER" + FortranFormatHelper.printf( idManager.getAsciiID( channel ), "i8" ) ); //$NON-NLS-1$//$NON-NLS-2$
      // Ueberlaufknoten optional
      final IRelationType rt2 = (IRelationType) channel.getFeatureType().getProperty( NaModelConstants.IKNOT_MEMBER_PROP );
      final Feature nodeFE = workspace.resolveLink( channel, rt2 );
      final Feature dwonstreamNode = channel.getDownstreamNode();
      if( nodeFE == null || nodeFE == dwonstreamNode )
        rhbBuffer.append( "       0" ); //$NON-NLS-1$
      else
        rhbBuffer.append( FortranFormatHelper.printf( idManager.getAsciiID( nodeFE ), "i8" ) ); //$NON-NLS-1$
      rhbBuffer.append( "  0.00" + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$
      // (itext,a80)
      // RHB 8
      rhbBuffer.append( m_asciiHelper.toAscii( channel, 8 ) + "\n" ); //$NON-NLS-1$
      // (lfs,i4)_(nams,a10)(sv,f10.6)(vmax,f10.6)(vmin,f10.6)(jev,i4)(itxts,a10)
      // RHB 9-10
      rhbBuffer.append( FortranFormatHelper.printf( idManager.getAsciiID( dwonstreamNode ), "i4" ) ); //$NON-NLS-1$
      final Double sv = ((Double) channel.getProperty( NaModelConstants.STORAGE_CHANNEL_SV_PROP )) / 1000000;
      final Double vmax = ((Double) channel.getProperty( NaModelConstants.STORAGE_CHANNEL_VMAX_PROP )) / 1000000;
      final Double vmin = ((Double) channel.getProperty( NaModelConstants.STORAGE_CHANNEL_VMIN_PROP )) / 1000000;
      rhbBuffer.append( " " + " FUNKTION " + FortranFormatHelper.printf( sv, "f9.6" ) + " " + FortranFormatHelper.printf( vmax, "f9.6" ) + " " //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
          + FortranFormatHelper.printf( vmin, "f9.6" ) ); //$NON-NLS-1$

      final Object wvqProp = channel.getProperty( NaModelConstants.STORAGE_CHANNEL_HVVSQD_PROP );
      if( wvqProp instanceof IObservation )
      {
        final int size = (((IObservation) wvqProp).getValues( null )).getCount();
        rhbBuffer.append( FortranFormatHelper.printf( size, "i4" ) + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$
        if( size > 24 )
          throw new Exception( Messages.getString( "org.kalypso.convert.namodel.manager.ChannelManager.33", FeatureHelper.getAsString( channel, "name" ) ) ); //$NON-NLS-1$ //$NON-NLS-2$
        // ____(hv,f8.2)________(vs,f9.6)______(qd,f8.3)
        writeWVQ( (IObservation) wvqProp, rhbBuffer );
      }
      else
      {
        System.out.println( Messages.getString( "org.kalypso.convert.namodel.manager.ChannelManager.2", idManager.getAsciiID( channel ) ) ); //$NON-NLS-1$
      }

      // Kommentar Ende Speicher
      // RHB 12
      rhbBuffer.append( "ENDE\n" ); //$NON-NLS-1$

    }
    else
      throw new UnsupportedOperationException( "can not write Feature to ascii" + channel.toString() ); //$NON-NLS-1$
  }

  /**
   * @param observation
   * @param rhbBuffer
   * @throws SensorException
   */
  private void writeWVQ( final IObservation observation, final StringBuffer rhbBuffer ) throws SensorException
  {
    final IAxis[] axisList = observation.getAxisList();
    final IAxis waterTableAxis = ObservationUtilities.findAxisByType( axisList, ITimeseriesConstants.TYPE_NORMNULL );
    final IAxis volumeAxis = ObservationUtilities.findAxisByType( axisList, ITimeseriesConstants.TYPE_VOLUME );
    final IAxis dischargeAxis = ObservationUtilities.findAxisByType( axisList, ITimeseriesConstants.TYPE_RUNOFF );
    final ITupleModel values = observation.getValues( null );
    final int count = values.getCount();
    for( int row = 0; row < count; row++ )
    {
      final Double w = (Double) values.getElement( row, waterTableAxis );
      final Double v = ((Double) values.getElement( row, volumeAxis )) / 1000000;
      final Double q = (Double) values.getElement( row, dischargeAxis );
      rhbBuffer.append( "    " + FortranFormatHelper.printf( w, "f8.2" ) ); //$NON-NLS-1$ //$NON-NLS-2$
      rhbBuffer.append( "        " + FortranFormatHelper.printf( v, "f9.6" ) ); //$NON-NLS-1$ //$NON-NLS-2$
      rhbBuffer.append( "      " + FortranFormatHelper.printf( q, "f8.3" ) + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    }
  }
}