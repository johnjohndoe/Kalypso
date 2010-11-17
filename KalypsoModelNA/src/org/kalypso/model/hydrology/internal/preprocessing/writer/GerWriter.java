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

import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.convert.namodel.manager.ASCIIHelper;
import org.kalypso.convert.namodel.manager.AsciiBuffer;
import org.kalypso.convert.namodel.manager.IDManager;
import org.kalypso.model.hydrology.binding.model.Channel;
import org.kalypso.model.hydrology.binding.model.KMChannel;
import org.kalypso.model.hydrology.binding.model.KMParameter;
import org.kalypso.model.hydrology.binding.model.StorageChannel;
import org.kalypso.model.hydrology.binding.model.VirtualChannel;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author doemming
 */
public class GerWriter
{
  public static final int VIRTUALCHANNEL = 0;

  private static final int KMCHANNEL = 1;

  private static final int STORAGECHANNEL = 2;

  private final NAConfiguration m_conf;

  private final ASCIIHelper m_asciiHelper = new ASCIIHelper( getClass().getResource( "/org/kalypso/convert/namodel/manager/resources/formats/gerinne.txt" ) );

  public GerWriter( final NAConfiguration conf )
  {
    m_conf = conf;
  }

  public void writeFile( final Channel[] channels, final AsciiBuffer asciiBuffer ) throws Exception
  {
    for( final Channel channel : channels )
      writeFeature( asciiBuffer, channel );
  }

  private void writeFeature( final AsciiBuffer asciiBuffer, final Channel channel ) throws Exception
  {
    final IDManager idManager = m_conf.getIdManager();

    final StringBuffer channelBuffer = asciiBuffer.getChannelBuffer();

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
    }
    else
      throw new UnsupportedOperationException( "can not write Feature to ascii" + channel.toString() ); //$NON-NLS-1$
  }
}