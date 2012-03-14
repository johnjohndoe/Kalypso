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
import java.util.Map.Entry;
import java.util.logging.Logger;

import org.kalypso.model.hydrology.binding.model.KMChannel;
import org.kalypso.model.hydrology.binding.model.KMParameter;
import org.kalypso.model.hydrology.binding.model.channels.Channel;
import org.kalypso.model.hydrology.binding.model.channels.StorageChannel;
import org.kalypso.model.hydrology.binding.model.channels.VirtualChannel;
import org.kalypso.model.hydrology.internal.IDManager;
import org.kalypso.model.hydrology.internal.preprocessing.net.NetElement;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author doemming
 */
public class GerWriter extends AbstractCoreFileWriter
{
  private static final int VIRTUALCHANNEL = 0;

  private static final int KMCHANNEL = 1;

  // private static final int STORAGECHANNEL = 2;

  private static final int STORAGECHANNEL_HRB = 4;

  private final IDManager m_idManager;

  private final Entry<NetElement, Integer>[] m_rootChannels;

  private final NetElement[] m_channels;

  public GerWriter( final IDManager idManager, final Entry<NetElement, Integer>[] rootChannels, final NetElement[] channels, final Logger logger )
  {
    super( logger );

    m_idManager = idManager;
    m_rootChannels = rootChannels;
    m_channels = channels;
  }

  @Override
  protected void writeContent( final PrintWriter writer )
  {
    for( final Entry<NetElement, Integer> rootChannel : m_rootChannels )
    {
      writer.println( rootChannel.getValue() );
      writer.println( GerWriter.VIRTUALCHANNEL );
    }

    for( final NetElement element : m_channels )
      writeChannel( writer, element.getChannel() );
  }

  private void writeChannel( final PrintWriter writer, final Channel channel )
  {
    final int channelID = m_idManager.getAsciiID( channel );
    writer.format( "%d\n", channelID ); //$NON-NLS-1$

    if( channel instanceof VirtualChannel )
      writer.println( VIRTUALCHANNEL );
    else if( channel instanceof KMChannel )
    {
      writer.println( KMCHANNEL );

      final KMChannel kmChannel = (KMChannel) channel;
      final double faktorRkf = kmChannel.getFaktorRkf();
      final double faktorRnf = kmChannel.getFaktorRnf();
      final IFeatureBindingCollection<KMParameter> parameters = kmChannel.getParameters();
      for( final KMParameter kmParameter : parameters )
        writeParameter( writer, kmParameter, faktorRnf, faktorRkf );
    }
    else if( channel instanceof StorageChannel )
      writer.println( STORAGECHANNEL_HRB );
    else
      throw new UnsupportedOperationException( "can not write Feature to ascii" + channel.toString() ); //$NON-NLS-1$
  }

  private void writeParameter( final PrintWriter writer, final KMParameter kmParameter, final double faktorRnf, final double faktorRkf )
  {
    final double qrk = kmParameter.getQrk();
    final double rnf = kmParameter.getRnf() * faktorRnf;
    final double rkf = kmParameter.getRkf() * faktorRkf;
    final double rkv = kmParameter.getRkv();
    final double rnv = kmParameter.getRnv();
    final double c = kmParameter.getC();

    // (qrk,*)_(rkf,*)_(rnf,*)_(rkv,*)_(rnv,*)_(c,*)_(IGNORE,f11.6)(IGNORE,f12.6)
    writer.format( Locale.US, "%.8f %.8f %.8f %.8f %.8f %.8f%n", qrk, rkf, rnf, rkv, rnv, c ); //$NON-NLS-1$
  }
}