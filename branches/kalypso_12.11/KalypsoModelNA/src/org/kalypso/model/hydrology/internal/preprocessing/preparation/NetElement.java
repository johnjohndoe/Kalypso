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
package org.kalypso.model.hydrology.internal.preprocessing.preparation;

import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;

import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.channels.Channel;
import org.kalypso.model.hydrology.binding.model.nodes.Node;
import org.kalypso.model.hydrology.internal.IDManager;
import org.kalypsodeegree.model.feature.Feature;

/**
 * A NetElement encapsulates a Channel-Element and its dependencies <br>
 * In the example below each channel represents one net-element. Here you can see the dependencies related to the one
 * channel written in capital letter in the middle.
 *
 * <pre>
 *                                          Node-----&gt;-----Node
 *                                            |              |
 *                                            V              V
 *                                            |              |
 *                         Catchment---&gt;---CHANNEL        Channel (downstream)
 *                             |              |
 *                             |              V
 *                             V              |
 *                             |             Node
 *                             |              |
 *                          Catchment         V
 *                             |              |
 *                             V           Channel (downstream)
 *                             |
 *                          Channel (downstream)
 *
 *
 * </pre>
 */
public class NetElement
{
  private boolean m_calculated = false;

  private final List<NetElement> m_upStreamDepends = new ArrayList<>();

  private final List<NetElement> m_downStreamDepends = new ArrayList<>();

  private final Channel m_channel;

  private static final int ANFANGSKNOTEN = 9001; //$NON-NLS-1$

  private static final String ENDKNOTEN = "   10000"; //$NON-NLS-1$

  private Node m_overflowNode = null;

  private final IDManager m_idManager;

  public NetElement( final Channel channel, final IDManager idManager )
  {
    m_channel = channel;
    m_idManager = idManager;

    // BUGFIX: generate ascii id in order of addition, see comment in RelevantNetElements
    getAsciiID();
  }

  public Channel getChannel( )
  {
    return m_channel;
  }

  public boolean isCalculated( )
  {
    return m_calculated;
  }

  public void addUpStream( final NetElement upStreamElement )
  {
    if( !m_upStreamDepends.contains( upStreamElement ) )
      m_upStreamDepends.add( upStreamElement );
    upStreamElement.addDownStream( this );
  }

  private void addDownStream( final NetElement downStreamElement )
  {
    // FIXME: should also add myself as upstream to downstream: but we need to avoid endless loop

    if( !m_downStreamDepends.contains( downStreamElement ) )
      m_downStreamDepends.add( downStreamElement );
  }

  public Feature getChannelsBelowDownStreamNode( )
  {
    final Node downStreamNode = m_channel.getDownstreamNode();
    return downStreamNode.getDownstreamChannel();
  }

  public NetElement[] getDownStreamNetElements( )
  {
    return m_downStreamDepends.toArray( new NetElement[m_downStreamDepends.size()] );
  }

  public NetElement[] getUpStreamNetElements( )
  {
    return m_upStreamDepends.toArray( new NetElement[m_upStreamDepends.size()] );
  }

  /**
   * Collects the relevant elements, that should be written to the net file.
   */
  public void collectRelevantElements( final RelevantNetElements relevantElements )
  {
    relevantElements.addChannel( this );

    m_calculated = true;

    // append channel:
    final Node downstreamNode = m_channel.getDownstreamNode();
    final Node upstreamNode = m_channel.findUpstreamNode();

    // append catchments
    final Catchment[] catchmentForThisChannel = m_channel.findCatchments();
    for( final Catchment catchment : catchmentForThisChannel )
      relevantElements.addCatchment( catchment );

    if( upstreamNode != null )
      relevantElements.addNode( upstreamNode );

    if( downstreamNode != null )
      relevantElements.addNode( downstreamNode );
  }

  /**
   * writes part 1 of netfile
   */
  public void write( final PrintWriter netBuffer )
  {
    // append channel:
    final int channelID = m_idManager.getAsciiID( m_channel );
    netBuffer.append( String.format( "%8d", channelID ) ); //$NON-NLS-1$

    final Node downstreamNode = m_channel.getDownstreamNode();
    final Node upstreamNode = m_channel.findUpstreamNode();

    final Catchment[] catchmentForThisChannel = m_channel.findCatchments();

    // append upstream node:
    final int upstreamNodeID = upstreamNode == null ? ANFANGSKNOTEN : m_idManager.getAsciiID( upstreamNode );
    netBuffer.append( String.format( "%8d", upstreamNodeID ) ); //$NON-NLS-1$

    // append downstream node:
    final int downstreamNodeID = m_idManager.getAsciiID( downstreamNode );
    netBuffer.append( String.format( "%8d", downstreamNodeID ) ); //$NON-NLS-1$

    // append catchments
    netBuffer.append( " " + catchmentForThisChannel.length + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$
    for( final Catchment catchment : catchmentForThisChannel )
    {
      final int chatchmentID = m_idManager.getAsciiID( catchment );
      netBuffer.append( String.format( "%8d\n", chatchmentID ) ); //$NON-NLS-1$
    }
  }

  public void writeRootChannel( final PrintWriter netBuffer, final int virtualChannelId )
  {
    final Node downstreamNode = m_channel.getDownstreamNode();
    if( downstreamNode == null )
      System.out.println( "knotU=null" ); //$NON-NLS-1$

    final int downstreamNodeID = m_idManager.getAsciiID( downstreamNode );

    netBuffer.append( "   " + virtualChannelId ); //$NON-NLS-1$
    netBuffer.append( String.format( "%8d", downstreamNodeID ) ); //$NON-NLS-1$
    netBuffer.append( ENDKNOTEN );
    netBuffer.append( " 0\n" ); //$NON-NLS-1$
  }

  @Override
  public String toString( )
  {
    final Feature channel = getChannel();
    return "FID:" + channel.getId() + " AsciiID: " + m_idManager.getAsciiID( channel ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  public void setOverflowNode( final Node overflowNode )
  {
    m_overflowNode = overflowNode;
  }

  public Node getOverflowNode( )
  {
    return m_overflowNode;
  }

  public int getAsciiID( )
  {
    return m_idManager.getAsciiID( getChannel() );
  }
}