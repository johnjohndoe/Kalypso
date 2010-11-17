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
package org.kalypso.model.hydrology.internal.preprocessing;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.kalypso.convert.namodel.manager.IDManager;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.Channel;
import org.kalypso.model.hydrology.binding.model.Node;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Collects the relevant net elements, that needs to be written to ascii files.
 * 
 * @author Gernot Belger
 */
public class RelevantNetElements
{
  private final List<Feature> m_channels = new ArrayList<Feature>();

  private final List<Feature> m_catchments = new ArrayList<Feature>();

  private final List<Integer> m_rootChannels = new ArrayList<Integer>();

  private final List<Node> m_nodes = new ArrayList<Node>();

  public void addChannel( final Channel channel )
  {
    if( !m_channels.contains( channel ) )
      m_channels.add( channel );
  }

  public void addCatchment( final Catchment channel )
  {
    if( !m_catchments.contains( channel ) )
      m_catchments.add( channel );
  }

  public boolean containsChannel( final Channel channel )
  {
    return m_channels.contains( channel );
  }

  public boolean containsCatchment( final Catchment catchment )
  {
    return m_catchments.contains( catchment );
  }

  /**
   * Returns all relevant channels sorted by type and id, so all files will be always written in the same order <br/>
   * Makes comparison of results etc. easier.
   */
  public Channel[] getChannels( final IDManager idManager )
  {
    final Channel[] channels = m_channels.toArray( new Channel[m_channels.size()] );
    Arrays.sort( channels, new ChannelTypeComparator( idManager ) );
    return channels;
  }

  public Catchment[] getCatchments( final IDManager idManager )
  {
    final Catchment[] catchments = m_catchments.toArray( new Catchment[m_catchments.size()] );
    Arrays.sort( catchments, new CatchmentIDComparator( idManager ) );
    return catchments;
  }

  public void addRootChannel( final int virtualChannelId )
  {
    m_rootChannels.add( virtualChannelId );
  }

  public Integer[] getRootChannels( )
  {
    return m_rootChannels.toArray( new Integer[m_rootChannels.size()] );
  }

  public void addNode( final Node node )
  {
    if( m_nodes.contains( node ) )
      return;

    m_nodes.add( node );
  }

  public Node[] getNodes( )
  {
    return m_nodes.toArray( new Node[m_nodes.size()] );
  }

}