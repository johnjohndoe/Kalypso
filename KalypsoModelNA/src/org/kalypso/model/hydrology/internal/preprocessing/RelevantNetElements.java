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
import java.util.Collections;
import java.util.List;
import java.util.Map.Entry;

import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.nodes.Node;
import org.kalypso.model.hydrology.internal.IDManager;
import org.kalypso.model.hydrology.internal.preprocessing.net.NetElement;

/**
 * Collects the relevant net elements, that needs to be written to ascii files.
 *
 * @author Gernot Belger
 */
public class RelevantNetElements
{
  private final List<NetElement> m_channels = new ArrayList<>();

  private final List<Catchment> m_catchments = new ArrayList<>();

  private final List<Entry<NetElement, Integer>> m_rootChannels = new ArrayList<>();

  private final List<Node> m_nodes = new ArrayList<>();

  public void addChannel( final NetElement channel )
  {
    if( !m_channels.contains( channel ) )
      m_channels.add( channel );
  }

  public void addCatchment( final Catchment channel )
  {
    if( !m_catchments.contains( channel ) )
      m_catchments.add( channel );
  }

  public boolean containsCatchment( final Catchment catchment )
  {
    return m_catchments.contains( catchment );
  }

  public NetElement[] getChannels( )
  {
    return m_channels.toArray( new NetElement[m_channels.size()] );
  }

  /**
   * Returns all relevant channels sorted by type and id, so all files will be always written in the same order <br/>
   * Makes comparison of results etc. easier.
   */
  public NetElement[] getChannelsSorted( final IDManager idManager )
  {
    final NetElement[] channels = m_channels.toArray( new NetElement[m_channels.size()] );

    // BUGFIX/REMARK: as the ascii id was accessed for the first time inside the sort
    // method, the ordering of elements depended on the sort algorythm.
    // We no (see above) initialize the ascii id when the net element is created,
    // so the order is independend of the this sort.

    Arrays.sort( channels, new ChannelTypeComparator( idManager ) );
    return channels;
  }

  public Catchment[] getCatchmentsSorted( final IDManager idManager )
  {
    final Catchment[] catchments = m_catchments.toArray( new Catchment[m_catchments.size()] );
    Arrays.sort( catchments, new CatchmentIDComparator( idManager ) );
    return catchments;
  }

  public void addRootChannel( final NetElement netElement, final int virtualChannelId )
  {
    final Entry<NetElement, Integer> rootChannelEntry = Collections.singletonMap( netElement, virtualChannelId ).entrySet().iterator().next();

    m_rootChannels.add( rootChannelEntry );
  }

  public Entry<NetElement, Integer>[] getRootChannels( )
  {
    return m_rootChannels.toArray( new Entry[m_rootChannels.size()] );
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