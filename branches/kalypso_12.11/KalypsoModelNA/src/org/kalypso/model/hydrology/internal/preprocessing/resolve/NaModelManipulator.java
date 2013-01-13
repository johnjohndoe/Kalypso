/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.hydrology.internal.preprocessing.resolve;

import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.binding.model.channels.Channel;
import org.kalypso.model.hydrology.binding.model.channels.IVirtualChannel;
import org.kalypso.model.hydrology.binding.model.nodes.INode;
import org.kalypso.model.hydrology.binding.model.nodes.Node;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * Wrapper around {@link org.kalypso.model.hydrology.binding.model.NaModell} that allows to manipulate the net in certain ways.
 * 
 * @author Andreas von Dömming
 * @author Gernot Belger
 */
class NaModelManipulator
{
  private final NaModell m_model;

  public NaModelManipulator( final NaModell model )
  {
    m_model = model;
  }

  /**
   * Inserts a new node connected by a virtual chanel upstreams of a given node.<br/>
   * In order to do that, another virtual channel is additonally added before the new node.<br/>
   * Before:
   * 
   * <pre>
   * o( existing )
   * </pre>
   * 
   * after:
   * 
   * <pre>
   *  |new Channel2|
   *     |
   *     V
   *     o(new Node2)  (return value)
   *     |
   *     V
   *  |new Channel1|
   *     |
   *     V
   *     o(existing)
   * </pre>
   */
  public Node insertUpstreamVChannel( final Node existingNode )
  {
    final IFeatureBindingCollection<Node> nodes = m_model.getNodes();
    final IFeatureBindingCollection<Channel> channels = m_model.getChannels();

    // add to collections:
    final Channel newChannel1 = channels.addNew( IVirtualChannel.FEATURE_VIRTUAL_CHANNEL );
    final Channel newChannel2 = channels.addNew( IVirtualChannel.FEATURE_VIRTUAL_CHANNEL );
    final Node newNode2 = nodes.addNew( INode.FEATURE_NODE );

    /* Create network */
    newChannel2.setDownstreamNode( newNode2 );
    newNode2.setDownstreamChannel( newChannel1 );
    newChannel1.setDownstreamNode( existingNode );

    return newNode2;
  }

  /**
   * Moves a catchment to a (new) virutal catchment that wil be connected to the downstream node of the catchments channel.
   * 
   * <pre>
   *     -C-o (existing channel c with catchment T and downstream node o)
   *      ^
   *      T
   * </pre>
   * 
   * after:
   * 
   * <pre>
   *     -C-o
   *        |
   *        V<T  (new virtual channel with existing catchment T, downstream node o and no upstream node)
   * </pre>
   */
  public void moveCatchmentToVirtualChannel( final Catchment catchment )
  {
    final Channel channel = catchment.getChannel();

    /* Create new channel and relocate catchment to the new channel. */
    final Channel newChannel = m_model.getChannels().addNew( IVirtualChannel.FEATURE_VIRTUAL_CHANNEL );
    catchment.setChannel( newChannel );

    /* Connect the new channel into the net */
    final Node node = channel.getDownstreamNode();
    newChannel.setDownstreamNode( node );
  }
}