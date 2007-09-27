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
package org.kalypso.kalypsomodel1d2d.conv.results;

import java.util.LinkedList;
import java.util.List;

import org.kalypso.kalypsomodel1d2d.schema.binding.results.GMLNodeResult;

/**
 * @author Thomas Jung
 * 
 */
public class NodeResultHelper
{

  /**
   * sets the mid-side node's water level and depth by interpolation between the corner nodes.
   * 
   * @param nodeDown
   *            first node of the corresponding arc.
   * @param nodeUp
   *            second node of the corresponding arc.
   * @param midsideNode
   *            the mid-side node
   */
  public static void checkMidsideNodeData( final GMLNodeResult nodeDown, final GMLNodeResult nodeUp, final GMLNodeResult midsideNode )
  {
    // TODO check what to do if some of the nodes is null
    // (in the moment exception is thrown...)

    if( nodeDown.getDepth() <= 0 && nodeUp.getDepth() <= 0 )
    {
      interpolateMidsideNodeData( nodeDown, nodeUp, midsideNode );
      // midsideNode.setResultValues( 0, 0, 0, midsideNode.getPoint().getZ() - 1 );
      return;
    }

    if( nodeDown.getDepth() > 0 && nodeUp.getDepth() <= 0 )
    {

      assignMidsideNodeData( nodeDown, midsideNode ); // assignment leads into extrapolation of the water level!!
      // interpolateMidsideNodeData( nodeDown, nodeUp, midsideNode );
      return;
    }
    if( nodeDown.getDepth() <= 0 && nodeUp.getDepth() > 0 )
    {
      assignMidsideNodeData( nodeUp, midsideNode ); // assignment leads into extrapolation of the water level!!
    }

    if( nodeDown.getDepth() > 0 && nodeUp.getDepth() > 0 )
    {
      interpolateMidsideNodeData( nodeDown, nodeUp, midsideNode );
      return;
    }
  }

  /**
   * interpolates the water level for the midside node by using the water levels of the corner nodes. The depth will be
   * calculated as well, using the interpolated water level.
   * 
   * @param nodeDown
   *            first node of the arc on which the corner node lies.
   * @param nodeUp
   *            second node
   * @param midsideNode
   *            the midside node
   */
  private static void interpolateMidsideNodeData( final GMLNodeResult nodeDown, final GMLNodeResult nodeUp, final GMLNodeResult midsideNode )
  {
    final List<Double> waterlevels = new LinkedList<Double>();
    final List<Double> depths = new LinkedList<Double>();

    waterlevels.add( nodeDown.getWaterlevel() );
    waterlevels.add( nodeUp.getWaterlevel() );

    depths.add( nodeDown.getDepth() );
    depths.add( nodeUp.getDepth() );

    final double waterlevel = getMeanValue( waterlevels );
    midsideNode.setWaterlevel( waterlevel );
    // midsideNode.setDepth( interpolate( depths ) );
    final double depth = waterlevel - midsideNode.getPoint().getZ();
    if( depth < 0 )
      midsideNode.setDepth( 0.0 );
    else
      midsideNode.setDepth( depth );
  }

  private static double getMeanValue( final List<Double> values )
  {
    double sum = 0;
    for( int i = 0; i < values.size(); i++ )
    {
      sum = sum + values.get( i );
    }
    return (sum / values.size());
  }

  private static void assignMidsideNodeData( final GMLNodeResult node, final GMLNodeResult midsideNode )
  {
    final double waterlevel = node.getWaterlevel();
    midsideNode.setWaterlevel( waterlevel );

    final double depth = waterlevel - midsideNode.getPoint().getZ();
    if( depth < 0 )
      midsideNode.setDepth( 0.0 );
    else
      midsideNode.setDepth( depth );
  }

}
