/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Bj�rnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universit�t Hamburg-Harburg, Institut f�r Wasserbau, Hamburg, Germany
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
package org.kalypso.kalypsomodel1d2d.ui.map.cline;

import org.apache.commons.lang3.ArrayUtils;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * Validates the currently snapped node when editing a continuity line.
 * 
 * @author Gernot Belger
 */
class ContinuityLineEditValidator
{
  private final IMapPanel m_panel;

  private final IFE1D2DNode m_snapNode;

  private final IFE1D2DNode[] m_nodes;

  private final IFEDiscretisationModel1d2d m_discModel;

  public ContinuityLineEditValidator( final IFEDiscretisationModel1d2d discModel, final IMapPanel panel, final IFE1D2DNode[] nodes, final IFE1D2DNode snapNode )
  {
    m_discModel = discModel;
    m_panel = panel;
    m_nodes = nodes;
    m_snapNode = snapNode;
  }

  public String execute( )
  {
    /* 1d-nodes: only on end of line */
    if( m_nodes.length == 0 && !is2dNode( m_snapNode ) )
    {
      final IFE1D2DElement[] elements = m_snapNode.getAdjacentElements();
      if( elements.length == 0 )
        return "Cannot add continuity line to single 1D node that is not attached to the net";

      if( elements.length > 1 )
        return "1D-continuity line must be at end of 1D-reach";
    }

    /* check for any conti line on node */
    final GM_Point snapPoint = m_snapNode.getPoint();
    final IFELine touchedLine = m_discModel.findContinuityLine( snapPoint, IFEDiscretisationModel1d2d.CLUSTER_TOLERANCE );
    if( touchedLine != null )
      return "Node is already part of a continuity line";

    if( m_nodes.length > 0 )
    {
      if( !is2dNode( m_snapNode ) )
        return "2D-continuity line cannot touch 1D-node";

      /* last point: special handling, else double click to finish will not work; also prevents line with only one point */
      if( m_nodes.length > 1 && m_snapNode == m_nodes[m_nodes.length - 1] )
        return null;

      /* duplicate point */
      for( final IFE1D2DNode node : m_nodes )
      {
        if( node == m_snapNode )
          return "Node already contained in line";
      }

      final ContinuityLine2DValidator validator2d = new ContinuityLine2DValidator( m_discModel, m_panel, ArrayUtils.add( m_nodes, m_snapNode ) );
      return validator2d.execute();
    }

    return null;
  }

  static boolean is2dNode( final IFE1D2DNode node )
  {
    final IFE1D2DElement[] elements = node.getAdjacentElements();
    for( final IFE1D2DElement element : elements )
    {
      if( !(element instanceof IPolyElement) )
        return false;
    }

    return true;
  }
}