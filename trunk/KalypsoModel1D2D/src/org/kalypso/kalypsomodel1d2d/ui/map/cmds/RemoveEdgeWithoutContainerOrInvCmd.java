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
package org.kalypso.kalypsomodel1d2d.ui.map.cmds;

import java.util.HashSet;
import java.util.Set;

import org.kalypso.commons.command.ICommand;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;

/**
 * Command to remove an edge without container and inverted edge
 * 
 * @author Patrice Congo
 * @author ig, barbarins
 */
public class RemoveEdgeWithoutContainerOrInvCmd implements ICommand
{
  private final IFEDiscretisationModel1d2d m_model1d2d;

  private final Set<IFE1D2DEdge> m_edgesToRemove = new HashSet<>();

  public RemoveEdgeWithoutContainerOrInvCmd( final IFEDiscretisationModel1d2d model1d2d, final IFE1D2DEdge edgeToDel )
  {
    m_model1d2d = model1d2d;

    addEdgeToRemove( edgeToDel );
  }

  @Override
  public String getDescription( )
  {
    return Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.cmds.RemoveEdgeWithoutContainerOrInvCmd.0" ); //$NON-NLS-1$
  }

  @Override
  public boolean isUndoable( )
  {
    return false;
  }

  @Override
  public void process( ) throws Exception
  {
    final Set<IFE1D2DNode> changedNodes = new HashSet<>();

    /* basically delete edges and find nodes, that are involved */
    for( final IFE1D2DEdge lEdgeToDelete : m_edgesToRemove )
    {

      final IFE1D2DNode[] nodes = lEdgeToDelete.getNodes();
      changedNodes.add( nodes[0] );
      changedNodes.add( nodes[1] );

      for( final IFE1D2DNode node : nodes )
        node.removeLinkedEdge( lEdgeToDelete );

      m_model1d2d.removeEdge( lEdgeToDelete );
    }

    /* Check if nodes can finally be removed */
    for( final IFE1D2DNode node : changedNodes )
    {
      if( node.getLinkedEdges().length == 0 )
        m_model1d2d.removeNode( node );
    }
  }

  public void addEdgeToRemove( final IFE1D2DEdge edgeToRemove )
  {
    if( edgeToRemove != null )
      m_edgesToRemove.add( edgeToRemove );
  }

  @Override
  public void redo( ) throws Exception
  {

  }

  @Override
  public void undo( ) throws Exception
  {

  }
}
