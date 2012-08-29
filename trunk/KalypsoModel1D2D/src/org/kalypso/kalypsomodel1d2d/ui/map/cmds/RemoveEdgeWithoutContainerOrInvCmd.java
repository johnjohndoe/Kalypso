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
import java.util.List;
import java.util.Set;

import org.kalypso.commons.command.ICommand;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

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
    final Set<IFE1D2DNode> nodesToRemove = new HashSet<>();
    final Set<IFE1D2DEdge> edgesToRemove = new HashSet<>();

    /* basically delete edges and find nodes, that are involved */
    for( final IFE1D2DEdge lEdgeToDelete : m_edgesToRemove )
    {
      edgesToRemove.add( lEdgeToDelete );

      final List<IFE1D2DNode> nodes = lEdgeToDelete.getNodes();
      changedNodes.addAll( nodes );

      final IFE1D2DNode middleNode = lEdgeToDelete.getMiddleNode();

      if( middleNode != null )
      {
        changedNodes.add( middleNode );
        /* middle nodes can always be removed directly */
        nodesToRemove.add( middleNode );
      }

      for( final IFE1D2DNode node : nodes )
        removeEdgeFromNode( node, lEdgeToDelete );
    }

    /* Check if nodes can finally be removed */
    for( final IFE1D2DNode node : changedNodes )
    {
      if( shouldRemoveNode( node, edgesToRemove ) )
        nodesToRemove.add( node );
    }

    m_model1d2d.getEdges().removeAll( edgesToRemove );
    m_model1d2d.getNodes().removeAll( nodesToRemove );

    // The caller of this command is responsible for the event

    // final IFE1D2DEdge[] removedEdges = edgesToRemove.toArray( new IFE1D2DEdge[edgesToRemove.size()] );
    // final IFE1D2DNode[] removedNodes = nodesToRemove.toArray( new IFE1D2DNode[nodesToRemove.size()] );

    // final GMLWorkspace workspace = m_model1d2d.getWorkspace();
    // workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, m_model1d2d, removedEdges,
    // FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_DELETE ) );
    // workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, m_model1d2d, removedNodes,
    // FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_DELETE ) );
  }

  private boolean shouldRemoveNode( final IFE1D2DNode node, final Set<IFE1D2DEdge> allRemovedEdges )
  {
    final IFeatureBindingCollection< ? > lActNodeContainers = node.getContainers();

    if( lActNodeContainers == null || lActNodeContainers.isEmpty() )
      return true;

    /* As soon as ther is one edge on the node, that will not be removed, do not remove the node as well */
    for( final Feature edge : lActNodeContainers )
    {
      if( !allRemovedEdges.contains( edge ) )
        return false;
    }

    return true;
  }

  private void removeEdgeFromNode( final IFE1D2DNode node, final IFE1D2DEdge lEdgeToDelete )
  {
    final FeatureList edgeContainer = node.getContainers().getFeatureList();

    final boolean removed = edgeContainer.removeLink( lEdgeToDelete );
    if( !removed )
      System.out.println( "SEVERE: inconsistent net" ); //$NON-NLS-1$
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
