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
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

/**
 * Command to remove an edge without container and inverted edge
 * 
 * 
 * @author Patrice Congo
 * @author ig, barbarins 
 * 
 */
@SuppressWarnings("unchecked")
public class RemoveEdgeWithoutContainerOrInvCmd implements ICommand
{

  private final IFEDiscretisationModel1d2d m_model1d2d;

  private Set< Feature > m_setToRemove = null;

  public RemoveEdgeWithoutContainerOrInvCmd( final IFEDiscretisationModel1d2d model1d2d, final IFE1D2DEdge edgeToDel )
  {
    m_setToRemove = new HashSet< Feature >();
    m_model1d2d = model1d2d;
    addEdgeToRemove( edgeToDel );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  @Override
  public String getDescription( )
  {
    return Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.cmds.RemoveEdgeWithoutContainerOrInvCmd.0"); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  @Override
  public boolean isUndoable( )
  {
    return false;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  @Override
  public void process( ) throws Exception
  {
    final Set< IFE1D2DNode > lSetNodesInvolved = new HashSet< IFE1D2DNode >();
    final Set< Feature > lSetNodesFeaturesInvolved = new HashSet< Feature >();
    final Set< String > lSetNodesIdsInvolved = new HashSet< String >();
    final Set< Feature > lSetEdgesIds = new HashSet< Feature >();
    
    for( final Feature lFeature : m_setToRemove )
    {
      IFE1D2DEdge lEdgeToDelete = (IFE1D2DEdge) lFeature.getAdapter( IFE1D2DEdge.class );
      final String edgeID = lEdgeToDelete.getGmlID();
      lSetEdgesIds.add( lEdgeToDelete.getFeature() );
      final List< IFE1D2DNode > nodes = lEdgeToDelete.getNodes();
      lSetNodesInvolved.addAll( nodes );
      
      final IFE1D2DNode middleNode = lEdgeToDelete.getMiddleNode();
      
      if( middleNode != null )
      {
        lSetNodesInvolved.add( middleNode );
        lSetNodesFeaturesInvolved.add( middleNode.getFeature() );
        lSetNodesIdsInvolved.add( middleNode.getGmlID() );
      }
      
      for( final IFE1D2DNode node : nodes )
        node.getContainers().getWrappedList().remove( edgeID );
    }
    
    for( final IFE1D2DNode node : lSetNodesInvolved )
    {
      IFeatureWrapperCollection lActNodeContainers = node.getContainers();
      if( lActNodeContainers == null || lActNodeContainers.isEmpty() || m_setToRemove.containsAll( lActNodeContainers ) ){
        lSetNodesFeaturesInvolved.add( node.getFeature() );
        lSetNodesIdsInvolved.add( node.getFeature().getId() );
      }
    }
    m_model1d2d.getEdges().removeAllAtOnce( lSetEdgesIds );
    m_model1d2d.getNodes().removeAllAtOnce( lSetNodesFeaturesInvolved );
    m_model1d2d.getNodes().removeAllRefsAtOnce( lSetNodesIdsInvolved );
  }

  public void addEdgeToRemove( IFE1D2DEdge edgeToRemove )
  {
    if( edgeToRemove != null ) 
      m_setToRemove.add( edgeToRemove.getFeature() );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  @Override
  public void redo( ) throws Exception
  {

  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  @Override
  public void undo( ) throws Exception
  {

  }

}
