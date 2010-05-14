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

import java.util.ArrayList;
import java.util.List;

import org.kalypso.commons.command.ICommand;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;

/**
 * Command to remove an edge without container and inverted edge
 * 
 * 
 * @author Patrice Congo
 * 
 */
public class RemoveEdgeWithoutContainerOrInvCmd implements ICommand
{

  private IFE1D2DEdge m_edgeToDelete;

  private final IFEDiscretisationModel1d2d m_model1d2d;

  public RemoveEdgeWithoutContainerOrInvCmd( final IFEDiscretisationModel1d2d model1d2d, final IFE1D2DEdge edgeToDel )
  {
    m_edgeToDelete = edgeToDel;
    m_model1d2d = model1d2d;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  public String getDescription( )
  {
    return Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.cmds.RemoveEdgeWithoutContainerOrInvCmd.0"); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  public boolean isUndoable( )
  {
    return false;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  @SuppressWarnings("unchecked")
  public void process( ) throws Exception
  {
    if( !m_edgeToDelete.getContainers().isEmpty() )
      return;
    final List<IFE1D2DNode> nodesInvolved = new ArrayList<IFE1D2DNode>();

    final String edgeID = m_edgeToDelete.getGmlID();
    final List<IFE1D2DNode> nodes = m_edgeToDelete.getNodes();
    nodesInvolved.addAll( nodes );
    final IFE1D2DNode middleNode = m_edgeToDelete.getMiddleNode();
    if( middleNode != null )
      nodesInvolved.add( middleNode );
    for( final IFE1D2DNode node : nodesInvolved )
      node.getContainers().getWrappedList().remove( edgeID );
    // remov edge
    m_model1d2d.getEdges().remove( m_edgeToDelete );

    for( final IFE1D2DNode node : nodesInvolved )
      if( node.getContainers().isEmpty() )
      {
        m_model1d2d.getNodes().remove( node );
        m_model1d2d.getNodes().removeAllRefs( node );
      }
  }

  public void setEdgeToDel( IFE1D2DEdge edgeToDel )
  {
    m_edgeToDelete = edgeToDel;
  }

  public IFE1D2DEdge getEdgeToDel( )
  {
    return m_edgeToDelete;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  public void redo( ) throws Exception
  {

  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  public void undo( ) throws Exception
  {

  }

}
