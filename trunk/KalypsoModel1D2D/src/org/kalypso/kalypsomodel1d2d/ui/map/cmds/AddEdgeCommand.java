/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.FE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Unduable add edge command.
 *
 * @author Patrice Congo
 */
public class AddEdgeCommand implements IFeatureChangeCommand
{
  private final AddNodeCommand m_node1Command;

  private final AddNodeCommand m_node2Command;

  private final IFEDiscretisationModel1d2d m_model;

  private IFE1D2DEdge addedEdge;

  public AddEdgeCommand( final IFEDiscretisationModel1d2d model, final AddNodeCommand node1Command, final AddNodeCommand node2Command )
  {
    m_node1Command = node1Command;
    m_node2Command = node2Command;
    m_model = model;
  }

  @Override
  public String getDescription( )
  {
    return Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.cmds.AddEdgeCommand.0" ); //$NON-NLS-1$
  }

  @Override
  public boolean isUndoable( )
  {
    return true;
  }

  @Override
  public void process( ) throws Exception
  {
    // TODO move code into discretisation model
    final IFE1D2DNode addedNode1 = m_node1Command.getAddedNode();
    final IFE1D2DNode addedNode2 = m_node2Command.getAddedNode();
    addedEdge = m_model.findEdge( addedNode1, addedNode2 );
    if( addedEdge == null )
      addedEdge = FE1D2DEdge.createFromModel( m_model, addedNode1, addedNode2 );
  }

  @Override
  public void redo( ) throws Exception
  {
    if( addedEdge != null )
    {
      process();
    }
  }

  @Override
  public void undo( ) throws Exception
  {
    if( addedEdge != null )
    {
      m_model.getEdges().remove( addedEdge );
      // TODO remove edges from node add method to node interface
    }
  }

  @Override
  public Feature[] getChangedFeatures( )
  {
    return new Feature[] { addedEdge };
  }

  @Override
  public String toString( )
  {
    final StringBuffer buf = new StringBuffer( 128 );
    buf.append( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.cmds.AddEdgeCommand.1" ) ); //$NON-NLS-1$
    buf.append( m_node1Command );
    buf.append( m_node2Command );
    buf.append( ']' );
    return buf.toString();
  }

  public AddNodeCommand getNode1Command( )
  {
    return m_node1Command;
  }

  public AddNodeCommand getNode2Command( )
  {
    return m_node2Command;
  }
}
