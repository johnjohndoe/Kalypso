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

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.FE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

/**
 * Unduable add edge command.
 * 
 * @author Patrice Congo
 */
public class AddEdgeCommand implements IDiscrModel1d2dChangeCommand
{
  private final AddNodeCommand node1Command;

  private final AddNodeCommand node2Command;

  private final IFEDiscretisationModel1d2d model;

  private IFE1D2DEdge addedEdge;

  public AddEdgeCommand( IFEDiscretisationModel1d2d model, AddNodeCommand node1Command, AddNodeCommand node2Command )
  {
    this.node1Command = node1Command;
    this.node2Command = node2Command;
    this.model = model;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  @Override
  public String getDescription( )
  {
    return Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.cmds.AddEdgeCommand.0" ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  @Override
  public boolean isUndoable( )
  {
    return true;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  @Override
  public void process( ) throws Exception
  {
    // TODO move code into discretisation model
    IFE1D2DNode addedNode1 = node1Command.getAddedNode();
    IFE1D2DNode addedNode2 = node2Command.getAddedNode();
    addedEdge = model.findEdge( addedNode1, addedNode2 );
    if( addedEdge == null )
      addedEdge = FE1D2DEdge.createFromModel( model, addedNode1, addedNode2 );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  @Override
  public void redo( ) throws Exception
  {
    if( addedEdge != null )
    {
      process();
    }
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  @Override
  public void undo( ) throws Exception
  {
    if( addedEdge != null )
    {
      model.getEdges().remove( addedEdge.getFeature() );
      // TODO remove edges from node add method to node interface
    }
  }

  /**
   * @see xp.IDiscrMode1d2dlChangeCommand#getChangedFeature()
   */
  @Override
  public IFeatureWrapper2[] getChangedFeature( )
  {
    return new IFeatureWrapper2[] { addedEdge };
  }

  /**
   * @see xp.IDiscrMode1d2dlChangeCommand#getDiscretisationModel1d2d()
   */
  @Override
  public IFEDiscretisationModel1d2d getDiscretisationModel1d2d( )
  {
    return model;
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    StringBuffer buf = new StringBuffer( 128 );
    buf.append( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.cmds.AddEdgeCommand.1" ) ); //$NON-NLS-1$
    buf.append( node1Command );
    buf.append( node2Command );
    buf.append( ']' );
    return buf.toString();
  }

  public AddNodeCommand getNode1Command( )
  {
    return node1Command;
  }

  public AddNodeCommand getNode2Command( )
  {
    return node2Command;
  }
}
