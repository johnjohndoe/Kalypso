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

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IEdgeInv;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

/**
 * Undoable add edge command.
 * 
 * @author Patrice Congo
 */
public class AddEdgeInvCommand implements IDiscrModel1d2dChangeCommand
{
  private AddEdgeCommand edgeCommand;
  private IFEDiscretisationModel1d2d model;
  private IEdgeInv addedEdgeInv;
  
  public AddEdgeInvCommand(
          IFEDiscretisationModel1d2d model, 
          AddEdgeCommand addEdgeCommand)
  {
    this.model=model;
    this.edgeCommand=addEdgeCommand;
  }
  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  public String getDescription( )
  {
    return "Add EdgeInv";
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  public boolean isUndoable( )
  {
    return true;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  public void process( ) throws Exception
  {
    //TODO move code into discretisation model
    IFE1D2DEdge edgeToInv = (IFE1D2DEdge)edgeCommand.getChangedFeature()[0];
    if(edgeToInv==null)
    {
      return;
    }
    
    if(edgeToInv.getNodes().size()!=2)
    {
      throw new RuntimeException(
            "Edge does not contains 2 nodes:"+edgeToInv.getNodes().size());
    }
    
    
    IFE1D2DNode<IFE1D2DEdge> addedNode1 = edgeToInv.getNode( 0 );
    IFE1D2DNode<IFE1D2DEdge> addedNode2 = edgeToInv.getNode( 1 );
    addedEdgeInv = (IEdgeInv)model.findEdge( addedNode2, addedNode1 );//got the node inverted
    if(addedEdgeInv==null)
    {
      throw new RuntimeException("Could not create edge for");
    }
        
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  public void redo( ) throws Exception
  {
    if(addedEdgeInv!=null)
    {
      process();
    }
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  public void undo( ) throws Exception
  {
    if(addedEdgeInv!=null)
    {
      model.getEdges().remove( addedEdgeInv.getWrappedFeature() );
      //TODO remove edges from node add method to node interface
    }
  }
  /**
   * @see xp.IDiscrMode1d2dlChangeCommand#getChangedFeature()
   */
  public IFeatureWrapper2[] getChangedFeature( )
  {
    return new IFeatureWrapper2[]{addedEdgeInv};
  }
  
  /**
   * @see xp.IDiscrMode1d2dlChangeCommand#getDiscretisationModel1d2d()
   */
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
    StringBuffer buf= new StringBuffer(128);
    buf.append("AddEdgeCommand[");
    buf.append( edgeCommand);
    buf.append( ']' );
    return buf.toString();
  }
}
