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

import org.kalypso.commons.command.ICommand;
import org.kalypso.kalypsomodel1d2d.schema.binding.IEdgeInv;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d;



/**
 * Command to remove an edge without container and inverted edge
 * 
 *
 * @author Patrice Congo
 *
 */
public class RemoveEdgeWithoutContainerOrInvCmd implements ICommand
{

  private IFE1D2DEdge edgeToDel;
  
  private IFEDiscretisationModel1d2d model1d2d;
  
  public RemoveEdgeWithoutContainerOrInvCmd(
                    IFEDiscretisationModel1d2d model1d2d,
                    IFE1D2DEdge edgeToDel)
  {
    this.edgeToDel=edgeToDel;
    this.model1d2d=model1d2d;
  }
  
  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  public String getDescription( )
  {
    return "Delete an edge without container and inverted";
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
  public void process( ) throws Exception
  {
    IFE1D2DEdge edge = edgeToDel;
    if(!edge.getContainers().isEmpty())
    {
      System.out.println("Edge has containers");
      return;
    }
    
    if(edge instanceof IEdgeInv)
    {
      IFE1D2DEdge inverted = ((IEdgeInv)edge).getInverted();
      inverted.resetInvEdge();
      
      //remove link to nodes
      String edgeID = edge.getGmlID();
      IFE1D2DNode[] nodeArray = 
        (IFE1D2DNode[])inverted.getNodes().toArray( new IFE1D2DNode[]{} );
      RemoveNodeWithoutContainer remNode = 
              new RemoveNodeWithoutContainer(null,model1d2d);
      for(IFE1D2DNode node: nodeArray)
      {
        node.getContainers().getWrappedList().remove( edgeID );
        
      }
      
      for(;model1d2d.getEdges().remove( edge );)
      {
        //does remove all ocurence;
      }
      RemoveEdgeWithoutContainerOrInvCmd remInverted= 
        new RemoveEdgeWithoutContainerOrInvCmd(model1d2d,inverted);
      remInverted.process();
    }
    else
    {//for normal edges
      
      
      IEdgeInv edgeInv = edge.getEdgeInv();
      if(edgeInv!=null)
      {
        if(!edgeInv.getContainers().isEmpty())
        {
          System.out.println("Edge inverted has container");
          
          return;
        }
        else
        {
          //remode  edge inv without container
          IFE1D2DEdge inverted = edgeInv.getInverted();
          inverted.resetInvEdge();
          
          //remove link to nodes
          String edgeID = edgeInv.getGmlID();
          IFE1D2DNode[] nodeArray = 
            (IFE1D2DNode[])inverted.getNodes().toArray( new IFE1D2DNode[]{} );
          RemoveNodeWithoutContainer remNode = 
                  new RemoveNodeWithoutContainer(null,model1d2d);
          for(IFE1D2DNode node: nodeArray)
          {
            node.getContainers().getWrappedList().remove( edgeID );
            
          }
          
          for(;model1d2d.getEdges().remove( edgeInv );)
          {
            //does remove all ocurence;
          }
        }
      }
      edgeInv=edge.getEdgeInv();
      if(edgeInv!=null)
      {
  //    TODO care with edge with invedge and no element
  //    may be readjust the network
        System.out.println("Edge still has inverted");
        return;
      }
      else
      {
        String edgeID=edge.getGmlID();
        IFE1D2DNode[] nodeArray = 
          (IFE1D2DNode[])edge.getNodes().toArray( new IFE1D2DNode[]{} );
        RemoveNodeWithoutContainer remNode = 
                new RemoveNodeWithoutContainer(null,model1d2d);
        for(IFE1D2DNode node: nodeArray)
        {
          boolean isRemoved = node.getContainers().getWrappedList().remove( edgeID );
          remNode.setNodeToDel( node );
          remNode.process();
        }        
        //remov edge
        model1d2d.getEdges().remove( edge );
      }
    }
  }

  public void setEdgeToDel( IFE1D2DEdge edgeToDel )
  {
    this.edgeToDel = edgeToDel;
  }
  
  public IFE1D2DEdge getEdgeToDel( )
  {
    return edgeToDel;
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
