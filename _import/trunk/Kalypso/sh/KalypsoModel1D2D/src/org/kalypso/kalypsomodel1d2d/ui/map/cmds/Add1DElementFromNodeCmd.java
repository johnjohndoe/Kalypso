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

import org.kalypso.kalypsomodel1d2d.ops.ModelOps;
import org.kalypso.kalypsomodel1d2d.schema.binding.FE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper;

/**
 * Undoable Add 1D fe element command from add node cmds
 * 
 * @author Patrice Congo
 */
public class Add1DElementFromNodeCmd implements IDiscrModel1d2dChangeCommand
{
  private IElement1D addedElement;
  
  private AddNodeCommand elementNodeCmds[];
  
  private IFEDiscretisationModel1d2d model;
  
  /**
   * @param model
   * @param elementEdgeCmds an array the command used to create the edges of the element to be created
   *  by this command. the array must contains only {@link AddEdgeCommand} and 
   *    {@link AddEdgeInvCommand} commands
   */
  public Add1DElementFromNodeCmd(
              IFEDiscretisationModel1d2d model,
              AddNodeCommand[] elementNodeCmds)
  {
    Assert.throwIAEOnNullParam( model, "model" );
    Assert.throwIAEOnNullParam( elementNodeCmds, "elementEdgeCmds" );
    for(IDiscrModel1d2dChangeCommand cmd:elementNodeCmds)
    {
      if(  cmd==null  )
      {
        throw new IllegalArgumentException(
            "elementNodeCmds must only contains non null node cmds:"+
            elementNodeCmds); 
      }
    }
    if(elementNodeCmds.length!=2)
    {
      throw new IllegalArgumentException(
          "This create 1D element command required 2 create node command "+
          "\n\tbut got="+elementNodeCmds.length+" node Command");
    }
    
    this.model=model;
    
    this.elementNodeCmds= elementNodeCmds;
    
  }
  
  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  public String getDescription( )
  {
    return "Add FE element";
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  public boolean isUndoable( )
  {
    //TODO implement undo using delete command
    return false;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  public void process( ) throws Exception
  {
    if(addedElement==null)
    {
      List<IFE1D2DEdge> edges = new ArrayList<IFE1D2DEdge>();
      IFE1D2DEdge curEdge;
     
      IFE1D2DNode<IFE1D2DEdge> node0=elementNodeCmds[0].getAddedNode();
      IFE1D2DNode<IFE1D2DEdge> node1=elementNodeCmds[1].getAddedNode();
      if(node0==null || node1==null)
      {
        throw new IllegalStateException(
            "One of the base node command returns a null node:"+
            "\n\tnode0="+node0+
            "\n\tnode1="+node1);
      }
      if(node0.equals( node1 ))
      {
        throw new UnsupportedOperationException(
            "Building an edge from one node not supported");
      }
      
      curEdge=model.findEdge( node0, node1  );
      if(curEdge==null)
      {
          final int size1 = model.getEdges().size();
          curEdge=FE1D2DEdge.createFromModel( model, node0, node1 );
          edges.add( curEdge );
          final int size2 = model.getEdges().size();
          if(size2-size1!=1)
          {
            throw new IllegalStateException("Multi edge created");
          }
      }
      else
      {
        //test whether the edge is in an element
        if(ModelOps.isContainedInAnElement( curEdge ))
        {
          
          return;
        }
      }
      
      addedElement=ModelOps.createElement1d( model, curEdge );
    }
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  public void redo( ) throws Exception
  {
    if(addedElement==null)
    {
      process();
    }
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  public void undo( ) throws Exception
  {
    if(addedElement!=null)
    {
      //TODO remove element and links to it edges
    }
  }

  /**
   * @see xp.IDiscrMode1d2dlChangeCommand#getChangedFeature()
   */
  public IFeatureWrapper[] getChangedFeature( )
  {
    return new IFeatureWrapper[]{addedElement};
  }
  
  /**
   * @see xp.IDiscrMode1d2dlChangeCommand#getDiscretisationModel1d2d()
   */
  public IFEDiscretisationModel1d2d getDiscretisationModel1d2d( )
  {
    return model;
  }
}
