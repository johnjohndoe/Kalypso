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
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper;

/**
 * Undoable Add fe element command
 * 
 * @author Patrice Congo
 */
public class AddElementCmdFromNodeCmd implements IDiscrModel1d2dChangeCommand
{
  //TODO donot forget firering update events
  private IFE1D2DElement addedElement;
  
  private AddNodeCommand elementNodeCmds[];
  
  private IFEDiscretisationModel1d2d model;
  
  /**
   * @param model
   * @param elementEdgeCmds an array the command used to create the edges of the element to be created
   *  by this command. the array must contains only {@link AddEdgeCommand} and 
   *    {@link AddEdgeInvCommand} commands
   */
  public AddElementCmdFromNodeCmd(
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
    return true;
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
      final int MAX_INDEX=elementNodeCmds.length-2;
      for(int i=0;i<=MAX_INDEX;i++)
      {
          IFE1D2DNode<IFE1D2DEdge> node0=elementNodeCmds[i].getAddedNode();
          IFE1D2DNode<IFE1D2DEdge> node1=elementNodeCmds[i+1].getAddedNode();
          
          curEdge=model.findEdge( node0, node1  );
          if(curEdge==null)
          {
            //create edge
            
              curEdge = model.getEdges().addNew( Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE );
              String edgeGmlID = curEdge.getGmlID();
              curEdge.addNode( node0.getGmlID() );
              node0.addContainer( edgeGmlID );
              //
              curEdge.addNode( node1.getGmlID() );
              node1.addContainer( edgeGmlID );
              edges.add( curEdge );
              curEdge.getWrappedFeature().invalidEnvelope();
              
          }
          else
          {
            edges.add( curEdge );
//            throw new RuntimeException("Edge not found");
          }
      }
      addedElement=ModelOps.createElement2d( model, edges );
      System.out.println("Adding elment:"+addedElement);
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
  public IFeatureWrapper getChangedFeature( )
  {
    return addedElement;
  }
  
  /**
   * @see xp.IDiscrMode1d2dlChangeCommand#getDiscretisationModel1d2d()
   */
  public IFEDiscretisationModel1d2d getDiscretisationModel1d2d( )
  {
    return model;
  }
}
