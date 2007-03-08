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
import org.kalypso.kalypsomodel1d2d.schema.binding.FE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DContinuityLine;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEJunction1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEMiddleNode;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IFEElement;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * Undoable Add 1D fe element command from add node cmds
 * 
 * @author Patrice Congo
 */
public class AddJunctionElementFromClAndElement1DCmd implements IDiscrModel1d2dChangeCommand
{
  private IElement1D element1D; 
  
  private IFEDiscretisationModel1d2d model;
  
  private IFE1D2DContinuityLine continuityLine;
  
  private IFEJunction1D2D addedJunction;
  
  /**
   * @param model
   * @param elementEdgeCmds an array the command used to create the edges of the element to be created
   *  by this command. the array must contains only {@link AddEdgeCommand} and 
   *    {@link AddEdgeInvCommand} commands
   */
  public AddJunctionElementFromClAndElement1DCmd(
              IFEDiscretisationModel1d2d model,
              IElement1D element1D,
              IFE1D2DContinuityLine continuityLine)
  {
    
    Assert.throwIAEOnNullParam( model, "model" );
    Assert.throwIAEOnNullParam( element1D, "element1D" );
    Assert.throwIAEOnNullParam( continuityLine, "continuityLine" );

    this.model=model;
    this.element1D=element1D;
    this.continuityLine=continuityLine;

  }
  
  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  public String getDescription( )
  {
    return "Add Junction from Element 1D and Continuity Line Selection";
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
    if(addedJunction==null)
    {
      try
      {
        IFE1D2DEdge elementEdge = 
                  element1D.getEdge();
        if(elementEdge==null)
        {
          System.out.println(
              "Element 1d does not contains an edge"+element1D);
          return;
        }
        
        IFE1D2DEdge clMiddleEdge=
            findCLMiddleEdge(continuityLine);
        if(clMiddleEdge==null)
        {
          System.out.println("Could not find cl middle edge");
          return;
        }

        IFE1D2DNode<IFE1D2DEdge> node0 = 
          findJunctionStartNode(elementEdge);
        
        IFE1D2DNode targetClNodel = addTargetCLNode(continuityLine,node0);
        
        
        IFE1D2DEdge curEdge = model.findEdge( node0, targetClNodel );
        if(curEdge==null)
        {
            final int size1 = model.getEdges().size();
            curEdge=FE1D2DEdge.createFromModel( model, node0, targetClNodel );
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
            System.out.println("Edge must not be already in an element:"+curEdge);
            return;
          }
        }
        
        addedJunction = 
          ModelOps.createJunction( model, curEdge );
      }
      catch( Exception e )
      {
        e.printStackTrace();
        throw e;
      }
    }
  }

  private IFE1D2DNode<IFE1D2DEdge> findJunctionStartNode( 
                                          IFE1D2DEdge elementEdge )
  {
    IFE1D2DNode node1 = elementEdge.getNode( 1 );
    if(node1.getContainers().size()==1)
    {
      return node1;
    }
    IFE1D2DNode node0 = elementEdge.getNode( 0 );
    if(node0.getContainers().size()==1)
    {
      return node0;
    }
    return null;
  }

  private final IFE1D2DNode addTargetCLNode( 
                        IFE1D2DContinuityLine cLine, IFE1D2DNode startNode )
  {
    IFeatureWrapperCollection<IFE1D2DEdge> cLineEdges = cLine.getEdges();
    final int SIZE = cLineEdges.size();
    GM_Point middlePoint;
    if((SIZE %2)==0)
    {
      //the target point taken from the end of middle left node
      IFE1D2DNode middleNode=cLineEdges.get( SIZE/2-1 ).getNode( 1 );
      middlePoint = middleNode.getPoint();
      
    }
    else
    {
      //middle of middle edge
      IFE1D2DEdge middleEdge = cLineEdges.get( (int)Math.floor( SIZE/2.0 ) );
      GM_Point point0 = middleEdge.getNode( 0 ).getPoint();
      GM_Point point1 = middleEdge.getNode( 1 ).getPoint();
      double x=(point0.getX()+point1.getX())/2;
      double y=(point0.getY()+point1.getY())/2;
      CS_CoordinateSystem crs=point0.getCoordinateSystem();
      middlePoint = GeometryFactory.createGM_Point( x, y, crs );
    }
    
    GM_Point startPoint = startNode.getPoint();
    double x=(middlePoint.getX()*9 + startPoint.getX())/10;
    double y=(middlePoint.getY()*9 + startPoint.getY())/10;
    CS_CoordinateSystem crs=startPoint.getCoordinateSystem();
    GM_Point targetPoint = 
              GeometryFactory.createGM_Point( x, y, crs );
    IFE1D2DNode targetNode = 
      model.getNodes().addNew( 
            Kalypso1D2DSchemaConstants.WB1D2D_F_NODE );
    targetNode.setPoint( targetPoint );
    //TODO check if this is not breaking the find egde computation
    
    return targetNode;
  }

  private IFE1D2DEdge findCLMiddleEdge( 
                          IFE1D2DContinuityLine continuityLine2 )
  {
      IFeatureWrapperCollection edges = 
                    continuityLine2.getEdges();
      if(edges.isEmpty())
      {
        return null;
      }
      else
      {
        int index = (int)Math.floor( edges.size()/2.0);
        return (IFE1D2DEdge) edges.get( index );
      }
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  public void redo( ) throws Exception
  {
    if(addedJunction==null)
    {
      process();
    }
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  public void undo( ) throws Exception
  {
    if(addedJunction!=null)
    {
      //TODO remove element and links to it edges
    }
  }

  /**
   * @see xp.IDiscrMode1d2dlChangeCommand#getChangedFeature()
   */
  public IFeatureWrapper[] getChangedFeature( )
  {
    return new IFeatureWrapper[]{addedJunction};
  }
  
  /**
   * @see xp.IDiscrMode1d2dlChangeCommand#getDiscretisationModel1d2d()
   */
  public IFEDiscretisationModel1d2d getDiscretisationModel1d2d( )
  {
    return model;
  }
}
