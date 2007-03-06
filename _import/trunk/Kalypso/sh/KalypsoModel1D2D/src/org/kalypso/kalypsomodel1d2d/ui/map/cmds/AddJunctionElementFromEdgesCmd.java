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


import org.kalypso.kalypsomodel1d2d.ops.ModelOps;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.FE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DContinuityLine;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEJunction1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEMiddleNode;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * Add junction element from 1D and 2D edge
 * 
 * 
 * @author Patrice Congo
 */
public class AddJunctionElementFromEdgesCmd implements IDiscrModel1d2dChangeCommand
{
  private IFE1D2DEdge edge1D;
  private IFE1D2DEdge edge2D;
  
  private IFEDiscretisationModel1d2d model;
  
  
  private IFEJunction1D2D addedJunction;
  
  /**
   * @param model
   * @param elementEdgeCmds an array the command used to create the edges of the element to be created
   *  by this command. the array must contains only {@link AddEdgeCommand} and 
   *    {@link AddEdgeInvCommand} commands
   */
  public AddJunctionElementFromEdgesCmd(
              IFEDiscretisationModel1d2d model,
              IFE1D2DEdge edge1D,
              IFE1D2DEdge edge2D
              )
  {
    
    Assert.throwIAEOnNullParam( model, "model" );
    Assert.throwIAEOnNullParam( edge1D, "edge1D" );
    Assert.throwIAEOnNullParam( edge2D, "edge2D" );
    this.model=model;
    this.edge2D=edge2D;
    this.edge1D=edge1D;
    
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
        IFEMiddleNode targetClNodel = addTargetCLNode(edge2D);
        
        IFE1D2DNode<IFE1D2DEdge> node0 = 
                          findJunctionStartNode(edge1D);
        
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

  private final IFEMiddleNode addTargetCLNode( 
                        IFE1D2DEdge clMiddleEdge )
  {
    IFE1D2DNode node0 = clMiddleEdge.getNode( 0 );
    IFE1D2DNode node1 = clMiddleEdge.getNode( 1 );
    GM_Point point0 = node0.getPoint();
    GM_Point point1 = node1.getPoint();
    double x=(point0.getX()+point1.getX())/2;
    double y=(point0.getY()+point1.getY())/2;
    CS_CoordinateSystem crs=point0.getCoordinateSystem();
    GM_Point middleNodePoint = 
              GeometryFactory.createGM_Point( x, y, crs );
    IFEMiddleNode middleNode = 
      (IFEMiddleNode) model.getNodes().addNew( 
            Kalypso1D2DSchemaConstants.WB1D2D_F_MIDDLE_NODE );
    middleNode.setPoint( middleNodePoint );
    //TODO check if this is not breaking the find egde computation
    middleNode.addContainer( clMiddleEdge.getGmlID() );
    return middleNode;
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
