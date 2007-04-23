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
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.FE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEEdgeToCLineJunction1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEEdgeToEdgeJunction1D2D;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Command For deleting an edge to edge junction element
 * 
 * @author Patrice Congo
 */
public class DeleteEdgeToCLineJunctionElementCmd implements IDiscrModel1d2dChangeCommand
{
  
  private IFEDiscretisationModel1d2d model1d2d;
  private IFEEdgeToCLineJunction1D2D junction;
  
  /**
   * Contains 4 points the first 2 points are the end and start
   * of the 1d edge and  the last 2 point are the start and
   * end of the 2d second 2d edge
   */
  private GM_Point[] elementPoints;
  
  private IFE1D2DComplexElement[] complexElements;
  
  public DeleteEdgeToCLineJunctionElementCmd(
                      IFEDiscretisationModel1d2d model1d2d,
                      Feature junctionFeature)
  {
    this.model1d2d = model1d2d;
    this.junction = 
        (IFEEdgeToCLineJunction1D2D)
            junctionFeature.getAdapter( IFEEdgeToCLineJunction1D2D.class );
  }
  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  public String getDescription( )
  {
    return "Delete element 1d to continuity line junction element 2D";
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
    String elementID=junction.getGmlID();
    
    List<IFE1D2DNode> nodes = new ArrayList<IFE1D2DNode>();//junction.getNodes();
    nodes.addAll( junction.getEdge().getNodes() );
    
    elementPoints = makeElementPoints(nodes);
    
    complexElements = getElementComplexElement();
    
    IFE1D2DEdge edges[]= makeElementEdges();
    
    deleteElement(elementID,edges,complexElements);
    
    deleteEdges(edges,elementID);
    
  }

  
  private void deleteEdges( IFE1D2DEdge[] edges, String elementID )
  {
    RemoveEdgeWithoutContainerOrInvCmd remEdgeCmd=
          new RemoveEdgeWithoutContainerOrInvCmd(model1d2d,null);
    for(IFE1D2DEdge edge:edges)
    {
      try
      {
        
        remEdgeCmd.setEdgeToDel( edge );
        remEdgeCmd.process();
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
    }
  }
  
  private void deleteElement( String elementID, IFE1D2DEdge[] edges, IFE1D2DComplexElement[] complexElements )
  {
    //delete link to complex elements
    for(IFE1D2DComplexElement complexElement:complexElements)
    {
     complexElement.getElements().remove( elementID ); 
    }
    
    //delete link to edges
    for(IFE1D2DEdge edge : edges)
    {
      IFeatureWrapperCollection containers = edge.getContainers();
      boolean isRemoved = containers.remove( elementID );
      if(!isRemoved)
      {
        throw new RuntimeException("Could not remove element as edge container");
      }
    }
    
    //delete element from model
    model1d2d.getElements().remove( junction );
    
  }
  
  private IFE1D2DEdge[] makeElementEdges( )
  {
    
    IFE1D2DEdge[] edgeArray =
        new IFE1D2DEdge[]{junction.getEdge()};
    return edgeArray;
  }
  
  private IFE1D2DComplexElement[] getElementComplexElement( )
  {
    IFeatureWrapperCollection containers = junction.getContainers();
    IFE1D2DComplexElement[] cElements = 
        (IFE1D2DComplexElement[]) containers.toArray( new IFE1D2DComplexElement[]{} );
    return cElements;
  }
  
  private GM_Point[] makeElementPoints(List<IFE1D2DNode> nodes )
  {
    int SIZE = nodes.size();
    GM_Point[] points=new GM_Point[SIZE];
    GM_Position nodePos;
    for(int i=SIZE-1;i>0;i--)
    {
      GM_Point point = nodes.get( i ).getPoint();
      nodePos=point.getPosition();
      double[] xyz = nodePos.getAsArray();
      int dimension = xyz.length;//getDimension();
      if(dimension==2)
      {
        points[i]= GeometryFactory.createGM_Point( 
            xyz[0], xyz[1], 
            point.getCoordinateSystem() );
      }
      else if(dimension==3)
      {
        points[i]= GeometryFactory.createGM_Point( 
                          xyz[0], xyz[1],xyz[2], 
                          point.getCoordinateSystem() ); 
      }
      else
      {
        throw new RuntimeException(
                      "Unsupported gm point dimension:"+dimension+
                      " "+nodePos);
      }
    }
    return points;
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
  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand#getChangedFeature()
   */
  public IFeatureWrapper[] getChangedFeature( )
  {
    return new IFeatureWrapper[]{junction};
  }
  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand#getDiscretisationModel1d2d()
   */
  public IFEDiscretisationModel1d2d getDiscretisationModel1d2d( )
  {
    return model1d2d;
  }
  
}
