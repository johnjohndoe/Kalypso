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

import java.util.List;

import org.kalypso.commons.command.ICommand;
import org.kalypso.kalypsomodel1d2d.schema.binding.FE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypso.kalypsosimulationmodel.core.discr.IFEElement;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Command For deleting 1D element
 * 
 * @author Patrice Congo
 */
public class DeleteElement1DCmd implements IDiscrModel1d2dChangeCommand
{
  
  private IFEDiscretisationModel1d2d model1d2d;
  private IElement1D element1D;
  private GM_Point[] elementPoints;
  
  private IFE1D2DComplexElement[] complexElements;
  
  public DeleteElement1DCmd(
                      IFEDiscretisationModel1d2d model1d2d,
                      Feature element2DFeature)
  {
    this.model1d2d = model1d2d;
    this.element1D = 
        (IElement1D)element2DFeature.getAdapter( IElement1D.class );
  }
  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  public String getDescription( )
  {
    return "Delete element 1D";
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
    String elementID = element1D.getGmlID();
    
    List<IFE1D2DNode> nodes = element1D.getNodes();
    
    elementPoints = makeElementPoints(nodes);
    
    complexElements = getElementComplexElement();
    
    IFE1D2DEdge edge = makeElementEdges();
    
    deleteElement(elementID,edge,complexElements);
    
    deleteEdges(edge,elementID);
    
  }

  
  private void deleteEdges( IFE1D2DEdge edge, String elementID )
  {
    RemoveEdgeWithoutContainerOrInvCmd remEdgeCmd=
          new RemoveEdgeWithoutContainerOrInvCmd(model1d2d,null);
//    for(IFE1D2DEdge edge:edges)
//    {
      try
      {
        
        remEdgeCmd.setEdgeToDel( edge );
        remEdgeCmd.process();
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
//    }
  }
  
  private void deleteElement( 
                    String elementID, 
                    IFE1D2DEdge edge, 
                    IFE1D2DComplexElement[] complexElements )
  {
    //delete link to complex elements
    for(IFE1D2DComplexElement complexElement:complexElements)
    {
     complexElement.getElements().remove( elementID ); 
    }
    
    //delete link to edges
      IFeatureWrapperCollection containers = edge.getContainers();
      boolean isRemoved = containers.remove( elementID );
      if(!isRemoved)
      {
        throw new RuntimeException("Could not remove element as edge container");
      }    
    //delete element from model
    model1d2d.getElements().remove( element1D );
    
  }
  
  private IFE1D2DEdge makeElementEdges( )
  {
    IFE1D2DEdge edge = element1D.getEdge();    
    return edge;
  }
  
  private IFE1D2DComplexElement[] getElementComplexElement( )
  {
    IFeatureWrapperCollection containers = element1D.getContainers();
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
    return new IFeatureWrapper[]{element1D};
  }
  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand#getDiscretisationModel1d2d()
   */
  public IFEDiscretisationModel1d2d getDiscretisationModel1d2d( )
  {
    return model1d2d;
  }
  
}
