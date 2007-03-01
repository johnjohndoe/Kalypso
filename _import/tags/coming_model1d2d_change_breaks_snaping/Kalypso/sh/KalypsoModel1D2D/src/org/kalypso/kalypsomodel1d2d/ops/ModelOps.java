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
package org.kalypso.kalypsomodel1d2d.ops;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.FE1D2DDiscretisationModel;
import org.kalypso.kalypsomodel1d2d.schema.binding.FE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.FE1D2D_2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.IEdgeInv;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Point;
@SuppressWarnings("unchecked")
/**
 * (static) helper functions for the {@link org.kalypso.kalypsomodel1d2d.schema.binding.FE1D2DDiscretisationModel}
 * class.
 * 
 * @author Gernot Belger
 * @author Thomas Jung
 */
public class ModelOps
{
  private ModelOps( )
  {
    // never instatiate
  }

  public static IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode>[] routing( 
                                  final /*FE1D2DNode*/IFE1D2DNode startNode, 
                                  final /*FE1D2DNode*/IFE1D2DNode endNode ) 
                                  throws CoreException
  {
    final boolean doTrace = Boolean.parseBoolean( Platform.getDebugOption( "KalypsoModel1D2D/debug/ops/continuity/routing" ) );

    final List<IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode>> edgeList = 
                new ArrayList<IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode>>();

    try
    {
      final Point endPoint = 
          (Point) JTSAdapter.export( endNode.getPoint() );

      final FE1D2DDiscretisationModel model = 
          new FE1D2DDiscretisationModel( 
                startNode.getWrappedFeature().getParent() );
      
      final int maxNodeCount = model.getNodes().size();

      IFE1D2DNode lastFoundNode = startNode;
      int count = 0;
      while( count < maxNodeCount && !lastFoundNode.equals( endNode ) )
      {
        count++; // Max number of iterations is the current node count

        if( doTrace )
        {
          System.out.println( "START: Routing" );
          System.out.println( "Current node: " + lastFoundNode.getWrappedFeature().getId() );
        }

        // alle benachbarten elemente des aktuellen knoten
        final Collection<IFE1D2DNode> neighbourNodes = lastFoundNode.getNeighbours( );

        if( neighbourNodes.size()/*length*/ == 0 )
        {
          final IStatus status = StatusUtilities.createErrorStatus( "No good nodes found for node:" + lastFoundNode.getWrappedFeature().getId() );
          throw new CoreException( status );
        }
        final Collection<IFE1D2DNode> neighbourNodeList = neighbourNodes;//Arrays.asList( neighbourNodes );

        // find suitable edge
        IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode> shortestFoundEdge = null;
        double minEdgeLength = Double.MAX_VALUE;

        final IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode>[] edges = lastFoundNode.getEdges();
        for( IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode> edge : edges )
        {
          if( edgeList.contains( edge ) )
          {
            continue;
          }

          // get opposite node (not me)
          final IFeatureWrapperCollection<IFE1D2DNode> nodes = edge.getNodes();
          if( nodes.size() == 2 )
          {
            final IFE1D2DNode oppositeNode;
            if( nodes.get( 0 ).equals( lastFoundNode ) )
            {
              oppositeNode = nodes.get( 1 );
            }
            else
            {
              //TODO cope with direction here by finding the right edge
              System.out.println();
              oppositeNode = nodes.get( 0 );
              edge=model.findEdge( lastFoundNode, oppositeNode );
            }

            if( neighbourNodeList.contains( oppositeNode ) )
            {
              // the current best edge is the one, whichs endpoint is nearest to the target, intersect the line segment
              // and was not encountered yet
              final Point oppositePoint = (Point) JTSAdapter.export( oppositeNode.getPoint() );
              final double length = oppositePoint.distance( endPoint );
              if( length < minEdgeLength )
              {
                shortestFoundEdge = edge;
                minEdgeLength = length;
              }
            }
          }
        }

        // if we have a shortest edge, use it!
        if( shortestFoundEdge != null )
        {
          edgeList.add( shortestFoundEdge );

          final IFeatureWrapperCollection<IFE1D2DNode> nodes = 
            (shortestFoundEdge instanceof IEdgeInv)?
                  ((IEdgeInv)shortestFoundEdge).getInverted().getNodes():shortestFoundEdge.getNodes();
          if( nodes.size() == 2 )
          {
            if( nodes.get( 0 ).equals( lastFoundNode ) )
              lastFoundNode = nodes.get( 1 );
            else
              lastFoundNode = nodes.get( 0 );
          }
          else
          {
            final IStatus status = StatusUtilities.createErrorStatus( "Edge with nodeCount != 2 encountered." );
            throw new CoreException( status );
          }
        }
        else
        {
          final IStatus status = StatusUtilities.createErrorStatus( "No shortest edge found for node: " + lastFoundNode.getWrappedFeature().getId() );
          throw new CoreException( status );
        }
      }

      if( count == maxNodeCount )
      {
        final IStatus status = StatusUtilities.createErrorStatus( "Routing failed, no path found." );
        throw new CoreException( status );
      }

    }
    catch( final GM_Exception e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      throw new CoreException( status );
    }

    if( doTrace )
    {
      System.out.println( "EndPoint reached: " + endNode.getWrappedFeature().getId() );
      System.out.println( "FINISHED: Routing" );
    }

    return edgeList.toArray( new IFE1D2DEdge[edgeList.size()] );

  }
  
  public static final void sortElementEdgesOLD(IFE1D2DElement element)
  {
    List<IFE1D2DEdge> edges= 
                   new ArrayList<IFE1D2DEdge>(
                       element.getEdges()/*Arrays.asList( 
                           ((FE1D2D_2DElement)element).getEdgesAsArray())*/);
    Comparator<IFE1D2DEdge> c=new Comparator<IFE1D2DEdge>()
    {

      /**
       * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
       */
      
      public int compare( IFE1D2DEdge edge1, IFE1D2DEdge edge2 )
      {
//        System.out.println("EDge1="+edge1+" edge2"+edge2);
//        if((edge1 instanceof IEdgeInv) && !(edge2 instanceof IEdgeInv))
//        {
//          System.out.println("EEEEDge1="+edge1+" edge2"+edge2);
//          return -1;
//        }
//        else if(!(edge1 instanceof IEdgeInv) && (edge2 instanceof IEdgeInv))
//        {
//          System.out.println("EDge1="+edge1+" edge2"+edge2);
//          return +1;
//        } 
        
        IFE1D2DNode<IFE1D2DEdge> node0_1=edge1.getNode( 0 ); 
        IFE1D2DNode<IFE1D2DEdge> node0_2=edge2.getNode( 0 );
        
        if(node0_1.getPoint().getX()<node0_2.getPoint().getX())
        {
          return -1;
        }
        else if(node0_1.getPoint().getX()>node0_2.getPoint().getX())
        {
          return 1;
        }
        else
        {
          //same x location consider y location
          if(node0_1.getPoint().getY()<node0_2.getPoint().getY())
          {
            return -1;
          }
          else if(node0_1.getPoint().getY()>node0_2.getPoint().getY())
          {
            return 1;
          }
          else
          {
            return 0;
          }
        }        
      }
      
    };
    element.getEdges().clear();
    Collections.sort( edges, c );
    for(IFE1D2DEdge edge:edges)
    {
      element.addEdge( edge.getWrappedFeature().getId() );
    }
    
  }
  
  public static final IFE1D2DElement createElement2d(
                              IFEDiscretisationModel1d2d model1d2d,
                              List<IFE1D2DEdge> edges)
  {
    Assert.throwIAEOnNullParam( model1d2d, "model1d2d" );
    Assert.throwIAEOnNullParam( edges, "edges" );
    final int EDGE_NUM=edges.size();
    if(!(EDGE_NUM == 3 || EDGE_NUM == 4))
    {
      throw new IllegalArgumentException(
          "2D element must have 3 or 4 element but number "+
          "of edges to set="+EDGE_NUM);
    }
    
    IFeatureWrapperCollection<IFE1D2DElement> elements = 
                                        model1d2d.getElements();
    IFE1D2DElement element = elements.addNew( 
          Kalypso1D2DSchemaConstants.WB1D2D_F_POLY_ELEMENT);
    for(IFE1D2DEdge edge:edges)
    {
      element.addEdge( edge.getGmlID() );
    }
    
//    sortElementEdgesOld( element );
//    sortEdgesAddToElement( element, edges );
    sortElementEdges( element );
    String elementID = element.getGmlID();
    for(IFE1D2DEdge edge:edges)
    {
      edge.addContainer( elementID );
    }
    
    return element;
    
  }
  
  public static final void sortEdgesAddToElement(
                                  IFE1D2DElement element,
                                  List<IFE1D2DEdge> toSortAndAddEdges)
  {
    IFeatureWrapperCollection<IFE1D2DEdge> elementEdges=element.getEdges();
    final int INITIAL_SIZE=toSortAndAddEdges.size();
    if(INITIAL_SIZE<3)
    {
      String str=
        "Illegal2D element:"+element.getGmlID()+
        " edgeCount="+INITIAL_SIZE;
//      throw new IllegalStateException(str);
      System.out.println(str);
      return;
    }
    List<IFE1D2DEdge> edges=
          new ArrayList<IFE1D2DEdge>(toSortAndAddEdges);
    
    //clear old edge for reordering
    elementEdges.clear();
    
    FeatureList edgeFeatureList=elementEdges.getWrappedList();
    
//  just select the first node
    IFE1D2DEdge edge=edges.remove(0);    
    edgeFeatureList.add( edge.getGmlID() );
    for(int i=0; edges.size()>0;)
    {
      
      IFE1D2DNode nodeEnd=edge.getNode( 1 );
      i=edges.size()-1;
      for(;i>=0;i--)
      {
        if( nodeEnd.getGmlID().equals( edges.get( i ).getNode( 0 ).getGmlID()) )
        {
          break;
        }
      }
      if(i==-1)
      {
        ///no following not found ordering ends
        return;
      }
      else
      {
        edge=edges.remove( i );
        edgeFeatureList.add( edge.getGmlID() );
      }
    }
    
    
  }

  public static final void sortElementEdges(IFE1D2DElement element)
  {
//    sortElementEdgesOld( element );
    IFeatureWrapperCollection<IFE1D2DEdge> elementEdges=element.getEdges();
    final int INITIAL_SIZE=elementEdges.size();
    if(INITIAL_SIZE<3)
    {
      String str=
        "Illegal2D element:"+element.getGmlID()+" edgeCount="+INITIAL_SIZE;
//      throw new IllegalStateException(str);
      System.out.println(str);
      return;
    }
    List<IFE1D2DEdge> edges=
          new ArrayList<IFE1D2DEdge>(element.getEdges());
    
    
    //clear old edge for reordering
    elementEdges.clear();
    
    FeatureList edgeFeatureList=elementEdges.getWrappedList();
    
//  just select the first node
    IFE1D2DEdge edge=edges.remove(0);
    edgeFeatureList.add( edge.getGmlID() );
    int SIZE=edges.size();
    for(int i=0; SIZE>0;SIZE=edges.size())
    {
      
      IFE1D2DNode nodeEnd=edge.getNode( 1 );
      
      findingNextNode: for(int j=0;j<SIZE;j++)
      {
        IFE1D2DEdge nextEdge=edges.get( j );//:endNodeEdges
        if(NodeOps.startOf(nodeEnd,nextEdge))
        {
          edge=nextEdge;
          break findingNextNode; 
        }
        
      }
      
      if(edge==null)
      {
        throw new RuntimeException("Could not fround next edge:"+
                  "\n\tnode:"+nodeEnd+
                  "\n\tedges="+edges);
      }
      else
      {
        if(edges.remove( edge ))
        {
          edgeFeatureList.add( edge.getGmlID() );
        }
        else
        {
          throw new RuntimeException(
              "edge not in list:"+
              "\n\tedge="+edge+
              "\n\tlist:"+edges);
        }
      }
      
//      i=edges.size()-1;
//      for(;i>=0;i--)
//      {
//        IFE1D2DEdge edge2 = edges.get( i );
//        if( nodeEnd.getGmlID().equals( edge2.getNode( 0 ).getGmlID()) )
//        {
//          break;
//        }
//        else
//        {
//          continue;
//        }
//      }
//      
//      if(i==-1)
//      {
//        ///no following not found ordering ends
//        return;
//      }
//      else
//      {
//        edge=edges.remove( i );
//        edgeFeatureList.add( edge.getGmlID() );
//      }
    }
    
    
  }
  
  
}