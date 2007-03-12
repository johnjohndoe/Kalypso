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
package org.kalypso.kalypsomodel1d2d.geom;

import java.util.List;

import org.kalypso.kalypsomodel1d2d.ops.EdgeOps;
import org.kalypso.kalypsomodel1d2d.schema.binding.FEEdgeToCLineJunction1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DContinuityLine;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEEdgeToCLineJunction1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEEdgeToEdgeJunction1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEJunction1D2D;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_SurfaceInterpolation;
import org.kalypsodeegree_impl.model.geometry.GM_SurfaceInterpolation_Impl;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * Provide methods to build the geometry of the 1D 2D finite 
 * element model constituants like 2D element, continuity line ... 
 * 
 * @author Patrice Congo
 *
 */
public class ModelGeometryBuilder
{
  
  
  public static final GM_Curve computeEgdeGeometry( 
              IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode> edge) throws GM_Exception
  {
    
    // REMARK: we assume here, that all nodes live in the same coordinate
    // system.
    if(edge==null)
    {
      return null;
    }
    
    final List<IFE1D2DNode> nodes=edge.getNodes();
    
    final int SIZE=nodes.size();
    if(SIZE!=2)
    {
      return null;
    }
    
    final CS_CoordinateSystem crs = 
                    nodes.get( 0 ).getPoint().getCoordinateSystem();
    
    
    GM_Position positions[]= new GM_Position[SIZE];
    GM_Point point;
    
    for( int i = 0; i < SIZE; i++ )
    {
      point = nodes.get( i ).getPoint();
      positions[i] = point.getPosition();
    }

    return GeometryFactory.createGM_Curve( positions, crs );
  }
  
  /**
   * Recalculates the geometry of this element. Used by the corresponding property function.
   */
  static public GM_Object computeElementGeometry(
              IFE1D2DElement<IFE1D2DComplexElement, IFE1D2DEdge> element) 
  {
      try
      {
        List<IFE1D2DNode> nodes=element.getNodes();
        final int SIZE=nodes.size();
        /* Positions from nodes */
        final GM_Position[] poses = new GM_Position[SIZE];
 
        if( SIZE <= 3 )
        {
          return null;
        }
 
        final CS_CoordinateSystem crs = 
          nodes.get(0).getPoint().getCoordinateSystem();
 
        for( int i = 0; i < poses.length; i++ )
        {
          final GM_Point point = nodes.get( i ).getPoint();
          poses[i] = point.getPosition();
        }
        
        return GeometryFactory.createGM_Surface( 
                        poses, 
                        new GM_Position[0][], 
                        new GM_SurfaceInterpolation_Impl( 
                                      GM_SurfaceInterpolation.PLANAR ), 
                        crs );
      }
      catch( Throwable th )
      {
        th.printStackTrace();
        return null;
      }
      
  }
  
  
    static public GM_Object computeJunction1D2DGeometry(
                                  IFEJunction1D2D<IFE1D2DComplexElement, IFE1D2DEdge> junction1D2D) throws GM_Exception 
    {
      IFeatureWrapperCollection<IFE1D2DEdge> edges = junction1D2D.getEdges();
      int size = edges.size();
      if(size==0)
      {
        return null;
      }
      else if(size==1)
      {
        return computeEgdeGeometry( edges.get( 0 ));
      }
      else
      {
        throw new RuntimeException("Support geo compute for one edge:"+edges);
        
      }
    }
  
  /**
   * Recalculates the geometry of this element. Used by the corresponding property function.
   */
  static public GM_Object computeContiniutyLineGeometry(
              IFE1D2DContinuityLine<IFE1D2DComplexElement, IFE1D2DEdge> continuityLine) 
  {
      try
      {
        //TODO this implementation is from element, check if its work for CLs
        List<IFE1D2DNode> nodes=continuityLine.getNodes();
        
        final int SIZE=nodes.size();
        
        final GM_Position[] poses = new GM_Position[SIZE];
 
        if( SIZE <= 1 )
        {
          return null;
        }
 
        final CS_CoordinateSystem crs = 
          nodes.get(0).getPoint().getCoordinateSystem();
 
        for( int i = 0; i < poses.length; i++ )
        {
          final GM_Point point = nodes.get( i ).getPoint();
          poses[i] = point.getPosition();
        }
        
        GM_Curve curve = GeometryFactory.createGM_Curve( poses, crs );
        
//        GM_LineString lineString= curve.getAsLineString();
        return curve;
//        return GeometryFactory.createGM_Surface( 
//                        poses, 
//                        new GM_Position[0][], 
//                        new GM_SurfaceInterpolation_Impl( 
//                                      GM_SurfaceInterpolation.PLANAR ), 
//                        crs );
      }
      catch( Throwable th )
      {
        th.printStackTrace();
        return null;
      }
      
  }
  
  public static final GM_Curve computeElement1DGeometry( 
              IElement1D element1D) throws GM_Exception
  {
    Assert.throwIAEOnNullParam( element1D, "element1D" );
    return computeEgdeGeometry( element1D.getEdge() );
  }

  public static GM_Object computeEdgeToEdgeJunction1D2DGeometry( IFEEdgeToEdgeJunction1D2D junction1D2D ) throws GM_Exception
  {
    IFE1D2DEdge edge1D = junction1D2D.get1DEdge();
    IFE1D2DEdge edge2D = junction1D2D.get2DEdge();
    if(edge1D==null || edge2D==null)
    {
      return null;
    }
    IFE1D2DNode<IFE1D2DEdge>  borderNode = 
                  EdgeOps.find1DEdgeEndNode( edge1D );
    if(borderNode==null)
    {
      throw new IllegalStateException(
          "The edge 1d does not have a border node");
    }
    
    IFE1D2DNode<IFE1D2DEdge> startNode = 
                  EdgeOps.find1DEdgeEndNode( edge1D );
    GM_Point startPoint = startNode.getPoint();
    GM_Point targetPoint = EdgeOps.computeEdgeMiddle( edge2D);
    
    GM_Position positions[]= new GM_Position[2];
    
    
    positions[0] = startPoint.getPosition();
    positions[1] = targetPoint.getPosition();
    
    return GeometryFactory.createGM_Curve( positions, targetPoint.getCoordinateSystem() );
    
  }

  public static final GM_Object computeEdgeToCLineJunction1D2DGeometry( FEEdgeToCLineJunction1D2D junction1D2D ) throws GM_Exception
  {
    Assert.throwIAEOnNullParam( junction1D2D, "junction1D2D" );
    IFE1D2DEdge edge = junction1D2D.getEdge();
    if(edge==null)
    {
      return null;
    }
    return computeEgdeGeometry( edge );
  }
  
  public static final GM_Object computeEdgeToCLineJunction1D2DGeometry( IFEEdgeToCLineJunction1D2D junction1D2D ) throws GM_Exception
  {
    Assert.throwIAEOnNullParam( junction1D2D, "junction1D2D" );
    IFE1D2DEdge edge = junction1D2D.getEdge();
    if(edge==null)
    {
      return null;
    }
    return computeEgdeGeometry( edge );
  }
}
