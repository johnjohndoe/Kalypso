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

import java.util.Collection;

import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IBoundaryLine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IEdgeInv;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IJunctionContext1DTo2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IJunctionContext1DToCLine;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;

/**
 * Algorithms around the junction context type:
 * <ul>
 *      <li> create from edge selection
 * </ul>
 * 
 * 
 * @author Patrice Congo
 *
 */
@SuppressWarnings( {"unchecked", "hiding"} )
public class JunctionContextOps
{
  private JunctionContextOps( )
  {
    //I do not like to instantiated
  }

  
  public static IJunctionContext1DToCLine create1DContextToCLineJunctionContext( 
      IFEDiscretisationModel1d2d model1d2d, 
      IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode> edge1D, 
      Collection<IFE1D2DEdge> edge2DList )
  {
    Assert.throwIAEOnCollectionNullOrHasNullElements( edge2DList, "edge2DList" );
    //check 2d edge for being a border edge
    for(IFE1D2DEdge edge2D: edge2DList)
    {
      if(!TypeInfo.isBorderEdge( edge2D ))
      {
        String message = 
        String.format( 
        "Edge 2d list must not only border edge but this edge[id = %s] is not a border edge", 
        edge2D.getGmlID() );      
        throw new IllegalArgumentException(message);
      }
    }
    
    //check 1dedge for being a real 1d edge and having a junctable point
    if(!TypeInfo.is1DEdge( edge1D ))
    {
      String message = 
      String.format( "Parameter edge1D must be an an 1d edge=", edge1D );
      throw new IllegalArgumentException(message);
    }
    
    if(EdgeOps.find1DEdgeEndNode( edge1D )==null)
    {
      String message = 
      String.format( "Parameter edge1d must have a connectable node:\n\tedge1D=%s", edge1D );
      throw new IllegalArgumentException(message);
    }
    
    IFE1D2DElement<IFE1D2DComplexElement, IFE1D2DEdge> element1D = edge1D.getContainers().get( 0 );
    IFeatureWrapperCollection<IFE1D2DElement> elements = model1d2d.getElements();
    
    //create continuity line to hold the element
    IFE1D2DEdge[] edge2DArray = edge2DList.toArray( new IFE1D2DEdge[edge2DList.size()] ); 
    IBoundaryLine<IFE1D2DComplexElement, IFE1D2DEdge> cLine = 
      getFirstContainerBoundaryLine( edge2DArray );
    if( cLine == null )
    {
      cLine = elements.addNew( 
        Kalypso1D2DSchemaConstants.WB1D2D_F_BOUNDARY_LINE, 
        IBoundaryLine.class );
        cLine.setEdges(edge2DArray );
    }
    
    //
    IFeatureWrapperCollection<IFE1D2DComplexElement> cElements = 
    model1d2d.getComplexElements();
    IJunctionContext1DToCLine jc1d2d = 
      cElements.addNew( 
        Kalypso1D2DSchemaConstants.WB1D2D_F_JUNTCION_CONTEXT_1D_CLINE,
        IJunctionContext1DToCLine.class );
    
    jc1d2d.addElementAsRef( element1D );
    jc1d2d.addElementAsRef( cLine );
    
    return jc1d2d;
    
  }
  
  
  public static final IJunctionContext1DTo2D createEdgeToEdgeJunctionContext(
                  IFEDiscretisationModel1d2d model1d2d,
                  IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode> edge1D,
                  IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode> edge2D)
  {
    //check 2d edge for being a border edge
    if(!TypeInfo.isBorderEdge( edge2D ))
    {
    String message = 
    String.format( 
    "Edge 2d must be a border edge: \n\tedge=", 
    edge2D );      
    throw new IllegalArgumentException(message);
    }
    
    //check 1dedge for being a real 1d edge and having a junctable point
    if(!TypeInfo.is1DEdge( edge1D ))
    {
    String message = 
    String.format( "Parameter edge1D must be an an 1d edge=", edge1D );
    throw new IllegalArgumentException(message);
    }
    
    if(EdgeOps.find1DEdgeEndNode( edge1D )==null)
    {
    String message = 
    String.format( "Parameter edge1d must have a connectable node:\n\tedge1D=%s", edge1D );
    throw new IllegalArgumentException(message);
    }
    
    IFE1D2DElement<IFE1D2DComplexElement, IFE1D2DEdge> element2D = edge2D.getContainers().get( 0 );
    IFE1D2DElement<IFE1D2DComplexElement, IFE1D2DEdge> element1D = edge1D.getContainers().get( 0 );
    IFeatureWrapperCollection<IFE1D2DElement> elements = model1d2d.getElements();
    
    //get a continuity line containing containing the 2Egde
    //or create continuity line to hold it
    IBoundaryLine<IFE1D2DComplexElement, IFE1D2DEdge> cLine = 
                                getFirstContainerBoundaryLine( edge2D );
//    found_container_cline: for(IFE1D2DElement ele : edge2D.getContainers())
//    {
//      if( ele instanceof IBoundaryLine )
//      {
//        cLine = (IBoundaryLine) ele;
//        break found_container_cline;
//      }
//    }
    if( cLine == null )
    {
      cLine = 
      elements.addNew( 
        Kalypso1D2DSchemaConstants.WB1D2D_F_BOUNDARY_LINE, 
        IBoundaryLine.class );
      cLine.setEdges( new IFE1D2DEdge[]{edge2D} );
    }
    
    //
    IFeatureWrapperCollection<IFE1D2DComplexElement> cElements =  
                                model1d2d.getComplexElements();
    IJunctionContext1DTo2D jc1d2d = 
    cElements.addNew( 
          Kalypso1D2DSchemaConstants.WB1D2D_F_JUNTCION_CONTEXT_1D_2D,
          IJunctionContext1DTo2D.class );
    
    jc1d2d.addElementAsRef( element1D);
    jc1d2d.addElementAsRef( element2D);
    jc1d2d.addElementAsRef( cLine);
    
    return jc1d2d;
  }

  /**
   * 
   */
  private static final IBoundaryLine getFirstContainerBoundaryLine(
                            IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode>[] edges2D )
  {
    for( IFE1D2DEdge edge : edges2D )
    {
      IBoundaryLine line = getFirstContainerBoundaryLine( edge );
      if( line != null )
      {
        return line;
      }
    }
    return null;
  }
  
  private static final IBoundaryLine getFirstContainerBoundaryLine(
                                        IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode> edge2D )
  {
    Assert.throwIAEOnNullParam( edge2D, "edge2D" );
    for(IFE1D2DElement ele : edge2D.getContainers())
    {
      if( ele instanceof IBoundaryLine )
      {
        return (IBoundaryLine) ele;        
      }
    }
    
    final IEdgeInv edgeInv = edge2D.getEdgeInv();
    if( edgeInv != null )
    {
      for(Object ele : edgeInv.getContainers())
      {
        if( ele instanceof IBoundaryLine )
        {
          return (IBoundaryLine) ele;        
        }
      }
    }
    
    return null;
  }
}
