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
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IEdgeInv;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.binding.FeatureWrapperCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

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

  public static final IElement1D createElement1d( IFEDiscretisationModel1d2d model1d2d, IFE1D2DEdge edge )
  {
    final IFeatureWrapperCollection<IFE1D2DElement> elements = model1d2d.getElements();
    final IElement1D element = elements.addNew( Kalypso1D2DSchemaConstants.WB1D2D_F_ELEMENT1D, IElement1D.class );

    element.setEdge( edge );

    return element;
  }

  public static final IPolyElement createElement2d( IFEDiscretisationModel1d2d model1d2d, List<IFE1D2DEdge> edges )
  {
    Assert.throwIAEOnNullParam( model1d2d, "model1d2d" );
    Assert.throwIAEOnNullParam( edges, "edges" );
    final int EDGE_NUM = edges.size();
    if( !(EDGE_NUM == 3 || EDGE_NUM == 4) )
    {
      throw new IllegalArgumentException( "2D element must have 3 or 4 element but number " + "of edges to set=" + EDGE_NUM );
    }

    IFeatureWrapperCollection<IFE1D2DElement> elements = model1d2d.getElements();
    final IPolyElement polyElement = elements.addNew( Kalypso1D2DSchemaConstants.WB1D2D_F_POLY_ELEMENT, IPolyElement.class );

    sortElementEdges( polyElement, edges );
    String elementID = polyElement.getGmlID();
    for( IFE1D2DEdge edge : edges )
    {
      edge.addContainer( elementID );
    }

    return polyElement;

  }

  public static final void sortElementEdges( final IFE1D2DElement element )
  {
    // TODO: not every IFE1D2DElement is a IElement2D!!! What to do?
    if( !(element instanceof IElement2D) )
      return;
    final IFeatureWrapperCollection edges = ((IElement2D) element).getEdges();
    final List<IFE1D2DEdge> toSort = new ArrayList<IFE1D2DEdge>( edges );
    edges.clear();
    sortElementEdges( (IElement2D) element, toSort );
  }

  public static final void sortElementEdges( IElement2D element, List<IFE1D2DEdge> toSortAndAddEdges )
  {
    // sortElementEdgesOld( element );
    IFeatureWrapperCollection<IFE1D2DEdge> elementEdges = element.getEdges();
    final int INITIAL_SIZE = toSortAndAddEdges.size();// elementEdges.size();
    if( INITIAL_SIZE < 3 )
    {
      return;
      // String str = "Illegal2D element:" + element.getGmlID() + " edgeCount=" + INITIAL_SIZE;
      // throw new IllegalStateException( str );
      // return;
    }
    List<IFE1D2DEdge> edges = new ArrayList<IFE1D2DEdge>( toSortAndAddEdges );// element.getEdges());

    // clear old edge for reordering
    elementEdges.clear();

    FeatureList edgeFeatureList = elementEdges.getWrappedList();

    // just select the first node
    IFE1D2DEdge edge = edges.remove( 0 );
    edgeFeatureList.add( edge.getGmlID() );
    int SIZE = edges.size();
    for( ; SIZE > 0; SIZE = edges.size() )
    {

      IFE1D2DNode nodeEnd = edge.getNode( 1 );

      findingNextNode: for( int j = 0; j < SIZE; j++ )
      {
        IFE1D2DEdge nextEdge = edges.get( j );// :endNodeEdges
        if( NodeOps.startOf( nodeEnd, nextEdge ) )
        {
          edge = nextEdge;
          break findingNextNode;
        }

      }

      if( edge == null )
      {
        throw new RuntimeException( "Could not found next edge:" + "\n\tnode:" + nodeEnd + "\n\tedges=" + edges );
      }
      else
      {
        if( edges.remove( edge ) )
        {
          edgeFeatureList.add( edge.getGmlID() );
        }
        else
        {
          throw new RuntimeException( "edge not in list:" + "\n\tedge=" + edge + "\n\tlist:" + edges );
        }
      }
    }

  }

  /**
   * Answer whether the edge is contained by a fe element. This is the when if its is directely contains in an element
   * or in the case of a normal edge through its edge inv
   * 
   * @param egde
   *            the edge to test
   * @return true if the given edge is in an element
   */
  public static final boolean isContainedInAnElement( IFE1D2DEdge edge )
  {
    if( !edge.getContainers().isEmpty() )
    {
      return true;
    }
    if( edge instanceof IEdgeInv )
    {
      return false;
    }
    else
    {
      IEdgeInv edgeInv = edge.getEdgeInv();
      if( edgeInv == null )
      {
        return false;
      }
      if( edgeInv.getContainers().isEmpty() )
      {
        return false;
      }
      return true;
    }
  }

  public static final IFeatureWrapperCollection<IFE1D2DComplexElement> getElementContainer( Feature featureToBind )
  {
    Object prop = null;
    try
    {
      prop = featureToBind.getProperty( Kalypso1D2DSchemaConstants.WB1D2D_PROP_ELEMENT_CONTAINERS );
    }
    catch( Throwable th )
    {
      th.printStackTrace();
    }

    FeatureWrapperCollection<IFE1D2DComplexElement> containers;
    if( prop == null )
    {
      // create the property tha is still missing
      containers = new FeatureWrapperCollection<IFE1D2DComplexElement>( featureToBind, Kalypso1D2DSchemaConstants.WB1D2D_F_COMPLEX_ELE_2D, Kalypso1D2DSchemaConstants.WB1D2D_PROP_ELEMENT_CONTAINERS, IFE1D2DComplexElement.class );
    }
    else
    {
      // just wrapped the existing one
      containers = new FeatureWrapperCollection<IFE1D2DComplexElement>( featureToBind, IFE1D2DComplexElement.class, Kalypso1D2DSchemaConstants.WB1D2D_PROP_ELEMENT_CONTAINERS );
    }
    return containers;
  }

  public static final Collection<IFE1D2DEdge> collectAll2DEdges( Feature[] selectedFeatures )
  {
    Set<IFE1D2DEdge> selected2DEdges = new HashSet<IFE1D2DEdge>();
    for( Feature feature : selectedFeatures )
    {
      if( IFE1D2DEdge.QNAME.equals( feature.getFeatureType().getQName() ) )
      {
        IFE1D2DEdge edge = (IFE1D2DEdge) feature.getAdapter( IFE1D2DEdge.class );
        if( TypeInfo.is2DEdge( edge ) )
        {
          selected2DEdges.add( edge );
        }
      }
    }
    return selected2DEdges;
  }

  public static final Collection<IFE1D2DEdge> collectAll1DEdges( Feature[] selectedFeatures )
  {
    Set<IFE1D2DEdge> selected1DEdges = new HashSet<IFE1D2DEdge>();

    for( Feature feature : selectedFeatures )
    {
      if( IFE1D2DEdge.QNAME.equals( feature.getFeatureType().getQName() ) )
      {
        IFE1D2DEdge edge = (IFE1D2DEdge) feature.getAdapter( IFE1D2DEdge.class );
        if( TypeInfo.is1DEdge( edge ) )
        {
          selected1DEdges.add( edge );
        }
      }
    }
    return selected1DEdges;

  }

  public static boolean hasOnlyBorderEdges( Collection<IFE1D2DEdge> edges )
  {
    if( edges == null )
    {
      return false;
    }
    else
    {
      for( IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode> edge : edges )
      {
        if( !TypeInfo.isBorderEdge( edge ) )
        {
          return false;
        }
      }
      return true;
    }
  }

}