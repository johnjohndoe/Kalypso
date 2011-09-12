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
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.kalypso.kalypsomodel1d2d.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

@SuppressWarnings("unchecked")
/*
 * (static) helper functions for the {@link org.kalypso.kalypsomodel1d2d.schema.binding.FE1D2DDiscretisationModel}
 * class.
 * 
 * @author Gernot Belger
 * 
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
    final IElement1D element = elements.addNew( IElement1D.QNAME, IElement1D.class );

    element.setEdge( edge );

    return element;
  }

  public static final IPolyElement createElement2d( IFEDiscretisationModel1d2d model1d2d, List<IFE1D2DEdge> edges )
  {
    Assert.throwIAEOnNullParam( model1d2d, "model1d2d" ); //$NON-NLS-1$
    Assert.throwIAEOnNullParam( edges, "edges" ); //$NON-NLS-1$
    final int EDGE_NUM = edges.size();
    if( !(EDGE_NUM == 3 || EDGE_NUM == 4) )
    {
      throw new IllegalArgumentException( Messages.getString( "org.kalypso.kalypsomodel1d2d.ops.ModelOps.2" ) + EDGE_NUM ); //$NON-NLS-1$
    }

    IFeatureWrapperCollection<IFE1D2DElement> elements = model1d2d.getElements();
    final IPolyElement polyElement = elements.addNew( IPolyElement.QNAME, IPolyElement.class );

    // sortElementEdges( polyElement );
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
    if( !(element instanceof IPolyElement) )
      return;

    final IFeatureWrapperCollection edgeFeatureCollection = ((IPolyElement) element).getEdges();
    final IFeatureWrapperCollection<IFE1D2DEdge> edges = edgeFeatureCollection;
    final FeatureList edgeFeatureList = edges.getWrappedList();

    edgeFeatureList.clear();

    final int INITIAL_SIZE = edges.size();
    if( INITIAL_SIZE < 3 )
    {
      return;
    }

    // just select the first node
    IFE1D2DEdge edge = edges.remove( 0 );
    IFE1D2DNode nodeEnd = edge.getNode( 0 );
    edgeFeatureList.add( edge.getGmlID() );

    while( !edges.isEmpty() )
    {
      for( final IFE1D2DEdge nextEdge : edges )
      {
        final IFeatureWrapperCollection<IFE1D2DNode> nodes = nextEdge.getNodes();
        final IFE1D2DNode node0 = nodes.get( 0 );
        final IFE1D2DNode node1 = nodes.get( 1 );
        if( node0.equals( nodeEnd ) )
        {
          edge = nextEdge;
          nodeEnd = node1;
          break;
        }
        else if( node1.equals( nodeEnd ) )
        {
          edge = nextEdge;
          nodeEnd = node0;
          break;
        }
      }
      if( edges.remove( edge ) )
      {
        edgeFeatureList.add( edge.getGmlID() );
      }
      else
      {
        throw new RuntimeException( Messages.getString( "org.kalypso.kalypsomodel1d2d.ops.ModelOps.3" , edge, edges ) ); //$NON-NLS-1$
      }
    }
  }

  /**
   * Answer whether the edge is contained by a fe element. This is the when if its is directely contains in an element
   * or in the case of a normal edge through its edge inv
   * 
   * @param egde
   *          the edge to test
   * @return true if the given edge is in an element
   */
  public static final boolean isContainedInAnElement( final IFE1D2DEdge edge )
  {
    return !edge.getContainers().isEmpty();
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

}