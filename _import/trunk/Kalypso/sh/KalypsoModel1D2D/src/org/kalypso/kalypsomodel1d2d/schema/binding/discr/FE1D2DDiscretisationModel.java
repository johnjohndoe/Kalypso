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
package org.kalypso.kalypsomodel1d2d.schema.binding.discr;

import java.util.List;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.ops.NodeOps;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.FeatureWrapperCollection;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.feature.binding.AbstractFeatureBinder;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Provide a implementation of {@link IFEDiscretisationModel1d2d} to bind wb1d2d:Discretisation gml elements
 * 
 * @author Gernot Belger
 * @author Patrice Congo
 */
@SuppressWarnings("unchecked")
public class FE1D2DDiscretisationModel extends AbstractFeatureBinder implements IFEDiscretisationModel1d2d
{
  private final IFeatureWrapperCollection<IFE1D2DElement> m_elements = new FeatureWrapperCollection<IFE1D2DElement>( getFeature(), IFE1D2DElement.class, Kalypso1D2DSchemaConstants.WB1D2D_PROP_ELEMENTS );

  private final IFeatureWrapperCollection<IFE1D2DEdge> m_edges = new FeatureWrapperCollection<IFE1D2DEdge>( getFeature(), IFE1D2DEdge.class, Kalypso1D2DSchemaConstants.WB1D2D_PROP_EDGES );

  private final IFeatureWrapperCollection<IFE1D2DNode> m_nodes = new FeatureWrapperCollection<IFE1D2DNode>( getFeature(), IFE1D2DNode.class, Kalypso1D2DSchemaConstants.WB1D2D_PROP_NODES );

  private final IFeatureWrapperCollection<IFE1D2DComplexElement> complexElements = new FeatureWrapperCollection<IFE1D2DComplexElement>( getFeature(), IFE1D2DComplexElement.class, Kalypso1D2DSchemaConstants.WB1D2D_PROP_COMPLEX_ELEMENTS );

  public FE1D2DDiscretisationModel( final Feature featureToBind )
  {
    super( featureToBind, Kalypso1D2DSchemaConstants.WB1D2D_F_DiscretisationModel );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d#findEdge(org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DNode,
   *      org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DNode)
   */
  public IFE1D2DEdge findEdge( final IFE1D2DNode node0, final IFE1D2DNode node1 )
  {
    IFE1D2DEdge toInv = null;
    for( final IFE1D2DEdge edge : (IFeatureWrapperCollection<IFE1D2DEdge>) node0.getContainers() )
    {
      if( NodeOps.endOf( node1, edge ) )
      {
        return edge;
      }
      else if( NodeOps.startOf( node1, edge ) )
      {
        toInv = edge;
      }
    }
    if( toInv != null )
    {
      return new EdgeInv( toInv, this );
    }
    else
    {
      return null;
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d#getComplexElements()
   */
  public IFeatureWrapperCollection<IFE1D2DComplexElement> getComplexElements( )
  {
    return complexElements;
  }

  public final IFeatureWrapperCollection<IFE1D2DElement> getElements( )
  {
    return m_elements;
  }

  public IFeatureWrapperCollection<IFE1D2DNode> getNodes( )
  {
    return m_nodes;
  }

  public IFeatureWrapperCollection<IFE1D2DEdge> getEdges( )
  {
    return m_edges;
  }

  public IFE1D2DContinuityLine createContinuityLine( )
  {
    final Feature parentFeature = getFeature();
    final IFeatureType parentFT = parentFeature.getFeatureType();
    final IRelationType rt = (IRelationType) parentFT.getProperty( Kalypso1D2DSchemaConstants.WB1D2D_PROP_ELEMENTS );

    final IFeatureType contiType = parentFT.getGMLSchema().getFeatureType( Kalypso1D2DSchemaConstants.WB1D2D_F_FE1D2DContinuityLine );
    final Feature contiFeature = parentFeature.getWorkspace().createFeature( parentFeature, rt, contiType );
    return new FE1D2DContinuityLine( contiFeature );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d#createNode(GM_Point, double, boolean[])
   */
  public IFE1D2DNode createNode( final GM_Point nodeLocation, final double searchRectWidth, final boolean[] alreadyExists )
  {
    Assert.throwIAEOnNullParam( nodeLocation, "nodeLocation" );

    // TODO: major performance bug for adding large numbers of points:
    // searching and adding each single node/location will cause
    // the geo-index of the nodes to be reindexed for each call of this method

    IFE1D2DNode node = null;
    if( searchRectWidth >= 0 )
    {
      // donot search if rect width is negative
      node = findNode( nodeLocation, searchRectWidth );
    }
    if( node != null )
    {
      if( alreadyExists != null )
      {
        if( alreadyExists.length > 0 )
        {
          alreadyExists[0] = false;
        }
      }
      return node;
    }
    else
    {
      // FeatureList nodeList = m_nodes.getWrappedList();

      node = m_nodes.addNew( Kalypso1D2DSchemaConstants.WB1D2D_F_NODE );
      node.setPoint( nodeLocation );
      alreadyExists[0] = false;
      return node;
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d#findNode(org.kalypsodeegree.model.geometry.GM_Point,
   *      double)
   */
  public IFE1D2DNode findNode( final GM_Point nodeLocation, final double searchRectWidth )
  {
    final FeatureList nodeList = m_nodes.getWrappedList();

    final GM_Envelope reqEnvelope = grabEnvelopeFromDistance( nodeLocation, searchRectWidth );

    final List<Feature> foundNodes = nodeList.query( reqEnvelope, null );
    if( foundNodes.isEmpty() )
    {
      // System.out.println("Node not found Found:"+reqEnvelope);
      return null;
    }
    else
    {
      double min = Double.MAX_VALUE, curDist;
      IFE1D2DNode nearest = null, curNode;
      for( final Feature feature : foundNodes )
      {
        curNode = new FE1D2DNode( feature );
        curDist = nodeLocation.distance( curNode.getPoint() );
        if( min > curDist )
        {
          nearest = curNode;
          min = curDist;
        }
      }
      return nearest;
    }
  }

  /**
   * Finds an element-2d near to the given point.
   * 
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d#find2DElement(org.kalypsodeegree.model.geometry.GM_Point,
   *      double)
   */
  public IPolyElement find2DElement( final GM_Point position, final double grabDistance )
  {
    return (IPolyElement) findElement( position, grabDistance, IPolyElement.class );
  }

  private IFeatureWrapper2 findElement( final GM_Point position, final double grabDistance, final Class elementClass )
  {
    final FeatureList modelList = m_elements.getWrappedList();
    final GM_Envelope reqEnvelope = grabEnvelopeFromDistance( position, grabDistance );
    final List<Feature> foundNodes = modelList.query( reqEnvelope, null );
    double min = Double.MAX_VALUE;
    IFeatureWrapper2 nearest = null;
    for( final Feature feature : foundNodes )
    {
      final IFeatureWrapper2 current = (IFeatureWrapper2) feature.getAdapter( elementClass );
      if( current != null )
      {
        final GM_Object geometryFromNetItem = geometryFromNetItem( current );
        if( geometryFromNetItem != null )
        {
          final double curDist = position.distance( geometryFromNetItem );
          if( min > curDist && curDist < grabDistance )
          {
            nearest = current;
            min = curDist;
          }
        }
      }
    }
    return nearest;
  }

  /**
   * Returns the geometry of the given item of the net.
   */
  private GM_Object geometryFromNetItem( final IFeatureWrapper2 netItem )
  {
    try
    {
      // TODO: add other known classes to switch

      // ATTENTION: order of these ifs is importent, because
      // IFE1D2DContinuityLine inherits from FE1D2D_2DElement but
      // has no surface...
      // FIX: make an own wrapper-class for Polygon-Elements, only those
      // have a surface
      if( netItem instanceof IFE1D2DContinuityLine )
        return ((IFE1D2DContinuityLine) netItem).recalculateElementGeometry();

      if( netItem instanceof IPolyElement )
      {
        return ((IPolyElement) netItem).recalculateElementGeometry();// getGeometry();
      }
      return null;
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
      return null;
    }

  }

  private GM_Envelope grabEnvelopeFromDistance( final GM_Point position, final double grabDistance )
  {
    final double posX = position.getX();
    final double posY = position.getY();
    final double grabDistanceHalf = grabDistance / 2;

    final GM_Position minPos = GeometryFactory.createGM_Position( posX - grabDistanceHalf, posY - grabDistanceHalf );
    final GM_Position maxPos = GeometryFactory.createGM_Position( posX + grabDistanceHalf, posY + grabDistanceHalf );

    final GM_Envelope reqEnvelope = GeometryFactory.createGM_Envelope( minPos, maxPos );
    return reqEnvelope;
  }

  /**
   * Finds a continuity line near the given position.
   * 
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d#findContinuityLine(org.kalypsodeegree.model.geometry.GM_Point,
   *      double)
   */
  public IFE1D2DContinuityLine findContinuityLine( final GM_Point position, final double grabDistance )
  {
    return (IFE1D2DContinuityLine) findElement( position, grabDistance, IFE1D2DContinuityLine.class );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d#find1DElement(org.kalypsodeegree.model.geometry.GM_Point,
   *      double)
   */
  public IElement1D find1DElement( final GM_Point position, final double grabDistance )
  {
    return (IElement1D) findElement( position, grabDistance, IElement1D.class );
  }
}
