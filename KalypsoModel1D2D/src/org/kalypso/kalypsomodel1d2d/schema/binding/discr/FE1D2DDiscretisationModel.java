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
import org.kalypso.kalypsomodel1d2d.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.VersionedModel;
import org.kalypso.kalypsosimulationmodel.core.discr.IFENetItem;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * Provide a implementation of {@link IFEDiscretisationModel1d2d} to bind wb1d2d:Discretisation gml elements
 * 
 * @author Gernot Belger
 * @author Patrice Congo
 */
@SuppressWarnings("unchecked")
public class FE1D2DDiscretisationModel extends VersionedModel implements IFEDiscretisationModel1d2d
{
  private final IFeatureBindingCollection<IFE1D2DElement> m_elements = new FeatureBindingCollection<IFE1D2DElement>( this, IFE1D2DElement.class, IFEDiscretisationModel1d2d.WB1D2D_PROP_ELEMENTS );

  private final IFeatureBindingCollection<IFE1D2DEdge> m_edges = new FeatureBindingCollection<IFE1D2DEdge>( this, IFE1D2DEdge.class, IFEDiscretisationModel1d2d.WB1D2D_PROP_EDGES );

  private final IFeatureBindingCollection<IFE1D2DNode> m_nodes = new FeatureBindingCollection<IFE1D2DNode>( this, IFE1D2DNode.class, IFEDiscretisationModel1d2d.WB1D2D_PROP_NODES );

  private final IFeatureBindingCollection<IFELine> m_continuityLines = new FeatureBindingCollection<IFELine>( this, IFELine.class, IFEDiscretisationModel1d2d.WB1D2D_PROP_CONTINUITY_LINES );

  private final IFeatureBindingCollection<IFE1D2DComplexElement> complexElements = new FeatureBindingCollection<IFE1D2DComplexElement>( this, IFE1D2DComplexElement.class, IFEDiscretisationModel1d2d.WB1D2D_PROP_COMPLEX_ELEMENTS );

  public FE1D2DDiscretisationModel( Object parent, IRelationType parentRelation, IFeatureType ft, String id, Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d#findEdge(org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DNode,
   *      org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DNode)
   */
  @Override
  public IFE1D2DEdge findEdge( final IFE1D2DNode node0, final IFE1D2DNode node1 )
  {
    final IFeatureBindingCollection<Feature> containers = node0.getContainers();
    for( final Feature featureWrapper : containers )
    {
      if( featureWrapper instanceof IFE1D2DEdge )
      {
        final IFE1D2DEdge edge = (IFE1D2DEdge) featureWrapper;
        if( edge.getNodes().contains( node1 ) )
        {
          return edge;
        }

      }
    }

    return null;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d#getComplexElements()
   */
  @Override
  public IFeatureBindingCollection<IFE1D2DComplexElement> getComplexElements( )
  {
    return complexElements;
  }

  @Override
  public final IFeatureBindingCollection<IFE1D2DElement> getElements( )
  {
    return m_elements;
  }

  @Override
  public IFeatureBindingCollection<IFE1D2DNode> getNodes( )
  {
    return m_nodes;
  }

  @Override
  public IFeatureBindingCollection<IFE1D2DEdge> getEdges( )
  {
    return m_edges;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d#createNode(GM_Point, double, boolean[])
   */
  @Override
  public IFE1D2DNode createNode( final GM_Point nodeLocation, final double searchRectWidth, final boolean[] alreadyExists )
  {
    Assert.throwIAEOnNullParam( nodeLocation, "nodeLocation" ); //$NON-NLS-1$

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
      // FeatureList nodeList = m_nodes.getFeatureList();

      node = m_nodes.addNew( Kalypso1D2DSchemaConstants.WB1D2D_F_NODE, IFE1D2DNode.class );
      node.setPoint( nodeLocation );
      alreadyExists[0] = false;
      return node;
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d#findNode(org.kalypsodeegree.model.geometry.GM_Point,
   *      double)
   * 
   */
  @Override
  public IFE1D2DNode findNode( final GM_Point nodeLocation, final double searchRectWidth )
  {
    final FeatureList nodeList = m_nodes.getFeatureList();
    final GM_Envelope reqEnvelope = GeometryUtilities.grabEnvelopeFromDistance( nodeLocation, searchRectWidth );
    final List<Feature> foundNodes = nodeList.query( reqEnvelope, null );
    if( foundNodes.isEmpty() )
      return null;
    else
    {
      double minDistance = Double.MAX_VALUE;
      IFE1D2DNode nearestNode = null;
      for( final Feature feature : foundNodes )
      {
        if( feature == null )
          continue; // TODO: this is a non-test: it should never happen, so we shouldnt test it here...
        final IFE1D2DNode currentNode = (IFE1D2DNode) feature;
        final GM_Point point = currentNode.getPoint();
        if( point == null )
          throw new IllegalArgumentException( Messages.getString( "org.kalypso.kalypsomodel1d2d.schema.binding.discr.FE1D2DDiscretisationModel.0" ) + currentNode ); //$NON-NLS-1$

        /**
         * calculating the distance only between two nodes(simple operation) should not be done with vividsolutions
         * 
         * @see org.kalypsodeegree_impl.model.geometry.GM_Object_Impl.distance( final GM_Object gmo ) huge and complex
         *      computing of distance between two geometries of any kind
         * 
         *      replaced by local simple and fast implementation
         */
        final double currentDistance = calculateDistance2d( nodeLocation, point );
        if( minDistance > currentDistance )
        {
          nearestNode = currentNode;
          minDistance = currentDistance;
        }
      }

      // HACK: check again for the grab distance. This is necessary as the geoindex doe not work properly at the moment
      if( minDistance > searchRectWidth )
        return null;

      return nearestNode;
    }
  }

  private double calculateDistance2d( final GM_Point firstPoint, final GM_Point secondPoint )
  {
    try
    {
      double dx = firstPoint.getX() - secondPoint.getX();
      double dy = firstPoint.getY() - secondPoint.getY();

      return Math.sqrt( dx * dx + dy * dy );
    }
    catch( Exception e )
    {
      return Double.MAX_VALUE;
    }
  }

  /**
   * Finds an element-2d near to the given point.
   * 
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d#find2DElement(org.kalypsodeegree.model.geometry.GM_Point,
   *      double)
   */
  @Override
  public IPolyElement find2DElement( final GM_Point position, final double grabDistance )
  {
    return findElement( position, grabDistance, IPolyElement.class );
  }

  @Override
  public <T extends IFENetItem> T findElement( final GM_Point position, final double grabDistance, final Class<T> elementClass )
  {
    final GM_Envelope reqEnvelope = GeometryUtilities.grabEnvelopeFromDistance( position, grabDistance );
    final List<IFE1D2DElement> foundElements = m_elements.query( reqEnvelope );
    double min = Double.MAX_VALUE;
    T nearest = null;
    for( final IFE1D2DElement current : foundElements )
    {
      if( elementClass.isInstance( current ))
      {
        final GM_Object geometryFromNetItem = current.getDefaultGeometryPropertyValue();
        if( geometryFromNetItem != null )
        {
          final double curDist = position.distance( geometryFromNetItem );
          if( min > curDist && curDist <= grabDistance )
          {
            nearest = (T) current;
            min = curDist;
          }
        }
      }
    }
    return nearest;
  }

  @Override
  public IFE1D2DEdge findEdge( final GM_Point position, final double grabDistance )
  {
    final FeatureList modelList = m_edges.getFeatureList();
    final GM_Envelope reqEnvelope = GeometryUtilities.grabEnvelopeFromDistance( position, grabDistance );
    final List<Feature> foundEdges = modelList.query( reqEnvelope, null );
    double min = Double.MAX_VALUE;
    IFE1D2DEdge nearest = null;
    for( final Feature feature : foundEdges )
    {
      if( feature == null )
        continue; // This should never happen!

      final IFE1D2DEdge current = (IFE1D2DEdge) feature.getAdapter( IFE1D2DEdge.class );
      if( current != null )
      {
        final GM_Curve curve = current.getGeometry();
        if( curve != null )
        {
          final double curDist = position.distance( curve );
          if( min > curDist && curDist <= grabDistance )
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
   * Finds a continuity line near the given position.
   * 
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d#findContinuityLine(org.kalypsodeegree.model.geometry.GM_Point,
   *      double)
   */
  @Override
  public IFELine findContinuityLine( final GM_Point position, final double grabDistance )
  {
    // if we just search for the first line within the line envelope, and line is z.B. under 45 deg angle,
    // then envelope is very big and nothing else within that envelope cannot be selected (2D element or node(s)).
    // maybe user wanted to select 2D element near the line, but he cannot do so

    // solution: get nodes from all the lines within the envelope; if mouse is near to any node, line is selected

    // final IFE1D2DNode nodeUnderMouse = findNode( position, grabDistance );
    // if( nodeUnderMouse == null )
    // return null;
    final GM_Envelope reqEnvelope = GeometryUtilities.grabEnvelopeFromDistance( position, grabDistance );
    final List<IFELine> query = m_continuityLines.query( reqEnvelope );
    for( final IFELine line : query )
    {
      final GM_Curve geometry = line.getGeometry();

      if( geometry == null )
        return null;

      if( geometry.distance( position ) < grabDistance )
        return line;
      // final List<IFE1D2DNode> nodes = line.getNodes();
      // for( final IFE1D2DNode node : nodes )
      // if( node != null && nodeUnderMouse.equals( node ) )
      // return line;
    }
    // if( query != null && query.size() > 0 )
    // return query.get( 0 );
    // else
    return null;
    // return findElement( position, grabDistance, IFELine.class );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d#find1DElement(org.kalypsodeegree.model.geometry.GM_Point,
   *      double)
   */
  @Override
  public IElement1D find1DElement( final GM_Point position, final double grabDistance )
  {
    return findElement( position, grabDistance, IElement1D.class );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d#getContinuityLines()
   */
  @Override
  public IFeatureBindingCollection<IFELine> getContinuityLines( )
  {
    return m_continuityLines;
  }
}
