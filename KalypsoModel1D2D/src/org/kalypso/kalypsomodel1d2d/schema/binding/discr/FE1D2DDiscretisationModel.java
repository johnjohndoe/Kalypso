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

import gnu.trove.THashSet;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.VersionedModel;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * A mesh with its elements, edges, nodes, complex elements (calculation units, transitions, junctions), continuity (boundary) lines
 * 
 * @author Stefan Kurzbach
 */
public class FE1D2DDiscretisationModel extends VersionedModel implements IFEDiscretisationModel1d2d
{
  private static final double CLUSTER_TOLERANCE = 0.001;

  private final IFeatureType m_polyType;

  public FE1D2DDiscretisationModel( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
    m_polyType = GMLSchemaUtilities.getFeatureTypeQuiet( IPolyElement.QNAME );
  }

  @Override
  public IFE1D2DEdge createEdge( final IFE1D2DNode node0, final IFE1D2DNode node1 )
  {
    Assert.throwIAEOnNullParam( node0, "node0" ); //$NON-NLS-1$
    Assert.throwIAEOnNullParam( node1, "node1" ); //$NON-NLS-1$

    IFE1D2DEdge edge = findEdge( node0, node1 );
    if( edge != null )
      return edge;

    try
    {
      edge = (IFE1D2DEdge)FeatureHelper.createFeatureForListProp( getEdgesInternal(), null, -1 );
      edge.setNodes( node0, node1 );
      return edge;
    }
    catch( final GMLSchemaException e )
    {
      throw new IllegalStateException( e );
    }
  }

  @Override
  public IElement1D createElement1D( final IFE1D2DEdge edge )
  {
    if( edge.getLinkedElements().length != 0 )
      throw new IllegalStateException( "The edge is already contained in one or more elements." ); //$NON-NLS-1$

    try
    {
      final IElement1D element = (IElement1D)FeatureHelper.createFeatureForListProp( getElementsInternal(), IElement1D.QNAME, -1 );
      element.setEdge( edge );
      return element;
    }
    catch( final GMLSchemaException e )
    {
      throw new IllegalStateException( e );
    }
  }

  @Override
  public IPolyElement createElement2D( final IFE1D2DEdge[] edges )
  {
    final GMLWorkspace workspace = getWorkspace();
    try
    {
      final IRelationType listRelation = getElementsInternal().getPropertyType();
      final IPolyElement element = (IPolyElement)workspace.createFeature( this, listRelation, m_polyType );
      workspace.addFeatureAsComposition( this, listRelation, -1, element );
      element.setEdges( edges );
      setEnvelopesUpdated();
      return element;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
    return null;
  }

  @Override
  public IFE1D2DNode createNode( final GM_Point nodeLocation )
  {
    Assert.throwIAEOnNullParam( nodeLocation, "nodeLocation" ); //$NON-NLS-1$

    final IFE1D2DNode existingNode = findNode( nodeLocation, CLUSTER_TOLERANCE );
    if( existingNode != null )
      return existingNode;

    if( elementsIntersect( nodeLocation ) )
      throw new IllegalStateException( "The given location is inside an existing element" ); //$NON-NLS-1$

    try
    {
      final IFE1D2DNode newNode = (IFE1D2DNode)FeatureHelper.createFeatureForListProp( getNodesInternal(), null, -1 );
      newNode.setPoint( nodeLocation );
      return newNode;
    }
    catch( final GMLSchemaException e )
    {
      throw new IllegalStateException( e );
    }
  }

  @Override
  public IElement1D find1DElement( final GM_Point position, final double grabDistance )
  {
    return findElement( position, grabDistance, IElement1D.class );
  }

  @Override
  public IPolyElement find2DElement( final GM_Point position, final double grabDistance )
  {
    return findElement( position, grabDistance, IPolyElement.class );
  }

  @Override
  public IFELine findContinuityLine( final GM_Point position, final double grabDistance )
  {
    final GM_Envelope reqEnvelope = GeometryUtilities.grabEnvelopeFromDistance( position, grabDistance );
    final List<IFELine> query = getLinesInternal().queryResolved( reqEnvelope, null );
    for( final IFELine line : query )
    {
      final GM_Curve geometry = line.getGeometry();
      if( geometry.distance( position ) < grabDistance )
        return line;
    }
    return null;
  }

  @Override
  public IFE1D2DEdge findEdge( final GM_Point position, final double grabDistance )
  {
    final FeatureList modelList = getEdgesInternal();
    final GM_Envelope reqEnvelope = GeometryUtilities.grabEnvelopeFromDistance( position, grabDistance );
    final List<IFE1D2DEdge> foundEdges = modelList.queryResolved( reqEnvelope, null );
    double min = Double.MAX_VALUE;
    IFE1D2DEdge nearest = null;
    for( final IFE1D2DEdge current : foundEdges )
    {
      final GM_Curve curve = current.getGeometry();
      final double curDist = position.distance( curve );
      if( min > curDist && curDist <= grabDistance )
      {
        nearest = current;
        min = curDist;
      }
    }
    return nearest;
  }

  @Override
  public IFE1D2DEdge findEdge( final IFE1D2DNode node0, final IFE1D2DNode node1 )
  {
    final IFE1D2DEdge[] containers = node0.getLinkedEdges();
    for( final IFE1D2DEdge edge : containers )
    {
      if( edge.containsNode( node1 ) )
        return edge;
    }
    return null;
  }

  @SuppressWarnings( "unchecked" )
  private <T extends IFENetItem> T findElement( final GM_Point position, final double grabDistance, final Class<T> elementClass )
  {
    final GM_Envelope reqEnvelope = GeometryUtilities.grabEnvelopeFromDistance( position, grabDistance );
    final List<IFE1D2DElement> foundElements = getElementsInternal().queryResolved( reqEnvelope, null );

    double min = Double.MAX_VALUE;
    T nearest = null;

    for( final IFE1D2DElement current : foundElements )
    {
      if( elementClass.isInstance( current ) )
      {
        final GM_Object geometryFromNetItem = current.getDefaultGeometryPropertyValue();
        final double curDist = position.distance( geometryFromNetItem );
        if( min > curDist && curDist <= grabDistance )
        {
          nearest = (T)current;
          min = curDist;
        }
      }
    }

    return nearest;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d#findNode(org.kalypsodeegree.model.geometry.GM_Point, double)
   */
  @Override
  public IFE1D2DNode findNode( final GM_Point nodeLocation, final double grabDistance )
  {
    final FeatureList nodeList = getNodesInternal();
    final GM_Envelope reqEnvelope = GeometryUtilities.grabEnvelopeFromDistance( nodeLocation, grabDistance );
    final List<IFE1D2DNode> foundNodes = nodeList.queryResolved( reqEnvelope, null );
    if( foundNodes.isEmpty() )
      return null;

    else
    {
      double minDistance = Double.MAX_VALUE;
      IFE1D2DNode nearestNode = null;
      for( final IFE1D2DNode currentNode : foundNodes )
      {
        final GM_Point point = currentNode.getPoint();
        final double currentDistance = point.distance( nodeLocation );
        if( minDistance > currentDistance )
        {
          nearestNode = currentNode;
          minDistance = currentDistance;
        }
      }

      return nearestNode;
    }
  }

  @Override
  public GM_Envelope getBoundingBox( )
  {
    return getNodesInternal().getBoundingBox();
  }

  @Override
  public IFE1D2DComplexElement<IFENetItem>[] getComplexElements( )
  {
    final FeatureList complexElements = getComplexElementsInternal();
    return complexElements.toFeatures( new IFE1D2DComplexElement[complexElements.size()] );
  }

  private FeatureList getComplexElementsInternal( )
  {
    return (FeatureList)getProperty( WB1D2D_PROP_COMPLEX_ELEMENTS );
  }

  @Override
  public IFELine[] getContinuityLines( )
  {
    final FeatureList lines = getLinesInternal();
    return lines.toFeatures( new IFELine[lines.size()] );
  }

  @Override
  public IFE1D2DEdge[] getEdges( )
  {
    final FeatureList edges = getEdgesInternal();
    return edges.toFeatures( new IFE1D2DEdge[edges.size()] );
  }

  private FeatureList getEdgesInternal( )
  {
    return (FeatureList)getProperty( WB1D2D_PROP_EDGES );
  }

  @Override
  public final IFE1D2DElement[] getElements( )
  {
    final FeatureList elements = getElementsInternal();
    return elements.toFeatures( new IFE1D2DElement[elements.size()] );
  }

  private FeatureList getElementsInternal( )
  {
    return (FeatureList)getProperty( WB1D2D_PROP_ELEMENTS );
  }

  private FeatureList getLinesInternal( )
  {
    return (FeatureList)getProperty( WB1D2D_PROP_CONTINUITY_LINES );
  }

  @Override
  public IFE1D2DNode[] getNodes( )
  {
    final FeatureList nodes = getNodesInternal();
    return nodes.toFeatures( new IFE1D2DNode[nodes.size()] );
  }

  private FeatureList getNodesInternal( )
  {
    return (FeatureList)getProperty( WB1D2D_PROP_NODES );
  }

  @Override
  public void removeContinuityLine( final IFELine line )
  {
    Assert.throwIAEOnNullParam( line, "line" );//$NON-NLS-1$
    final FeatureList lines = getLinesInternal();
    if( lines.contains( line ) )
      lines.remove( line );
  }

  @Override
  public void removeEdge( final IFE1D2DEdge edge )
  {
    Assert.throwIAEOnNullParam( edge, "edge" );//$NON-NLS-1$

    final FeatureList edges = getEdgesInternal();
    if( !edges.contains( edge ) )
      return;

    if( edge.getLinkedElements().length != 0 )
      throw new IllegalStateException( "Edge is referenced by one or more elements." ); //$NON-NLS-1$

    final IFE1D2DNode[] nodes = edge.getNodes();
    for( final IFE1D2DNode node : nodes )
    {
      node.removeLinkedEdge( edge );
      if( node.getLinkedEdges().length == 0 )
        removeNode( node );
    }
    edges.remove( edge );
  }

  @Override
  public void removeElement( final IFE1D2DElement element )
  {
    Assert.throwIAEOnNullParam( element, "element" );//$NON-NLS-1$

    final FeatureList elements = getElementsInternal();
    if( !elements.contains( element ) )
      return;

    if( element.getLinkedElements().length != 0 )
      throw new IllegalStateException( "Element is referenced by one or more complex elements." ); //$NON-NLS-1$

    final IFE1D2DEdge[] edges = element.getEdges();
    for( final IFE1D2DEdge edge : edges )
    {
      edge.removeLinkedElement( element );
      if( edge.getLinkedElements().length == 0 )
        removeEdge( edge );
    }

    elements.remove( element );
  }

  @Override
  public void removeNode( final IFE1D2DNode node )
  {
    Assert.throwIAEOnNullParam( node, "node" );//$NON-NLS-1$
    final FeatureList nodes = getNodesInternal();
    if( !nodes.contains( node ) )
      return;

    if( node.getLinkedEdges().length != 0 )
      throw new IllegalStateException( "Node is referenced by one or more edges." ); //$NON-NLS-1$

    nodes.remove( node );
  }

  @Override
  @SuppressWarnings( "unchecked" )
  public void removeAllNodes( final Collection<IFE1D2DNode> nodesToRemove )
  {
    Assert.throwIAEOnNullParam( nodesToRemove, "nodesToRemove" );//$NON-NLS-1$

    for( final IFE1D2DNode node : nodesToRemove )
    {
      if( node.getLinkedEdges().length != 0 )
        throw new IllegalStateException( "Node is referenced by one or more edges." ); //$NON-NLS-1$
    }

    final FeatureList nodes = getNodesInternal();
    nodes.removeAll( nodesToRemove );
  }

  @Override
  @SuppressWarnings( "unchecked" )
  public void removeAllEdges( final Collection<IFE1D2DEdge> edgesToRemove )
  {
    Assert.throwIAEOnNullParam( edgesToRemove, "edgesToRemove" );//$NON-NLS-1$

    for( final IFE1D2DEdge edge : edgesToRemove )
    {
      if( edge.getLinkedElements().length != 0 )
        throw new IllegalStateException( "Edge is referenced by one or more elements." ); //$NON-NLS-1$
    }

    final FeatureList edges = getEdgesInternal();
    edges.removeAll( edgesToRemove );

    final Set<IFE1D2DNode> nodesToRemove = new THashSet<>();
    for( final IFE1D2DEdge edge : edgesToRemove )
    {
      final IFE1D2DNode[] nodes = edge.getNodes();
      for( final IFE1D2DNode node : nodes )
      {
        node.removeLinkedEdge( edge );
        if( node.getLinkedEdges().length == 0 )
          nodesToRemove.add( node );
      }
    }
    removeAllNodes( nodesToRemove );
  }

  @Override
  @SuppressWarnings( "unchecked" )  
  public void removeAllElements( final Collection< ? extends IFE1D2DElement> elementsToRemove )
  {
    Assert.throwIAEOnNullParam( elementsToRemove, "elementsToRemove" );//$NON-NLS-1$

    for( final IFE1D2DElement element : elementsToRemove )
    {
      if( element.getLinkedElements().length != 0 )
        throw new IllegalStateException( "Element is referenced by one or more complex elements." ); //$NON-NLS-1$
    }

    final FeatureList elements = getElementsInternal();
    elements.removeAll( elementsToRemove );

    final Set<IFE1D2DEdge> edgesToRemove = new THashSet<>();
    for( final IFE1D2DElement element : elementsToRemove )
    {
      final IFE1D2DEdge[] edges = element.getEdges();
      for( final IFE1D2DEdge edge : edges )
      {
        edge.removeLinkedElement( element );
        if( edge.getLinkedElements().length == 0 )
          edgesToRemove.add( edge );
      }
    }
    removeAllEdges( edgesToRemove );
  }

  @Override
  public List<IFE1D2DElement> queryElements( final GM_Envelope env, final List<IFE1D2DElement> result )
  {
    return getElementsInternal().queryResolved( env, result );
  }

  @Override
  public boolean elementsIntersect( final GM_Point point )
  {
    final GM_Envelope reqEnvelope = GeometryUtilities.grabEnvelopeFromDistance( point, CLUSTER_TOLERANCE );
    final List<IFE1D2DElement> elements = queryElements( reqEnvelope, null );
    for( final Iterator<IFE1D2DElement> iterator = elements.iterator(); iterator.hasNext(); )
    {
      final IFE1D2DElement element = iterator.next();
      if( element instanceof IPolyElement )
      {
        if( !((IPolyElement)element).getGeometry().contains( point ) )
          iterator.remove();
      }
      else if( element instanceof IElement1D )
      {
        if( !((IElement1D)element).getEdge().getGeometry().contains( point ) )
          iterator.remove();
      }
    }
    return !elements.isEmpty();
  }

  @Override
  public IContinuityLine1D createContinuityLine1D( final IFE1D2DNode node )
  {
    Assert.throwIAEOnNullParam( node, "node" ); //$NON-NLS-1$

    final IFE1D2DEdge[] linkedEdges = node.getLinkedEdges();
    if( linkedEdges.length != 1 || linkedEdges[0].getLinkedElements().length != 1 || !(linkedEdges[0].getLinkedElements()[0] instanceof IElement1D) )
      throw new IllegalStateException( "Can only create a 1D continuity line at the end of a 1D element" ); //$NON-NLS-1$

    try
    {
      final IContinuityLine1D line = (IContinuityLine1D)FeatureHelper.createFeatureForListProp( getLinesInternal(), ICalculationUnit1D.QNAME, -1 );
      line.setNode( node );
      return line;
    }
    catch( final GMLSchemaException e )
    {
      throw new IllegalStateException( e );
    }
  }

  @Override
  public IContinuityLine2D createContinuityLine2D( final IFE1D2DNode[] nodes )
  {
    Assert.throwIAEOnNullParam( nodes, "nodes" ); //$NON-NLS-1$
    try
    {
      final IContinuityLine2D line = (IContinuityLine2D)FeatureHelper.createFeatureForListProp( getLinesInternal(), IContinuityLine2D.QNAME, -1 );
      line.setNodes( nodes );
      return line;
    }
    catch( final GMLSchemaException e )
    {
      throw new IllegalStateException( e );
    }
  }

  @Override
  public ICalculationUnit createCalculationUnit( final QName calcUnitType )
  {
    try
    {
      return (ICalculationUnit)FeatureHelper.createFeatureForListProp( getComplexElementsInternal(), calcUnitType, -1 );
    }
    catch( final GMLSchemaException e )
    {
      throw new IllegalStateException( e );
    }
  }

  @Override
  public ITransitionElement createTransitionElement( )
  {
    try
    {
      final ITransitionElement transElement = (ITransitionElement)FeatureHelper.createFeatureForListProp( getComplexElementsInternal(), ITransitionElement.QNAME, -1 );
      return transElement;
    }
    catch( final GMLSchemaException e )
    {
      throw new IllegalStateException( e );
    }
  }

  @Override
  public IJunctionElement createJunctionElement( )
  {
    try
    {
      final IJunctionElement junctElement = (IJunctionElement)FeatureHelper.createFeatureForListProp( getComplexElementsInternal(), IJunctionElement.QNAME, -1 );
      return junctElement;
    }
    catch( final GMLSchemaException e )
    {
      throw new IllegalStateException( e );
    }
  }

  @Override
  public void removeComplexElement( final IFE1D2DComplexElement< ? extends IFENetItem> complexElement )
  {
    Assert.throwIAEOnNullParam( complexElement, "complexElement" );//$NON-NLS-1$
    final FeatureList complexElements = getComplexElementsInternal();
    if( complexElements.contains( complexElement ) )
      complexElements.remove( complexElement );
  }
}
