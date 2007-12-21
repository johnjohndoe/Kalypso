/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.kalypsomodel1d2d.ui.map;

import java.awt.Graphics;
import java.awt.Point;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.ObjectUtils;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.command.ICommand;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.DiscretisationModelUtils;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.FE1D2DDiscretisationModel;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.FE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.FE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.PolyElement;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.ListPropertyChangeCommand;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.command.CompositeCommand;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.editor.gmleditor.util.command.AddFeatureCommand;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * TODO: separate (again) geometry building from 1d2d element building!
 * 
 * This class is used to build new 1D2D Elements
 * 
 * @author Gernot Belger
 */
public class ElementGeometryBuilder
{
  /**
   * Stores the count of points which this geometry must have. If it is 0, there is no rule.
   */
  private final int m_cnt_points;

  /** A list of either nodes or locations for new nodes. */
  private final List<Object> m_nodes = new ArrayList<Object>();

  private final IKalypsoFeatureTheme m_nodeTheme;

  /**
   * The constructor.
   * 
   * @param cnt_points
   *            If >0 the the geometry will be finished, if the count of points is reached. If 0 no rule regarding the
   *            count of the points will apply.
   * @param targetCrs
   *            The target coordinate system.
   */
  public ElementGeometryBuilder( final int cnt_points, final IKalypsoFeatureTheme nodeTheme )
  {
    m_nodeTheme = nodeTheme;
    if( cnt_points > 0 )
      m_cnt_points = cnt_points;
    else
      m_cnt_points = 0;
  }

  /**
   * Adds a node or a would-be node (i.e. a GM_Point) to this builder.<br>
   * REMARK: No validity check is done here. Call {@link #checkNewNode(Object)} before a new node is added.
   */
  public ICommand addNode( final Object node ) throws Exception
  {
    Assert.isTrue( node instanceof GM_Point || node instanceof IFE1D2DNode );

    m_nodes.add( node );
    removeDuplicates( m_nodes );

    if( m_nodes.size() == m_cnt_points )
      return finish();

    return null;
  }

  /**
   * REMARK: No validity check is done here. Call {@link #checkNewNode(Object)} before a new node is added.
   * 
   * @see org.kalypso.informdss.manager.util.widgets.IGeometryBuilder#finish()
   */
  public ICommand finish( ) throws Exception
  {
    final CompositeCommand command = new CompositeCommand( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.ElementGeometryBuilder.1" ) ); //$NON-NLS-1$

    final CommandableWorkspace workspace = m_nodeTheme.getWorkspace();
    final FeatureList featureList = m_nodeTheme.getFeatureList();
    final Feature parentFeature = featureList.getParentFeature();
    final IFeatureType parentType = parentFeature.getFeatureType();
    final IRelationType parentNodeProperty = featureList.getParentFeatureTypeProperty();
    final IRelationType parentEdgeProperty = (IRelationType) parentType.getProperty( Kalypso1D2DSchemaConstants.WB1D2D_PROP_EDGES );
    final IRelationType parentElementProperty = (IRelationType) parentType.getProperty( Kalypso1D2DSchemaConstants.WB1D2D_PROP_ELEMENTS );
    final IGMLSchema schema = workspace.getGMLSchema();

    final IFeatureType nodeFeatureType = schema.getFeatureType( Kalypso1D2DSchemaConstants.WB1D2D_F_NODE );
    final IPropertyType nodeContainerPT = nodeFeatureType.getProperty( Kalypso1D2DSchemaConstants.WB1D2D_PROP_NODE_CONTAINERS );

    final IFeatureType edgeFeatureType = schema.getFeatureType( IFE1D2DEdge.QNAME );
    final IPropertyType edgeContainerPT = edgeFeatureType.getProperty( Kalypso1D2DSchemaConstants.WB1D2D_PROP_EDGE_CONTAINERS );

    /* Initialize elements needed for edges and elements */
    final IFEDiscretisationModel1d2d discModel = new FE1D2DDiscretisationModel( parentFeature );
    final List<FeatureChange> changes = new ArrayList<FeatureChange>();
    /* Build new nodes */
    final FE1D2DNode[] nodes = new FE1D2DNode[m_nodes.size()];
    for( int i = 0; i < m_nodes.size(); i++ )
    {
      final Object node = m_nodes.get( i );
      if( node instanceof FE1D2DNode )
        nodes[i] = (FE1D2DNode) node;
      else
      {
        // create new node
        final FE1D2DNode newNode = FE1D2DNode.createNode( discModel );
        // TODO: we should try to snap again here..., dont we?
        newNode.setPoint( (GM_Point) node );
        newNode.setName( "" ); //$NON-NLS-1$
        newNode.setDescription( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.ElementGeometryBuilder.3" ) ); //$NON-NLS-1$
        final AddFeatureCommand addNodeCommand = new AddFeatureCommand( workspace, parentFeature, parentNodeProperty, -1, newNode.getFeature(), null, false );
        command.addCommand( addNodeCommand );
        nodes[i] = newNode;
      }
    }

    /* Build new edges */
    final IFE1D2DEdge[] edges = new IFE1D2DEdge[m_nodes.size()];
    final boolean[] edgesGen = new boolean[m_nodes.size()]; // flag indicating if edge was generated
    for( int i = 0; i < edges.length; i++ )
    {
      final FE1D2DNode node0 = nodes[i];
      final FE1D2DNode node1 = nodes[(i + 1) % nodes.length];

      /* Search for existing edge */
      final IFE1D2DEdge edge = discModel.findEdge( node0, node1 );
      if( edge == null )
      {
        final FE1D2DEdge newEdge = FE1D2DEdge.createEdge( discModel );
        newEdge.setNodes( node0, node1 );
        edges[i] = newEdge;
        edgesGen[i] = true;
        final AddFeatureCommand addEdgeCommand = new AddFeatureCommand( workspace, parentFeature, parentEdgeProperty, -1, newEdge.getFeature(), null, false );

        command.addCommand( addEdgeCommand );

        addNodeContainerCommand( workspace, node0, node1, nodeContainerPT, newEdge, changes );
      }
      else
      {
        edges[i] = edge;
        edgesGen[i] = false;
      }
    }

    /* Build new element */
    final IPolyElement element = PolyElement.createPolyElement( discModel );

    element.setEdges( edges );

    final AddFeatureCommand addElementCommand = new AddFeatureCommand( workspace, parentFeature, parentElementProperty, -1, element.getWrappedFeature(), null, true );
    command.addCommand( addElementCommand );

    addEdgeContainerCommand( workspace, edges, edgeContainerPT, element, changes );
    command.addCommand( new ListPropertyChangeCommand( workspace, changes.toArray( new FeatureChange[changes.size()] ) ) );
    return command;
  }

  private static final void addEdgeContainerCommand( final GMLWorkspace workspace, final IFE1D2DEdge[] edges, final IPropertyType propertyType, final IFE1D2DElement element, final List<FeatureChange> changes )
  {
    final Feature elementFeature = element.getWrappedFeature();
    final String elementID = elementFeature.getId();

    // FeatureChange changes[]= new FeatureChange[edges.length];

    for( final IFE1D2DEdge element2 : edges )
    {

      changes.add( new FeatureChange( element2.getWrappedFeature(), propertyType, elementID ) );
    }
  }

  private static final void addNodeContainerCommand( final GMLWorkspace workspace, final IFE1D2DNode node0, final IFE1D2DNode node1, final IPropertyType propertyType, final IFE1D2DEdge edge, final List<FeatureChange> changes )
  {
    final Feature edgeFeature = edge.getWrappedFeature();
    final Feature node0Feature = node0.getWrappedFeature();
    final Feature node1Feature = node1.getWrappedFeature();
    final String edgeID = edgeFeature.getId();
    final FeatureChange change0 = new FeatureChange( node0Feature, propertyType, edgeID );
    changes.add( change0 );
    final FeatureChange change1 = new FeatureChange( node1Feature, propertyType, edgeID );
    changes.add( change1 );
  }

  /**
   * @see org.kalypso.informdss.manager.util.widgets.IGeometryBuilder#paint(java.awt.Graphics,
   *      org.kalypsodeegree.graphics.transformation.GeoTransform)
   */
  public void paint( final Graphics g, final GeoTransform projection, final Point currentPoint )
  {
    // IMPORTANT: we remember GM_Points (not Point's) and re-transform them for painting
    // because the projection depends on the current map-extent, so this builder
    // is stable in regard to zoom in/out
    final int[][] points = getPointArrays( projection, currentPoint );

    final int[] arrayX = points[0];
    final int[] arrayY = points[1];

    /* Paint a linestring. */
    g.drawPolygon( arrayX, arrayY, arrayX.length );
    drawHandles( g, arrayX, arrayY );
  }

  private int[][] getPointArrays( final GeoTransform projection, final Point currentPoint )
  {
    final List<Integer> xArray = new ArrayList<Integer>();
    final List<Integer> yArray = new ArrayList<Integer>();

    for( final Object node : m_nodes )
    {
      final GM_Point point;
      if( node instanceof FE1D2DNode )
        point = ((FE1D2DNode) node).getPoint();
      else
        point = (GM_Point) node;

      final int x = (int) projection.getDestX( point.getX() );
      final int y = (int) projection.getDestY( point.getY() );

      xArray.add( new Integer( x ) );
      yArray.add( new Integer( y ) );
    }

    xArray.add( currentPoint.x );
    yArray.add( currentPoint.y );

    final int[] xs = ArrayUtils.toPrimitive( xArray.toArray( new Integer[xArray.size()] ) );
    final int[] ys = ArrayUtils.toPrimitive( yArray.toArray( new Integer[yArray.size()] ) );

    return new int[][] { xs, ys };
  }

  private void drawHandles( final Graphics g, final int[] x, final int[] y )
  {
    final int sizeOuter = 6;
    for( int i = 0; i < y.length; i++ )
      g.drawRect( x[i] - sizeOuter / 2, y[i] - sizeOuter / 2, sizeOuter, sizeOuter );
  }

  public int getNumberOfNodes( )
  {
    return m_nodes.size();
  }

  public boolean contains( final IFE1D2DNode snapNode )
  {
    return m_nodes.contains( snapNode );
  }

  /**
   * Checks, if the resulting element would be valid, if the given new node would be inserted at the given position.
   */
  public IStatus checkNewNode( final Object newNode )
  {
    final List<Object> list = new ArrayList<Object>( m_nodes );
    list.add( newNode );
    removeDuplicates( list );

    return checkNewElement( list.toArray() );
  }

  private static void removeDuplicates( final List<Object> list )
  {
    Object last = null;
    for( final Iterator<Object> iterator = list.iterator(); iterator.hasNext(); )
    {
      final Object next = iterator.next();
      if( ObjectUtils.equals( last, next ) )
        iterator.remove();
      last = next;
    }
  }

  // REMARK: some optimization is done here, in order to enhance performance.
  // We assume, that the same checks has been done for every newly added node, so we check only
  // Criteria, which could go wring for the new node (i.e. the last one in the array).
  private IStatus checkNewElement( final Object[] allNodes )
  {
    try
    {
      final Object newNode = allNodes[allNodes.length - 1];
      final GM_Point newPoint = newNode instanceof GM_Point ? (GM_Point) newNode : ((IFE1D2DNode) newNode).getPoint();

      final IFEDiscretisationModel1d2d discModel = DiscretisationModelUtils.modelForTheme( m_nodeTheme );

      // 0) Node was already grabbed
      if( ArrayUtils.indexOf( allNodes, newNode ) != allNodes.length - 1 )
        return StatusUtilities.createErrorStatus( "Doppelter Knoten" );

      // 1) New Node lies inside an element (only for non-grabbed point)
      if( newNode instanceof GM_Point )
      {
        final IPolyElement elementForNewNode = discModel.find2DElement( newPoint, 0.0 );
        if( elementForNewNode != null )
        {
          final GM_Surface<GM_SurfacePatch> surface = elementForNewNode.getGeometry();
          if( surface.contains( newPoint ) )
            return StatusUtilities.createErrorStatus( "Position innerhalb eines bestehenden Elements" );
        }
      }

      if( allNodes.length < 2 )
        return Status.OK_STATUS;

      // 2) New Edge crosses other edge

      // 3) New edge links two non-adjacent points

      if( allNodes.length < 3 )
        return Status.OK_STATUS;

      final GM_Position[] ring = ringFromNodes( allNodes );

      // 4) New Element self-intersects
      if( GeometryUtilities.isSelfIntersecting( ring ) )
        return StatusUtilities.createErrorStatus( "Ungültiges Polygon: selbstschneidend" );

      // New Element intersects other elements
      final GM_Surface<GM_SurfacePatch> newSurface = GeometryFactory.createGM_Surface( ring, new GM_Position[][] {}, null, KalypsoCorePlugin.getDefault().getCoordinatesSystem() );
      final List<IFE1D2DElement> elements = discModel.getElements().query( newSurface.getEnvelope() );
      for( final IFE1D2DElement element : elements )
      {
        if( element instanceof IElement2D )
        {
          final GM_Surface<GM_SurfacePatch> eleGeom = ((IElement2D) element).getGeometry();
          if( eleGeom.intersects( newSurface ) )
          {
            final GM_Object intersection = eleGeom.intersection( newSurface );
            if( intersection instanceof GM_Surface )
              return StatusUtilities.createErrorStatus( "Neues Element überdeckt vorhandene" );
          }
        }
      }

      return Status.OK_STATUS;
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
      return StatusUtilities.statusFromThrowable( e );
    }
  }

  private GM_Position[] ringFromNodes( final Object[] nodes )
  {
    final GM_Position[] poses = new GM_Position[nodes.length + 1];
    for( int i = 0; i < nodes.length; i++ )
    {
      final Object node = nodes[i];
      if( node instanceof GM_Point )
        poses[i] = ((GM_Point) node).getPosition();
      else if( node instanceof IFE1D2DNode )
        poses[i] = ((IFE1D2DNode) node).getPoint().getPosition();
    }
    // close the ring
    poses[nodes.length] = poses[0];

    return poses;
  }
}
