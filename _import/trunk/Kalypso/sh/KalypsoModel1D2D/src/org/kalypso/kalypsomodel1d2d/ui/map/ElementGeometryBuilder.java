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

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Point;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.ObjectUtils;
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
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.command.CompositeCommand;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Curve;
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
  // search distance in meter
  private static final double SEARCH_DISTANCE = 0.10;

  /**
   * Stores the count of points which this geometry must have. If it is 0, there is no rule.
   */
  private final int m_cnt_points;

  /** A list of locations for new nodes. */
  private final List<GM_Point> m_nodes = new ArrayList<GM_Point>();

  private final IKalypsoFeatureTheme m_nodeTheme;

  private boolean m_valid;

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
  public ICommand addNode( final GM_Point node ) throws Exception
  {
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
  @SuppressWarnings("unchecked")
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
    ElementGeometryHelper.createAddElement( command, workspace, parentFeature, parentNodeProperty, parentEdgeProperty, parentElementProperty, nodeContainerPT, edgeContainerPT, discModel, m_nodes );

    return command;
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
    final Color color = g.getColor();

    Color preViewColor;
    if( m_valid == true )
      preViewColor = new Color( 100, 255, 100 );
    else
      preViewColor = new Color( 255, 100, 100 );

    g.setColor( preViewColor );

    g.drawPolygon( arrayX, arrayY, arrayX.length );
    drawHandles( g, arrayX, arrayY );

    g.setColor( color );

  }

  private int[][] getPointArrays( final GeoTransform projection, final Point currentPoint )
  {
    final List<Integer> xArray = new ArrayList<Integer>();
    final List<Integer> yArray = new ArrayList<Integer>();

    for( final GM_Point point : m_nodes )
    {
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

  @SuppressWarnings("unchecked")
  public boolean contains( final IFE1D2DNode snapNode )
  {
    return m_nodes.contains( snapNode );
  }

  /**
   * Checks, if the resulting element would be valid, if the given new node would be inserted at the given position.
   */
  public IStatus checkNewNode( final GM_Point newNode )
  {
    final List<GM_Point> list = new ArrayList<GM_Point>( m_nodes );
    list.add( newNode );
    removeDuplicates( list );
    IStatus status = checkNewElement( list.toArray( new GM_Point[list.size()] ) );

    if( status == Status.OK_STATUS )
      m_valid = true;
    else
      m_valid = false;

    return status;
  }

  private static void removeDuplicates( final List<GM_Point> list )
  {
    Object last = null;
    for( final Iterator<GM_Point> iterator = list.iterator(); iterator.hasNext(); )
    {
      final GM_Point next = iterator.next();
      if( ObjectUtils.equals( last, next ) )
        iterator.remove();
      last = next;
    }
  }

  // REMARK: some optimization is done here, in order to enhance performance.
  // We assume, that the same checks has been done for every newly added node, so we check only
  // Criteria, which could go wring for the new node (i.e. the last one in the array).
  @SuppressWarnings("unchecked")
  private IStatus checkNewElement( final GM_Point[] allNodes )
  {
    try
    {
      final GM_Point newPoint = allNodes[allNodes.length - 1];

      final IFEDiscretisationModel1d2d discModel = DiscretisationModelUtils.modelForTheme( m_nodeTheme );

      // 0) Node was already grabbed
      if( ArrayUtils.indexOf( allNodes, newPoint ) != allNodes.length - 1 )
        return StatusUtilities.createErrorStatus( "Doppelter Knoten" );

      // 1) New Node lies inside an element (only for non-grabbed point)

      final IPolyElement elementForNewNode = discModel.find2DElement( newPoint, 0.0 );
      if( elementForNewNode != null )
      {
        final IFE1D2DNode foundNode = discModel.findNode( newPoint, SEARCH_DISTANCE );
        if( foundNode == null )
        {
          final GM_Surface<GM_SurfacePatch> surface = elementForNewNode.getGeometry();
          if( surface.contains( newPoint ) )
            return StatusUtilities.createErrorStatus( "Position innerhalb eines bestehenden Elements" );
        }
      }

      if( allNodes.length < 2 )
        return Status.OK_STATUS;

      // 2) New Edge crosses other edge

      // 2b) First edge crosses other elements
      if( allNodes.length == 2 )
      {
        final GM_Position[] line = ElementGeometryHelper.linePositionsFromNodes( allNodes );

        final GM_Curve curve = GeometryFactory.createGM_Curve( line, KalypsoCorePlugin.getDefault().getCoordinatesSystem() );
        final List<IFE1D2DElement> elements = discModel.getElements().query( curve.getEnvelope() );
        for( final IFE1D2DElement element : elements )
        {
          if( element instanceof IElement2D )
          {
            final GM_Surface<GM_SurfacePatch> eleGeom = ((IElement2D) element).getGeometry();
            if( eleGeom.intersects( curve ) )
            {
              final GM_Object intersection = eleGeom.intersection( curve );
              if( intersection instanceof GM_Curve )
                return StatusUtilities.createErrorStatus( "Neues Element überdeckt vorhandene" );
            }
          }
        }

      }
      // 3) New edge links two non-adjacent points

      if( allNodes.length < 3 )
        return Status.OK_STATUS;

      final GM_Position[] ring = ElementGeometryHelper.ringPositionsFromNodes( allNodes );

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

}
