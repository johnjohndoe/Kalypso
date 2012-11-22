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
package org.kalypso.kalypsomodel1d2d.ui.map;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Point;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.IFileEditorInput;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.DiscretisationModelUtils;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.element1d.Create2dElementCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * TODO: separate (again) geometry building from 1d2d element building! This class is used to build new 1D2D Elements
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
  private final List<GM_Point> m_nodes = new ArrayList<>();

  private final IKalypsoFeatureTheme m_nodeTheme;

  private boolean m_valid;

  /**
   * The constructor.
   * 
   * @param cnt_points
   *          If >0 the the geometry will be finished, if the count of points is reached. If 0 no rule regarding the
   *          count of the points will apply.
   * @param targetCrs
   *          The target coordinate system.
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
  public final Create2dElementCommand addNode( final GM_Point node ) throws Exception
  {
    m_nodes.add( node );
    removeDuplicates( m_nodes );

    if( m_nodes.size() == m_cnt_points && m_cnt_points != 0 )
    {
      return finish();
    }

    return null;
  }

  /**
   * REMARK: No validity check is done here. Call {@link #checkNewNode(Object)} before a new node is added.
   */
  public final Create2dElementCommand finish( ) throws Exception
  {
    final FeatureList featureList = m_nodeTheme.getFeatureList();
    final Feature parentFeature = featureList.getOwner();

    /* Initialize elements needed for edges and elements */
    final IFEDiscretisationModel1d2d discModel = (IFEDiscretisationModel1d2d)parentFeature;

    final GM_Point[] nodes = m_nodes.toArray( new GM_Point[m_nodes.size()] );
    return new Create2dElementCommand( discModel, nodes );
  }

  public void paint( final Graphics g, final GeoTransform projection, final Point currentPoint )
  {
    // IMPORTANT: we remember GM_Points (not Point's) and re-transform them for painting
    // because the projection depends on the current map-extent, so this builder
    // is stable in regard to zoom in/out
    final int[][] points = UtilMap.getPointArrays( currentPoint, m_nodes, projection );

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
    UtilMap.drawHandles( g, arrayX, arrayY );

    g.setColor( color );

  }

  public final List<GM_Point> getNodes( )
  {
    return m_nodes;
  }

  public int getNumberOfNodes( )
  {
    return m_nodes.size();
  }

  public boolean contains( final IFE1D2DNode snapNode )
  {
    return m_nodes.contains( snapNode );
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

  public IStatus checkNewNode( final GM_Point newNode )
  {
    return checkNewNode( newNode, true );
  }

  /**
   * Checks, if the resulting element would be valid, if the given new node would be inserted at the given position.
   */
  public IStatus checkNewNode( final GM_Point newNode, final boolean pBoolFinalPoint )
  {
    final List<GM_Point> list = new ArrayList<>( m_nodes );
    list.add( newNode );

    removeDuplicates( list );

    final IStatus status = checkNewElement( list.toArray( new GM_Point[list.size()] ), pBoolFinalPoint );

    if( status == Status.OK_STATUS )
      m_valid = true;
    else
      m_valid = false;

    return status;
  }

  // REMARK: some optimization is done here, in order to enhance performance.
  // We assume, that the same checks has been done for every newly added node, so we check only
  // Criteria, which could go wrong for the new node (i.e. the last one in the array).
  private IStatus checkNewElement( final GM_Point[] allNodes, final boolean pBoolFinalPont )
  {
    try
    {
      final GM_Point newPoint = allNodes[allNodes.length - 1];

      final IFEDiscretisationModel1d2d discModel = DiscretisationModelUtils.modelForTheme( m_nodeTheme );

      // 0) Node was already grabbed
      if( ArrayUtils.indexOf( allNodes, newPoint ) != allNodes.length - 1 )
        return new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.ElementGeometryBuilder.0" ) ); //$NON-NLS-1$

      // 1) New Node lies inside an element (only for non-grabbed point)

      final IPolyElement elementForNewNode = discModel.find2DElement( newPoint, 0.0 );
      if( elementForNewNode != null )
      {
        final IFE1D2DNode foundNode = discModel.findNode( newPoint, IFEDiscretisationModel1d2d.CLUSTER_TOLERANCE );
        if( foundNode == null )
        {
          final GM_Polygon surface = elementForNewNode.getGeometry();
          if( surface.contains( newPoint ) )
            return new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, org.kalypso.kalypsomodel1d2d.ui.i18n.Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.ElementGeometryBuilder.6" ) ); //$NON-NLS-1$
        }
      }

      // 1a) Node lies on 1d node
      final IFE1D2DNode node = discModel.findNode( newPoint, IFEDiscretisationModel1d2d.CLUSTER_TOLERANCE);
      if( node != null )
      {
        final IFE1D2DEdge[] nodeEdges = node.getLinkedEdges();
        for( final IFE1D2DEdge nodeItem : nodeEdges )
        {
          final IFE1D2DElement[] containers = nodeItem.getLinkedElements();
          for( final IFE1D2DElement edgeContainer : containers )
          {
            if( edgeContainer instanceof IElement1D )
              return new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString("ElementGeometryBuilder.0") ); //$NON-NLS-1$
          }
        }
      }

      if( allNodes.length < 2 )
        return Status.OK_STATUS;

      // 2) New Edge crosses other edge

      // 2b) First edge crosses other elements
      if( allNodes.length == 2 )
      {
        final GM_Position[] line = ElementGeometryHelper.linePositionsFromNodes( allNodes );

        final GM_Curve curve = GeometryFactory.createGM_Curve( line, KalypsoDeegreePlugin.getDefault().getCoordinateSystem() );
        final List<IFE1D2DElement> elements = discModel.queryElements( curve.getEnvelope(), null );
        for( final IFE1D2DElement element : elements )
        {
          if( element instanceof IPolyElement )
          {
            final GM_Polygon eleGeom = ((IPolyElement)element).getGeometry();
            if( eleGeom == null )
            {
              // check for null geometries... What to do?
              // delete the elements???

              return new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.ElementGeometryBuilder.2" ) ); //$NON-NLS-1$
            }
            if( eleGeom.intersects( curve ) )
            {
              final GM_Object intersection = eleGeom.intersection( curve );
              if( intersection instanceof GM_Curve )
              {
                final GM_Curve intersCurve = (GM_Curve)intersection;
                final GM_Point startPoint = intersCurve.getAsLineString().getStartPoint();
                final GM_Point endPoint = intersCurve.getAsLineString().getEndPoint();

                if( checkIntersectionCurve( allNodes, startPoint, endPoint ) )
                  return new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.ElementGeometryBuilder.3" ) ); //$NON-NLS-1$
              }
            }
          }
        }
      }

      // 3) New edge links two non-adjacent points

      if( allNodes.length < 3 )
        return Status.OK_STATUS;

      final GM_Position[] ring = ElementGeometryHelper.ringPositionsFromNodes( allNodes );

      // 4) New Element self-intersects
      if( GeometryUtilities.isSelfIntersecting( ring ) && pBoolFinalPont )
        return new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.ElementGeometryBuilder.4" ) ); //$NON-NLS-1$

      final GM_Polygon newSurface = GeometryFactory.createGM_Surface( ring, new GM_Position[][] {}, KalypsoDeegreePlugin.getDefault().getCoordinateSystem() );

      // new element is not convex
      if( m_cnt_points > 0 && newSurface.getConvexHull().difference( newSurface ) != null )
        return new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.ElementGeometryBuilder.7" ) ); //$NON-NLS-1$

      // new element intersects other elements
      final List<IFE1D2DElement> elements = discModel.queryElements( newSurface.getEnvelope(), null );
      for( final IFE1D2DElement element : elements )
      {
        if( element instanceof IPolyElement )
        {
          final GM_Polygon eleGeom = ((IPolyElement)element).getGeometry();
          if( eleGeom.intersects( newSurface ) && pBoolFinalPont )
          {
            final GM_Object intersection = eleGeom.intersection( newSurface );
            if( intersection instanceof GM_Polygon )
              return new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.ElementGeometryBuilder.5" ) ); //$NON-NLS-1$
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

  /**
   * checks, if the first and last point lies on the same position as the intersection curve. If this is the case,
   * return false, else return true.
   */
  private boolean checkIntersectionCurve( final GM_Point[] allNodes, final GM_Point startPoint, final GM_Point endPoint )
  {
    if( startPoint.isWithinDistance( allNodes[0], 0.0 ) && endPoint.isWithinDistance( allNodes[1], 0.0 ) )
      return false;
    if( startPoint.isWithinDistance( allNodes[1], 0.0 ) && endPoint.isWithinDistance( allNodes[0], 0.0 ) )
      return false;

    return true;

  }

  public void removeLast( )
  {
    if( m_nodes != null && m_nodes.size() > 0 )
      m_nodes.remove( m_nodes.size() - 1 );
  }
}
