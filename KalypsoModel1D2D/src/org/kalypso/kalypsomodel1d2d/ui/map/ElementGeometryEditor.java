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
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.afgui.model.Util;
import org.kalypso.commons.command.ICommand;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.DiscretisationModelUtils;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.Element1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.ChangeNodePositionCommand;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Ring;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * This class is used to edit 1D2D Elements
 * 
 * @author Thomas Jung
 */
public class ElementGeometryEditor
{
  private static final double SEARCH_DISTANCE = 0.1;

  private final Set<IFE1D2DElement> m_elementList = new HashSet<>();

  private final IKalypsoFeatureTheme m_nodeTheme;

  private IFE1D2DNode m_startNode;

  private IFE1D2DNode m_endNode;

  private GM_Point m_endPoint;

  private final IMapPanel m_mapPanel;

  private boolean m_valid;

  public ElementGeometryEditor( final IMapPanel panel, final IKalypsoFeatureTheme nodeTheme )
  {
    m_mapPanel = panel;
    m_nodeTheme = nodeTheme;
  }

  private void addElements( final IFE1D2DElement[] elements )
  {
    for( final IFE1D2DElement element : elements )
    {
      m_elementList.add( element );
    }
  }

  /**
   * REMARK: No validity check is done here. Call {@link #checkNewNode(Object)} before a new node is added.
   */
  public void finish( final IFEDiscretisationModel1d2d discModel ) throws Exception
  {
    final double z = m_startNode.getPoint().getZ();

    final GM_Point newPosition = GeometryFactory.createGM_Point( m_endPoint.getX(), m_endPoint.getY(), z, m_endPoint.getCoordinateSystem() );
    final ICommand changeCommand = new ChangeNodePositionCommand( discModel, m_startNode, newPosition, true );
    Util.postCommand( IFEDiscretisationModel1d2d.class, changeCommand );
  }

  public void paint( final Graphics g, final GeoTransform projection, final Point currentPoint )
  {
    if( m_startNode != null )
      paintPreview( g, projection, currentPoint );
  }

  private void paintPreview( final Graphics g, final GeoTransform projection, final Point currentPoint )
  {
    final IFE1D2DElement[] elements = m_elementList.toArray( new IFE1D2DElement[m_elementList.size()] );
    for( final IFE1D2DElement element : elements )
    {
      final List<GM_Point> pointsToDraw = new ArrayList<>();

      final IFE1D2DNode[] nodes = element.getNodes();
      for( final IFE1D2DNode node : nodes )
      {
        if( node.equals( m_startNode ) )
          pointsToDraw.add( MapUtilities.transform( m_mapPanel, currentPoint ) );
        else
          pointsToDraw.add( node.getPoint() );
      }
      paintPreviewElement( g, projection, pointsToDraw.toArray( new GM_Point[pointsToDraw.size()] ) );
    }
  }

  private void paintPreviewElement( final Graphics g, final GeoTransform projection, final GM_Point[] points )
  {
    for( int i = 0; i < points.length - 1; i++ )
    {
      /* paint a line between start point and current position. */
      final int[][] drawPoints = ElementGeometryHelper.getPolygonAsPointArrays( projection, points );

      final int[] arrayX = drawPoints[0];
      final int[] arrayY = drawPoints[1];

      final Color color = g.getColor();

      Color preViewColor;
      if( m_valid == true )
        preViewColor = new Color( 100, 255, 100 );
      else
        preViewColor = new Color( 255, 100, 100 );

      g.setColor( preViewColor );

      // line
      g.drawPolygon( arrayX, arrayY, arrayX.length );

      g.setColor( color );
    }

  }

  /**
   * REMARK: some optimisation is done here, in order to enhance performance. We assume, that the same checks has been
   * done for every newly added node, so we check only Criteria, which could go wrong for the new node (i.e. the last
   * one in the array).
   */
  private IStatus checkNewElements( )
  {
    try
    {
      final IFEDiscretisationModel1d2d discModel = DiscretisationModelUtils.modelForTheme( m_nodeTheme );

      /* A) element type checks */
      // A.1) check for 1d-elements (they are not supported yet)
      final IFE1D2DElement[] startElements = m_startNode.getAdjacentElements();
      for( final IFE1D2DElement element : startElements )
      {
        if( element instanceof Element1D )
          return new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.ElementGeometryEditor.0" ) ); //$NON-NLS-1$
      }

      /* B) node position checks */
      // B.1) Node was already grabbed =>
      if( m_endNode != null )
      {
        if( m_startNode.equals( m_endNode ) )
          return new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.ElementGeometryEditor.1" ) ); //$NON-NLS-1$
      }

      // B.2) New Node lies inside an element, that is not in the current elements list
      // (only for non-grabbed point)
      if( m_endNode == null )
      {
        final IPolyElement elementForNewNode = discModel.find2DElement( m_endPoint, 0.0 );
        if( elementForNewNode != null )
        {
          final GM_Polygon surface = elementForNewNode.getGeometry();
          if( surface.contains( m_endPoint ) && !m_elementList.contains( elementForNewNode ) )
            return new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.ElementGeometryEditor.2" ) ); //$NON-NLS-1$
        }
      }
      else
        // B.3) right now we don't allow that the new Node lies on an already existing node
        return new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.ElementGeometryEditor.3" ) ); //$NON-NLS-1$

      /* C) edge checks */
      // C.1) one of the new edges crosses other edges
      // C.2) one of the new edges links two non-adjacent points
      /* D) element geometry checks */
      final String crs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
      final GM_Ring[] rings = getNewGeometries();
      for( final GM_Ring ring : rings )
      {
        final GM_Position[] poses = ring.getPositions();
        // D.1) new element self-intersects
        if( GeometryUtilities.isSelfIntersecting( poses ) )
          return new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.ElementGeometryEditor.4" ) ); //$NON-NLS-1$

        final GM_Polygon newSurface = GeometryFactory.createGM_Surface( poses, new GM_Position[][] {}, crs );

        // D.2) new element is not convex
        if( newSurface.getConvexHull().difference( newSurface ) != null && poses.length < 4 )
          return new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.ElementGeometryEditor.7" ) ); //$NON-NLS-1$

        // D.3) new elements intersect other elements
        final List<IFE1D2DElement> elements = discModel.queryElements( newSurface.getEnvelope(), null );
        for( final IFE1D2DElement element : elements )
        {
          if( element instanceof IPolyElement )
          {
            final IPolyElement element2D = (IPolyElement)element;
            final GM_Polygon eleGeom = element2D.getGeometry();
            if( eleGeom.intersects( newSurface ) && !m_elementList.contains( element2D ) )
            {
              final GM_Object intersection = eleGeom.intersection( newSurface );
              if( intersection instanceof GM_Polygon )
                return new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.ElementGeometryEditor.5" ) ); //$NON-NLS-1$
            }
          }
        }

        /* E) Boundary Condition check */
        final GM_Position[] positions = ring.getPositions();
        for( final GM_Position position : positions )
        {
          final GM_Point point = GeometryFactory.createGM_Point( position, crs );
          final IFELine contiLine = discModel.findContinuityLine( point, SEARCH_DISTANCE );
          if( contiLine != null )
            return new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.ElementGeometryEditor.6" ) ); //$NON-NLS-1$
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
   * generates the new element geometries based on the end node (only 2D-Elements hare andled)
   */
  private GM_Ring[] getNewGeometries( ) throws GM_Exception
  {
    final List<GM_Ring> ringList = new ArrayList<>();
    for( final IFE1D2DElement element : m_elementList )
    {
      if( element instanceof IPolyElement )
        ringList.add( getEditedGeometryAsRing( element ) );
    }
    return ringList.toArray( new GM_Ring[ringList.size()] );
  }

  /**
   * returns a given {@link IFE1D2DElement} as a {@link GM_Ring}
   */
  private GM_Ring getEditedGeometryAsRing( final IFE1D2DElement element ) throws GM_Exception
  {
    final GM_Position[] poses = getEditedNodesPositions( element );

    final String crs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
    return GeometryFactory.createGM_Ring( poses, crs );
  }

  /**
   * returns the updated geometry of an edited {@link IFE1D2DElement} as {@link GM_Position} array. <BR>
   * The z-coordinate value of the moved element node remains the same.
   */
  private GM_Position[] getEditedNodesPositions( final IFE1D2DElement element )
  {
    final List<GM_Position> posList = new ArrayList<>();
    final IFE1D2DNode[] nodes = element.getNodes();
    for( final IFE1D2DNode node : nodes )
    {
      if( node.equals( m_startNode ) )
      {
        final GM_Position position = m_endPoint.getPosition();
        final double x = position.getX();
        final double y = position.getY();
        double z;
        if( m_startNode.getPoint().getCoordinateDimension() == 3 )
        {
          z = m_startNode.getPoint().getPosition().getZ();
          posList.add( GeometryFactory.createGM_Position( x, y, z ) );
        }
        else
          posList.add( GeometryFactory.createGM_Position( x, y ) );
      }
      else
      {
        final GM_Position position = node.getPoint().getPosition();
        final double x = position.getX();
        final double y = position.getY();
        double z;
        if( node.getPoint().getCoordinateDimension() == 3 )
        {
          z = position.getZ();
          posList.add( GeometryFactory.createGM_Position( x, y, z ) );
        }
        else
          posList.add( GeometryFactory.createGM_Position( x, y ) );
      }
    }

    return posList.toArray( new GM_Position[posList.size()] );
  }

  public IFE1D2DNode getStartNode( )
  {
    return m_startNode;
  }

  public void setStartNode( final IFE1D2DNode startNode )
  {
    m_startNode = startNode;

    if( startNode != null && m_elementList.size() == 0 )
    {
      addElements( startNode.getAdjacentElements() );
    }
  }

  /**
   * check the node
   */
  public IStatus checkNewNode( final Object newNode )
  {
    /* set the new end node candidate */
    if( newNode instanceof IFE1D2DNode )
    {
      m_endNode = (IFE1D2DNode)newNode;
      m_endPoint = m_endNode.getPoint();
    }
    else
    {
      m_endNode = null;
      m_endPoint = (GM_Point)newNode;
    }

    final IStatus status = checkNewElements();
    if( status == Status.OK_STATUS )
      m_valid = true;
    else
      m_valid = false;

    return status;
  }

  public boolean isValid( )
  {
    return m_valid;
  }
}
