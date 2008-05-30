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
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypsomodel1d2d.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.DiscretisationModelUtils;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.Element1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.Element2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.FE1D2DDiscretisationModel;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.DeleteCmdFactory;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.command.CompositeCommand;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Ring;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * 
 * This class is used to edit 1D2D Elements
 * 
 * @author Thomas Jung
 */
public class ElementGeometryEditor
{
  private static final double SEARCH_DISTANCE = 0.1;

  @SuppressWarnings("unchecked")
  private final Set<IFE1D2DElement> m_elementList = new HashSet<IFE1D2DElement>();

  private final IKalypsoFeatureTheme m_nodeTheme;

  @SuppressWarnings("unchecked")
  private IFE1D2DNode m_startNode;

  @SuppressWarnings("unchecked")
  private IFE1D2DNode m_endNode;

  private GM_Point m_endPoint;

  private final MapPanel m_mapPanel;

  private boolean m_valid;

  public ElementGeometryEditor( final MapPanel panel, final IKalypsoFeatureTheme nodeTheme )
  {
    m_mapPanel = panel;
    m_nodeTheme = nodeTheme;
  }

  @SuppressWarnings("unchecked")
  public void addElements( IFE1D2DElement[] elements )
  {
    for( int i = 0; i < elements.length; i++ )
    {
      m_elementList.add( elements[i] );
    }
  }

  /**
   * REMARK: No validity check is done here. Call {@link #checkNewNode(Object)} before a new node is added.
   * 
   * @see org.kalypso.informdss.manager.util.widgets.IGeometryBuilder#finish()
   */
  @SuppressWarnings("unchecked")
  public void finish( ) throws Exception
  {
    final CommandableWorkspace workspace = m_nodeTheme.getWorkspace();
    final FeatureList featureList = m_nodeTheme.getFeatureList();
    final Feature parentFeature = featureList.getParentFeature();

    /* Initialize elements needed for edges and elements */
    final IFEDiscretisationModel1d2d discModel = new FE1D2DDiscretisationModel( parentFeature );

    // add remove element command
    for( IFE1D2DElement element : m_elementList )
    {
      if( element instanceof Element2D )
      {
        final IDiscrModel1d2dChangeCommand deleteCmd = DeleteCmdFactory.createDeleteCmd( element.getFeature(), discModel );
        m_nodeTheme.getWorkspace().postCommand( deleteCmd );
      }
    }

    /* create new elements */
    for( IFE1D2DElement element : m_elementList )
    {
      final CompositeCommand command = new CompositeCommand( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.ElementGeometryBuilder.1" ) ); //$NON-NLS-1$

      final List<GM_Point> points = new ArrayList<GM_Point>();

      if( element instanceof Element2D )
      {
        // get nodes array of the new geometries
        final GM_Ring ring = getEditedGeometryAsRing( element );
        final GM_Position[] positions = ring.getPositions();

        for( int i = 0; i < positions.length - 1; i++ )
        {
          final GM_Point point = GeometryFactory.createGM_Point( positions[i], KalypsoDeegreePlugin.getDefault().getCoordinateSystem() );
          points.add( point );
        }
      }

      // create the new elements
      ElementGeometryHelper.createAdd2dElement( command, workspace, parentFeature, discModel, points );

      m_nodeTheme.getWorkspace().postCommand( command );
    }
  }

  /**
   * @see org.kalypso.informdss.manager.util.widgets.IGeometryBuilder#paint(java.awt.Graphics,
   *      org.kalypsodeegree.graphics.transformation.GeoTransform)
   */
  public void paint( final Graphics g, final GeoTransform projection, final Point currentPoint )
  {
    if( m_startNode != null )
    {
      // /* paint a line between start point and current position. */
      // final int[][] points = getLineAsPointArrays( projection, m_startNode.getPoint(), currentPoint );
      //
      // int[] arrayX = points[0];
      // int[] arrayY = points[1];
      //
      // // line
      // g.drawPolygon( arrayX, arrayY, arrayX.length );
      // // small start point rect
      // drawHandles( g, arrayX, arrayY );

      // paint preview
      paintPreview( g, projection, currentPoint );
    }
  }

  @SuppressWarnings("unchecked")
  private void paintPreview( Graphics g, GeoTransform projection, Point currentPoint )
  {
    IFE1D2DElement[] elements = m_elementList.toArray( new IFE1D2DElement[m_elementList.size()] );
    for( IFE1D2DElement element : elements )
    {
      List<GM_Point> pointsToDraw = new ArrayList<GM_Point>();

      List<IFE1D2DNode> nodes = element.getNodes();
      for( int i = 0; i < nodes.size(); i++ )
      {
        if( nodes.get( i ).equals( m_startNode ) )
          pointsToDraw.add( MapUtilities.transform( m_mapPanel, currentPoint ) );
        else
          pointsToDraw.add( nodes.get( i ).getPoint() );
      }
      paintPreviewElement( g, projection, pointsToDraw.toArray( new GM_Point[pointsToDraw.size()] ) );
    }
  }

  private void paintPreviewElement( Graphics g, GeoTransform projection, GM_Point[] points )
  {
    for( int i = 0; i < points.length - 1; i++ )
    {
      /* paint a line between start point and current position. */
      final int[][] drawPoints = ElementGeometryHelper.getPolygonAsPointArrays( projection, points );

      int[] arrayX = drawPoints[0];
      int[] arrayY = drawPoints[1];

      final Color color = g.getColor();

      Color preViewColor;
      if( m_valid == true )
        preViewColor = new Color( 100, 255, 100 );
      else
        preViewColor = new Color( 255, 100, 100 );

      g.setColor( preViewColor );

      // line
      g.drawPolygon( arrayX, arrayY, arrayX.length );
      // small start point rect
      // drawHandles( g, arrayX, arrayY );

      g.setColor( color );
    }

  }

  /**
   * REMARK: some optimization is done here, in order to enhance performance. We assume, that the same checks has been
   * done for every newly added node, so we check only Criteria, which could go wring for the new node (i.e. the last
   * one in the array).
   */
  @SuppressWarnings("unchecked")
  private IStatus checkNewElements( )
  {
    try
    {

      final IFEDiscretisationModel1d2d discModel = DiscretisationModelUtils.modelForTheme( m_nodeTheme );

      /* A) element type checks */
      // A.1) check for 1d-elements (they are not supported yet)
      final IFE1D2DElement[] startElements = m_startNode.getElements();
      for( IFE1D2DElement element : startElements )
      {
        if( element instanceof Element1D )
          return StatusUtilities.createErrorStatus( "1D Knoten können nicht verschoben werden" );
      }

      /* B) node position checks */
      // B.1) Node was already grabbed =>
      if( m_endNode != null )
      {
        if( m_startNode.equals( m_endNode ) )
          return StatusUtilities.createErrorStatus( "Anfangs- und Endknoten identisch" );
      }

      // B.2) New Node lies inside an element, that is not in the current elements list
      // (only for non-grabbed point)
      if( m_endNode == null )
      {
        final IPolyElement elementForNewNode = discModel.find2DElement( m_endPoint, 0.0 );
        if( elementForNewNode != null )
        {
          final GM_Surface<GM_SurfacePatch> surface = elementForNewNode.getGeometry();
          if( surface.contains( m_endPoint ) && !m_elementList.contains( elementForNewNode ) )
            return StatusUtilities.createErrorStatus( "Position innerhalb eines bestehenden Elements" );
        }
      }
      else
        // B.3) right now we don't allow that the new Node lies on an already existing node
        return StatusUtilities.createErrorStatus( "Position liegt auf bereits vorhandenem Knoten" );

      /* C) edge checks */
      // C.1) one of the new edges crosses other edges
      // C.2) one of the new edges links two non-adjacent points
      /* D) element geometry checks */
      final String crs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
      final GM_Ring[] rings = getNewGeometries();
      for( GM_Ring ring : rings )
      {
        final GM_Position[] poses = ring.getPositions();
        // D.1) New Element self-intersects
        if( GeometryUtilities.isSelfIntersecting( poses ) )
          return StatusUtilities.createErrorStatus( "Ungültiges Polygon: selbstschneidend" );

        // D.2) new elements intersect other elements
        final GM_Surface<GM_SurfacePatch> newSurface = GeometryFactory.createGM_Surface( poses, new GM_Position[][] {}, null, crs );
        final List<IFE1D2DElement> elements = discModel.getElements().query( newSurface.getEnvelope() );
        for( final IFE1D2DElement element : elements )
        {
          if( element instanceof IElement2D )
          {
            final IElement2D element2D = (IElement2D) element;
            final GM_Surface<GM_SurfacePatch> eleGeom = element2D.getGeometry();
            if( eleGeom.intersects( newSurface ) && !m_elementList.contains( element2D ) )
            {
              final GM_Object intersection = eleGeom.intersection( newSurface );
              if( intersection instanceof GM_Surface )
                return StatusUtilities.createErrorStatus( "Neues Element überdeckt vorhandene" );
            }
          }
        }

        /* E) Boundary Condition check */
        GM_Position[] positions = ring.getPositions();
        for( int i = 0; i < positions.length; i++ )
        {
          final GM_Point point = GeometryFactory.createGM_Point( positions[i], crs );
          final IFELine contiLine = discModel.findContinuityLine( point, SEARCH_DISTANCE );
          if( contiLine != null )
            return StatusUtilities.createErrorStatus( "Zu verändernde Elemente enthalten eine Kontinuitätslinie" );
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
  @SuppressWarnings("unchecked")
  private GM_Ring[] getNewGeometries( ) throws GM_Exception
  {
    final List<GM_Ring> ringList = new ArrayList<GM_Ring>();
    for( IFE1D2DElement element : m_elementList )
    {
      if( element instanceof IElement2D )
        ringList.add( getEditedGeometryAsRing( element ) );
    }
    return ringList.toArray( new GM_Ring[ringList.size()] );
  }

  /**
   * returns a given {@link IFE1D2DElement} as a {@link GM_Ring}
   */
  @SuppressWarnings("unchecked")
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
  @SuppressWarnings("unchecked")
  private GM_Position[] getEditedNodesPositions( final IFE1D2DElement element )
  {
    final List<GM_Position> posList = new ArrayList<GM_Position>();
    final List<IFE1D2DNode> nodes = element.getNodes();
    for( int i = 0; i < nodes.size(); i++ )
    {
      final IFE1D2DNode node = nodes.get( i );
      if( node.equals( m_startNode ) )
      {
        final GM_Position position = m_endPoint.getPosition();
        double x = position.getX();
        double y = position.getY();
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
        double x = position.getX();
        double y = position.getY();
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
    final GM_Position[] poses = posList.toArray( new GM_Position[posList.size()] );
    return poses;
  }

  @SuppressWarnings("unchecked")
  public IFE1D2DNode getStartNode( )
  {
    return m_startNode;
  }

  @SuppressWarnings("unchecked")
  public void setStartNode( IFE1D2DNode startNode )
  {
    m_startNode = startNode;

    if( startNode != null && m_elementList.size() == 0 )
    {
      addElements( startNode.getElements() );
    }
  }

  /**
   * check the node
   */
  @SuppressWarnings("unchecked")
  public IStatus checkNewNode( Object newNode )
  {
    /* set the new end node candidate */
    if( newNode instanceof IFE1D2DNode )
    {
      m_endNode = (IFE1D2DNode) newNode;
      m_endPoint = m_endNode.getPoint();
    }
    else
    {
      m_endNode = null;
      m_endPoint = (GM_Point) newNode;
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
