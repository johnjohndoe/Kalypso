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

import java.awt.Point;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.List;

import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.widgets.EditGeometryWidget;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * {@link IWidget} that provide the mechnism for edition the geometrie of finite element concepts (Node, Edge, elements,
 * and Complex elements) This class decorate the {@link EditGeometryWidget} with the capability to :
 * <ul>
 * <li/>find all feature affected by a geometric change in the edited fe concepts; <li/>invalidate the envelops of the
 * found feature <li/> and fire feature change event holding the affected feature
 * </ul>
 * 
 * This widget rely on the assumtion that the map to edit has layer holding feture with the QName
 * {@link Kalypso1D2DSchemaConstants#WB1D2D_F_NODE}
 * 
 * @author Dejan Antanaskovic
 * 
 */
public class SnapToGeometryWidget extends EditGeometryWidget
{
  protected IFEDiscretisationModel1d2d m_discModel;

  private MapPanel m_mapPanel;

  private boolean m_snappingActive = true;

  private IFE1D2DNode m_snapNode = null;

  private IFE1D2DNode m_startNode;

  private List<IFE1D2DNode> m_neighbourNodes = new ArrayList<IFE1D2DNode>();

  private static final double SNAPPING_RADIUS = 50.0;

  private Point m_currentMapPoint;

  public SnapToGeometryWidget( final String name, final String title )
  {
    super( name, title );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public void activate( final ICommandTarget commandPoster, final MapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );
    m_mapPanel = mapPanel;
    m_discModel = UtilMap.findFEModelTheme( mapPanel.getMapModell() );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.EditGeometryWidget#leftPressed(java.awt.Point)
   */
  @Override
  public void leftPressed( final Point p )
  {
    final GM_Point currentPosition = MapUtilities.transform( m_mapPanel, p );
    m_startNode = m_discModel.findNode( currentPosition, SNAPPING_RADIUS );

    // create the list of neighbour nodes to exclude from snapping into
    // i.e. to prevent user to create zero-area polygons
    m_neighbourNodes.clear();
    if( m_startNode != null )
    {
      m_neighbourNodes.add( m_startNode );
      final IFeatureWrapperCollection<IFeatureWrapper2> edgeContainers = m_startNode.getContainers();
      for( final IFeatureWrapper2 edgeContainer : edgeContainers )
      {
        if( edgeContainer instanceof IFE1D2DEdge )
        {
          final IFE1D2DNode middleNode = ((IFE1D2DEdge) edgeContainer).getMiddleNode();
          if( middleNode != null && !m_neighbourNodes.contains( middleNode ) )
            m_neighbourNodes.add( middleNode );
          final IFeatureWrapperCollection<IFeatureWrapper2> elementContainers = ((IFE1D2DEdge) edgeContainer).getContainers();
          for( final IFeatureWrapper2 elementContainer : elementContainers )
          {
            if( elementContainer instanceof IFE1D2DElement )
            {
              final List<IFE1D2DNode> nodes = ((IFE1D2DElement) elementContainer).getNodes();
              for( final IFE1D2DNode node : nodes )
              {
                if( !m_neighbourNodes.contains( node ) )
                  m_neighbourNodes.add( node );
              }
            }
          }
        }
      }
    }
    super.leftPressed( p );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.EditGeometryWidget#moved(java.awt.Point)
   */
  @Override
  public void moved( final Point p )
  {
    snap( p );
    if( m_snapNode != null )
      m_currentMapPoint = MapUtilities.retransform( getMapPanel(), m_snapNode.getPoint() );
    else
      m_currentMapPoint = p;
    super.moved( m_currentMapPoint );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.EditGeometryWidget#dragged(java.awt.Point)
   */
  @Override
  public void dragged( final Point p )
  {
    snap( p );
    if( m_snapNode == null )

      super.dragged( p );
    else
    {
      if( isNodeSnappable() )
      {
        final Point snappedPoint = MapUtilities.retransform( m_mapPanel, m_snapNode.getPoint() );
        super.dragged( snappedPoint );
      }
      else
      {
        m_snapNode = null;
        super.dragged( p );
      }
    }
  }

  private void snap( final Point p )
  {
    if( m_snappingActive && m_discModel != null )
    {
      final GM_Point currentGM_Point = MapUtilities.transform( m_mapPanel, p );
      m_snapNode = m_discModel.findNode( currentGM_Point, SNAPPING_RADIUS );
    }
    else
      m_snapNode = null;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#keyPressed(java.awt.event.KeyEvent)
   */
  @Override
  public void keyPressed( final KeyEvent e )
  {
    if( e.getKeyCode() == KeyEvent.VK_SHIFT )
      m_snappingActive = false;
    super.keyPressed( e );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#keyReleased(java.awt.event.KeyEvent)
   */
  @Override
  public void keyReleased( final KeyEvent e )
  {
    if( e.getKeyCode() == KeyEvent.VK_SHIFT )
      m_snappingActive = true;
    super.keyReleased( e );
  }

  protected void mapRepaint( )
  {
    final MapPanel panel = getMapPanel();
    if( panel != null )
      panel.repaint();
  }

  private final boolean isNodeSnappable( )
  {
    if( m_snapNode == null )
      return true;
    // we don't want to snap to our neighbours
    if( m_neighbourNodes.contains( m_snapNode ) )
      return false;
    // neither to some middle node
    if( m_snapNode.getContainers().size() < 2 )
      return false;
    return true;
  }

  public IFEDiscretisationModel1d2d getDiscModel( )
  {
    return m_discModel;
  }

  public boolean isSnappingActive( )
  {
    return m_snappingActive;
  }

  public IFE1D2DNode getSnapNode( )
  {
    return m_snapNode;
  }

  public IFE1D2DNode getStartNode( )
  {
    return m_startNode;
  }

  public List<IFE1D2DNode> getNeighbourNodes( )
  {
    return m_neighbourNodes;
  }

  public static double getSnappingRadius( )
  {
    return SNAPPING_RADIUS;
  }

  public Point getCurrentMapPoint( )
  {
    return m_currentMapPoint;
  }

}
