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

import java.awt.Graphics;
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
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * Widget that provides the snapping mechanism for editing the geometry of finite element:
 * <ul>
 * <li>Snapping to the nearest node in the snapping radius is the default behavior.
 * <li>Snapping will not occur if SHIFT button is pressed during the operation.
 * <li>Pressing the ESC button cancels the current operation
 * </ul>
 * 
 * @author Dejan Antanaskovic
 * 
 */
public class SnapToGeometryWidget extends EditGeometryWidget
{
  /** Snapping radius in screen-pixels. */
  public static final int SNAPPING_RADIUS = 20;

  private final List<IFE1D2DNode> m_neighborNodes = new ArrayList<IFE1D2DNode>();

  private IFEDiscretisationModel1d2d m_discModel;

  private boolean m_snappingActive = true;

  private IFE1D2DNode m_snapNode = null;

  private IFE1D2DNode m_startNode;

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

    reinit();
  }

  /**
   * Reset this widget
   */
  protected void reinit( )
  {
    // Search the discretisation Model

    m_discModel = null;

    final IMapModell mapModell = getMapPanel().getMapModell();
    m_discModel = UtilMap.findFEModelTheme( mapModell );

  }

  /**
   * TODO: move to {@link EditFEConceptGeometryWidget}, its only used there...
   * 
   * @see org.kalypso.ogc.gml.map.widgets.EditGeometryWidget#leftPressed(java.awt.Point)
   */
  @Override
  public void leftPressed( final Point p )
  {
    final MapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;

    final GM_Point currentPosition = MapUtilities.transform( mapPanel, p );

    final double snapRadius = MapUtilities.calculateWorldDistance( mapPanel, currentPosition, SNAPPING_RADIUS );
    m_startNode = m_discModel.findNode( currentPosition, snapRadius );

    // create the list of neighbor nodes to exclude from snapping into
    // i.e. to prevent user to create zero-area polygons
    // TODO: This is probably nonsense! Check the complete polygon after editing
    // in order to prevent such things; preventing the snap here only creates
    // new nodes and such stuff...
    m_neighborNodes.clear();
    if( m_startNode != null )
    {
      m_neighborNodes.add( m_startNode );
      final IFeatureWrapperCollection<IFeatureWrapper2> edgeContainers = m_startNode.getContainers();
      for( final IFeatureWrapper2 edgeContainer : edgeContainers )
      {
        if( edgeContainer instanceof IFE1D2DEdge )
        {
          final IFE1D2DNode middleNode = ((IFE1D2DEdge) edgeContainer).getMiddleNode();
          if( middleNode != null && !m_neighborNodes.contains( middleNode ) )
            m_neighborNodes.add( middleNode );
          final IFeatureWrapperCollection<IFeatureWrapper2> elementContainers = ((IFE1D2DEdge) edgeContainer).getContainers();
          for( final IFeatureWrapper2 elementContainer : elementContainers )
          {
            if( elementContainer instanceof IFE1D2DElement )
            {
              final List<IFE1D2DNode> nodes = ((IFE1D2DElement) elementContainer).getNodes();
              for( final IFE1D2DNode node : nodes )
              {
                if( !m_neighborNodes.contains( node ) )
                  m_neighborNodes.add( node );
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
   * TODO: move to {@link EditFEConceptGeometryWidget}, its only used there
   * 
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
      final MapPanel mapPanel = getMapPanel();
      if( isNodeSnappable() && mapPanel != null )
      {
        final Point snappedPoint = MapUtilities.retransform( mapPanel, m_snapNode.getPoint() );

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
    final MapPanel mapPanel = getMapPanel();
    if( mapPanel != null && m_snappingActive && m_discModel != null )
    {
      final GM_Point currentGM_Point = MapUtilities.transform( mapPanel, p );

      final double snapRadius = MapUtilities.calculateWorldDistance( mapPanel, currentGM_Point, SNAPPING_RADIUS );
      m_snapNode = m_discModel.findNode( currentGM_Point, snapRadius );
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

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#keyTyped(java.awt.event.KeyEvent)
   */
  @Override
  public void keyTyped( final KeyEvent e )
  {
    super.keyTyped( e );
    if( KeyEvent.VK_ESCAPE == e.getKeyChar() )
    {
      reinit();
      mapRepaint();
    }
  }

  private final boolean isNodeSnappable( )
  {
    // TODO: is this really ok? Causes NPE if this happens
    if( m_snapNode == null )
      return true;

    // we don't want to snap to our neighbours
    if( m_neighborNodes.contains( m_snapNode ) )
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

  public Point getCurrentMapPoint( )
  {
    return m_currentMapPoint;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    final MapPanel mapPanel = getMapPanel();
    final IFE1D2DNode snapNode = getSnapNode();
    if( snapNode != null && mapPanel != null )
    {
      final Point snappedPoint = MapUtilities.retransform( mapPanel, m_snapNode.getPoint() );
      final int snapRadius = SNAPPING_RADIUS / 2;
      g.drawRect( (int) snappedPoint.getX() - snapRadius, (int) snappedPoint.getY() - snapRadius, SNAPPING_RADIUS, SNAPPING_RADIUS );
    }
  }
}
