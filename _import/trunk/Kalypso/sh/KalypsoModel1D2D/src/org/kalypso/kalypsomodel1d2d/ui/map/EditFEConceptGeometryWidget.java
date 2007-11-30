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
import java.util.Collection;
import java.util.List;

import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.kalypsomodel1d2d.ops.GeometryRecalculator;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.widgets.EditGeometryWidget;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
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
 * @author Patrice Congo
 * @author Dejan Antanaskovic
 * 
 */
public class EditFEConceptGeometryWidget extends EditGeometryWidget
{
  private IKalypsoFeatureTheme m_nodeTheme;

  private IKalypsoFeatureTheme m_polyElementTheme;

  private IKalypsoFeatureTheme m_edgeTheme;

  private IKalypsoFeatureTheme m_continuityLineTheme;

  private IKalypsoFeatureTheme m_flowRelationsTheme;

  private IMapModell m_mapModell;

  private IFEDiscretisationModel1d2d m_discModel;

  private MapPanel m_mapPanel;

  private boolean m_snappingActive = true;

  private IFE1D2DNode m_snapNode = null;

  private static final double SNAPPING_RADIUS = 0.5;

  public EditFEConceptGeometryWidget( )
  {
    super( "FE Model Geometrie Editieren", "FE Model Geometrie Editieren" );
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
    m_mapModell = m_mapPanel.getMapModell();
    m_nodeTheme = UtilMap.findEditableTheme( m_mapModell, Kalypso1D2DSchemaConstants.WB1D2D_F_NODE );
    m_polyElementTheme = UtilMap.findEditableTheme( m_mapModell, Kalypso1D2DSchemaConstants.WB1D2D_F_POLY_ELEMENT );
    m_edgeTheme = UtilMap.findEditableTheme( m_mapModell, IFE1D2DEdge.QNAME );
    m_continuityLineTheme = UtilMap.findEditableTheme( m_mapModell, IFELine.QNAME );
    m_flowRelationsTheme = UtilMap.findEditableTheme( m_mapModell, IBoundaryCondition.QNAME );
    m_discModel = UtilMap.findFEModelTheme( m_mapModell );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.EditGeometryWidget#dragged(java.awt.Point)
   */
  @Override
  public void dragged( final Point p )
  {
    if( !m_snappingActive )
    {
      super.dragged( p );
      return;
    }
    final GM_Point currentPosition = MapUtilities.transform( m_mapPanel, p );
    final IFE1D2DNode snapNode = m_discModel.findNode( currentPosition, SNAPPING_RADIUS );
    if( snapNode != null )
    {
      m_snapNode = snapNode;
      final Point snappedPoint = MapUtilities.retransform( m_mapPanel, snapNode.getPoint() );
      super.dragged( snappedPoint );
    }
    else
      super.dragged( p );
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
   * @see org.kalypso.ogc.gml.map.widgets.EditGeometryWidget#perform()
   */
  @Override
  protected Collection<Feature> perform( )
  {
    invalidateLists();
    final Collection<Feature> list = super.perform();
    for( final Feature feature : list )
    {
      final IFE1D2DElement element = (IFE1D2DElement) feature.getAdapter( IFE1D2DElement.class );
      if( element != null )
      {
        final List<IFE1D2DNode> nodes = element.getNodes();
        for( final IFE1D2DNode node : nodes )
        {
          if( m_snappingActive && m_snapNode != null && !m_snapNode.equals( node ) && node.getPoint().distance( m_snapNode.getPoint() ) <= SNAPPING_RADIUS)
          {
              m_discModel.replaceNode( node, m_snapNode );
          }
          else
          {
            final IFeatureWrapperCollection containers = node.getContainers();
            for( final Object containerObject : containers )
            {
              if( containerObject instanceof IFE1D2DEdge )
                ((IFE1D2DEdge) containerObject).recalculateMiddleNodePosition();
            }
          }
        }
      }
    }
    final GeometryRecalculator recalculator = new GeometryRecalculator( list, m_flowRelationsTheme );
    recalculator.fireChanges();
    return list;
  }

  private final void invalidateLists( )
  {
    final IKalypsoFeatureTheme[] themes = { m_nodeTheme, m_edgeTheme, m_polyElementTheme, m_continuityLineTheme, m_flowRelationsTheme };
    for( final IKalypsoFeatureTheme theme : themes )
    {
      if( theme != null )
      {
        final FeatureList featureList = theme.getFeatureList();
        if( featureList != null )
        {
          featureList.invalidate();
        }
      }
    }
  }
}
