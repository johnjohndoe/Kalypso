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
import java.util.ArrayList;
import java.util.List;

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.widgets.EditGeometryWidget;
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

  private final boolean m_snappingActive = true;

  private IFE1D2DNode m_snapNode = null;

  private Point m_currentMapPoint;

  public SnapToGeometryWidget( final String name, final String title )
  {
    super( name, title );
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

  public void snap( final Point p )
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

}
