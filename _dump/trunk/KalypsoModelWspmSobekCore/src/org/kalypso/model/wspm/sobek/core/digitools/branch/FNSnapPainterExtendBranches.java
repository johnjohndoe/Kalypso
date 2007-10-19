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
package org.kalypso.model.wspm.sobek.core.digitools.branch;

import java.awt.Graphics;
import java.awt.Point;
import java.util.ArrayList;
import java.util.List;

import org.kalypso.jts.SnapUtilities.SNAP_TYPE;
import org.kalypso.model.wspm.sobek.core.interfaces.IBranch;
import org.kalypso.model.wspm.sobek.core.interfaces.IModelMember;
import org.kalypso.model.wspm.sobek.core.pub.ISnapPainter;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * @author kuch
 */
public class FNSnapPainterExtendBranches implements ISnapPainter
{
  public static final int RADIUS = 10;

  List<GM_Curve> m_curves = new ArrayList<GM_Curve>();

  public FNSnapPainterExtendBranches( final IModelMember modelMember )
  {
    discoverBranchGeometries( modelMember );
  }

  private void discoverBranchGeometries( final IModelMember modelMember )
  {

    final IBranch[] branches = modelMember.getBranchMembers();
    for( final IBranch branch : branches )
    {
      final GM_Curve curve = branch.getGeometryProperty();
      m_curves.add( curve );
    }
  }

  /**
   * @see org.kalypso.nofdpidss.ui.application.flow.network.ISnapPainter#isSnapPoaint(org.kalypso.ogc.gml.map.MapPanel,
   *      java.awt.Point)
   */
  public GM_Point getSnapPoint( final MapPanel panel, final GM_Point point )
  {
    try
    {
      final Point p = MapUtilities.retransform( panel, point );

      for( final GM_Curve curve : m_curves )
      {
        final GM_Point pSnap = MapUtilities.snap( panel, curve, p, FNSnapPainterExtendBranches.RADIUS, SNAP_TYPE.SNAP_AUTO );
        if( pSnap != null )
          return pSnap;
      }
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
    }

    return null;
  }

  /**
   * @see org.kalypso.nofdpidss.ui.application.flow.network.ISnapPainter#paint(java.awt.Graphics,
   *      org.kalypsodeegree.graphics.transformation.GeoTransform, java.awt.Point)
   */
  public Point paint( final Graphics g, final MapPanel panel, final Point currentPoint )
  {
    try
    {
      if( g == null || panel == null || currentPoint == null )
        return null;

      for( final GM_Curve curve : m_curves )
      {
        final GM_Point pSnap = MapUtilities.snap( panel, curve, currentPoint, FNSnapPainterExtendBranches.RADIUS, SNAP_TYPE.SNAP_AUTO );

        if( pSnap != null )
        {
          final Point point = MapUtilities.retransform( panel, pSnap );
          // $ANALYSIS-IGNORE
          g.drawRect( (int) point.getX() - 10, (int) point.getY() - 10, 20, 20 );

          return point;
        }
      }
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
    }

    return null;
  }
}
