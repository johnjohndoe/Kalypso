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
package org.kalypso.model.wspm.sobek.core.digitools.crosssection;

import java.awt.Graphics;
import java.awt.Point;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import javax.xml.namespace.QName;

import org.kalypso.jts.SnapUtilities.SNAP_TYPE;
import org.kalypso.model.wspm.sobek.core.interfaces.IBranch;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekModelMember;
import org.kalypso.model.wspm.sobek.core.pub.ISnapPainter;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * @author kuch
 */
public class FNSnapPainterCreateProfileNode implements ISnapPainter
{
  public static final int RADIUS = 10;

  Map<IBranch, GM_Curve> m_curves = new HashMap<IBranch, GM_Curve>();

  Map<Feature, GM_Curve> m_profiles = new HashMap<Feature, GM_Curve>();

  private IBranch m_lastSnappedBranch = null;

  private Feature m_lastSnappedCrossSection = null;

  public FNSnapPainterCreateProfileNode( final ISobekModelMember model )
  {
    discoverBranchGeometries( model );
    discoverCrossSections( model.getWorkspace() );

  }

  private void discoverCrossSections( final CommandableWorkspace workspace )
  {
    final Feature root = workspace.getRootFeature();
    final List< ? > waterbodies = (List< ? >) root.getProperty( new QName( "org.kalypso.nofdpidss.1dmodel", "waterBodyMember" ) );

    final QName qProfileMember = new QName( "org.kalypso.nofdpidss.1dmodel", "profileMember" );

    for( final Object objWaterbody : waterbodies )
    {
      if( !(objWaterbody instanceof Feature) )
        continue;

      final Feature waterbody = (Feature) objWaterbody;
      final List< ? > profiles = (List< ? >) waterbody.getProperty( qProfileMember );

      for( final Object objProfile : profiles )
      {
        if( !(objProfile instanceof Feature) )
          continue;

        final Feature profile = (Feature) objProfile;
        final GM_Object geometry = profile.getDefaultGeometryProperty();

        if( geometry instanceof GM_Curve )
          m_profiles.put( profile, (GM_Curve) geometry );
      }
    }
  }

  private void discoverBranchGeometries( final ISobekModelMember model )
  {
    final IBranch[] branches = model.getBranchMembers();
    for( final IBranch branch : branches )
      m_curves.put( branch, branch.getGeometryProperty() );
  }

  public IBranch getLastSnappedBranch( )
  {
    return m_lastSnappedBranch;
  }

  public Feature getLastSnappedCrossSection( )
  {
    return m_lastSnappedCrossSection;
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

      final Set<Entry<IBranch, GM_Curve>> entrySet = m_curves.entrySet();

      for( final Entry<IBranch, GM_Curve> entry : entrySet )
      {
        final GM_Point pSnap = MapUtilities.snap( panel, entry.getValue(), p, FNSnapPainterCreateProfileNode.RADIUS, SNAP_TYPE.SNAP_AUTO );
        if( pSnap != null )
        {
          m_lastSnappedBranch = entry.getKey();

          return pSnap;
        }
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
      final Set<Entry<IBranch, GM_Curve>> branches = m_curves.entrySet();

      GM_Point pBranchSnap = null;

      for( final Entry<IBranch, GM_Curve> branch : branches )
      {
        pBranchSnap = MapUtilities.snap( panel, branch.getValue(), currentPoint, FNSnapPainterCreateProfileNode.RADIUS, SNAP_TYPE.SNAP_TO_LINE );
        /* points snaps on branch? */
        if( pBranchSnap != null )
          break;
      }

      if( pBranchSnap == null )
        return null;

      final Set<Entry<Feature, GM_Curve>> profiles = m_profiles.entrySet();

      for( final Entry<Feature, GM_Curve> profile : profiles )
      {
        final GM_Point pSnapProfile = MapUtilities.snap( panel, profile.getValue(), currentPoint, FNSnapPainterCreateProfileNode.RADIUS, SNAP_TYPE.SNAP_TO_LINE );
        if( pSnapProfile != null )
        {
          m_lastSnappedCrossSection = profile.getKey();

          final Point point = MapUtilities.retransform( panel, pBranchSnap );
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
