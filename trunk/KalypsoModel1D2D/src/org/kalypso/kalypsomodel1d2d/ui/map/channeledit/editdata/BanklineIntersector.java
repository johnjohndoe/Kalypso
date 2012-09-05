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
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit.editdata;

import java.util.Map;
import java.util.Map.Entry;

import org.apache.commons.lang3.tuple.Pair;
import org.kalypso.jts.JTSUtilities;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.ChannelEditUtil;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateChannelData.SIDE;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;

/**
 * @author Gernot Belger
 */
class BanklineIntersector
{
  private final Map<GM_Curve, SIDE> m_banks;

  private final SIDE m_side;

  private final BankData m_bankData;

  private final int m_numberOfBankSegments;

  private final ISegmentData m_segment;

  public BanklineIntersector( final ISegmentData segment, final Map<GM_Curve, SIDE> banks, final SIDE side, final int numberOfBankSegments )
  {
    m_segment = segment;
    m_banks = banks;
    m_side = side;
    m_numberOfBankSegments = numberOfBankSegments;

    m_bankData = calculateBanklines();
  }

  /**
   * Intersects the selected bank lines with the two profiles of the segment. This method will be called at the very
   * beginning. Here, the four intersection points will be initialized.
   */
  private BankData calculateBanklines( )
  {
    try
    {
      final IProfileData downProfile = m_segment.getProfileDown();
      final IProfileData upProfile = m_segment.getProfileUp();

      final LineString upProfileLine = ChannelEditUtil.convertProfilesToLineStrings( upProfile.getFeature() );
      final LineString downProfileLine = ChannelEditUtil.convertProfilesToLineStrings( downProfile.getFeature() );

      return intersectBankline( upProfileLine, downProfileLine );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return null;
    }
  }

  /**
   * Find a bank line that intersect both profiles in exactly one point.
   */
  private Pair<LineString, Pair<Point, Point>> findBankForProfiles( final LineString upProfile, final LineString downProfile )
  {
    for( final Entry<GM_Curve, SIDE> bankEntry : m_banks.entrySet() )
    {
      if( m_side == bankEntry.getValue() )
      {
        try
        {
          /* convert current bankLine in Curve */
          final GM_Curve bankCurve = bankEntry.getKey();
          final LineString bankLine = (LineString) JTSAdapter.export( bankCurve );

          final Geometry upIntersection = bankLine.intersection( upProfile );
          final Geometry downIntersection = bankLine.intersection( downProfile );

          /* intersect it with the first (previous) profile of this segment */
          if( upIntersection instanceof Point && downIntersection instanceof Point )
          {
            final Pair<Point, Point> points = Pair.of( (Point) upIntersection, (Point) downIntersection );
            return Pair.of( bankLine, points );
          }
        }
        catch( final GM_Exception e )
        {
          e.printStackTrace();
        }
      }
    }

    return null;
  }

  private BankData intersectBankline( final LineString upProfileLine, final LineString downProfileLine ) throws Exception
  {
    final Pair<LineString, Pair<Point, Point>> intersection = findBankForProfiles( upProfileLine, downProfileLine );

    if( intersection == null )
      return null;

    final Pair<Point, Point> points = intersection.getValue();

    final Point upPoint = points.getKey();
    final Point downPoint = points.getValue();

    final LineString bankLine = intersection.getKey();

    final LineString croppedBankLine = JTSUtilities.createLineString( bankLine, upPoint, downPoint );

    final LineString segmentedGeometry = ChannelEditUtil.intersectLineString( croppedBankLine, m_numberOfBankSegments );

    return new BankData( m_segment, bankLine, croppedBankLine, segmentedGeometry, false );
  }

  public BankData getBankline( )
  {
    return m_bankData;
  }
}