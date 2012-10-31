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
import org.kalypso.commons.java.lang.Doubles;
import org.kalypso.jts.JTSUtilities;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.ChannelEditData.SIDE;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.ChannelEditUtil;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.util.WspmProfileHelper;
import org.kalypso.transformation.transformer.GeoTransformerException;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.linearref.LengthIndexedLine;

/**
 * Intersects profiles with banklines and creates the segmented bank line.
 * 
 * @author Gernot Belger
 */
class BanklineIntersector
{
  private final Map<GM_Curve, SIDE> m_banks;

  private final SIDE m_side;

  private final BankData m_bankData;

  private final int m_numberOfBankPoints;

  private final ISegmentData m_segment;

  public BanklineIntersector( final ISegmentData segment, final Map<GM_Curve, SIDE> banks, final SIDE side, final int numberOfBankPoints )
  {
    m_segment = segment;
    m_banks = banks;
    m_side = side;
    m_numberOfBankPoints = numberOfBankPoints;

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

      return intersectBankline( downProfile, upProfile );
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
      return null;
    }
  }

  private BankData intersectBankline( final IProfileData downProfile, final IProfileData upProfile ) throws GM_Exception
  {
    final LineString upProfileLine = convertProfilesToLineStrings( upProfile );
    final LineString downProfileLine = convertProfilesToLineStrings( downProfile );

    final Pair<LineString, Pair<Point, Point>> intersection = findBankForProfiles( downProfileLine, upProfileLine );

    if( intersection == null )
      return null;

    final Pair<Point, Point> points = intersection.getValue();

    final Point downPoint = points.getLeft();
    final Point upPoint = points.getRight();

    final LineString bankLine = intersection.getKey();

    /* extract line between start end end intersection */
    final LineString croppedBankLine = (LineString)JTSUtilities.extractLineString( bankLine, downPoint, upPoint );

    /* handle z values of start a<nd end point */
    final double downZ = findIntersectionZ( downProfile, croppedBankLine.getStartPoint(), bankLine );
    final double upZ = findIntersectionZ( upProfile, croppedBankLine.getEndPoint(), bankLine );

    /* cropped line has z if bankLine has z; addionally, we want to force the profile height onto the endpoints */
    final LineString croppedBankLineWithZEndPoints = replaceEndpointZ( croppedBankLine, downZ, upZ );

    /* now make sure, we have z everywhere */
    final LineString croppedBankLineWithZ = JTSUtilities.interpolateMissingZ( croppedBankLineWithZEndPoints );

    final LineString segmentedGeometry = ChannelEditUtil.intersectLineString( croppedBankLineWithZ, m_numberOfBankPoints );

    return new BankData( m_segment, bankLine, croppedBankLine, segmentedGeometry, false );
  }

  /**
   * converts a WSPM profile into an linestring
   * 
   * @param profile
   *          Input profile to be converted.
   */
  private static LineString convertProfilesToLineStrings( final IProfileData upProfile ) throws GM_Exception
  {
    final GM_Curve profCurve = upProfile.getOriginalProfileGeometry();
    return (LineString)JTSAdapter.export( profCurve );
  }

  private LineString replaceEndpointZ( final LineString bankLine, final double startZ, final double endZ )
  {
    final Point startPoint = bankLine.getStartPoint();
    final Point endPoint = bankLine.getEndPoint();

    final Coordinate[] coordinates = bankLine.getCoordinates();
    final Coordinate[] newCoordinates = new Coordinate[coordinates.length];

    newCoordinates[0] = new Coordinate( startPoint.getX(), startPoint.getY(), startZ );
    System.arraycopy( coordinates, 1, newCoordinates, 1, coordinates.length - 2 );
    newCoordinates[coordinates.length - 1] = new Coordinate( endPoint.getX(), endPoint.getY(), endZ );

    return bankLine.getFactory().createLineString( newCoordinates );
  }

  // TODO: fetch heights from profile at this points
  // - either from bankline, if bankline has z
  // -or from profile
  private double findIntersectionZ( final IProfileData profileData, final Point intersection, final LineString bankLine )
  {
    try
    {
      /* original height from where ??? */
      final double intersectionZ = intersection.getCoordinate().z;

      if( !Double.isNaN( intersectionZ ) )
        return intersectionZ;

      final IProfile profilOrg = profileData.getOriginalProfile();

      final LengthIndexedLine bankIndex = new LengthIndexedLine( bankLine );
      final double indexOnBankLine = bankIndex.project( intersection.getCoordinate() );
      final Coordinate bankPoint = bankIndex.extractPoint( indexOnBankLine );

      final Double width = WspmProfileHelper.getWidthPosition( intersection, profilOrg );
      if( Doubles.isNaN( width ) )
        return Double.NaN;

      final Double profileHeight = WspmProfileHelper.getHeightByWidth( width, profilOrg );

      // TODO: if both abk point and profile have height's: compute difference and show if too big

      return profileHeight;
    }
    catch( GM_Exception | GeoTransformerException e )
    {
      e.printStackTrace();
      return Double.NaN;
    }
  }

  /**
   * Find a bank line that intersect both profiles in exactly one point.
   */
  private Pair<LineString, Pair<Point, Point>> findBankForProfiles( final LineString downProfile, final LineString upProfile )
  {
    for( final Entry<GM_Curve, SIDE> bankEntry : m_banks.entrySet() )
    {
      if( m_side == bankEntry.getValue() )
      {
        try
        {
          /* convert current bankLine in Curve */
          final GM_Curve bankCurve = bankEntry.getKey();
          final LineString bankLine = (LineString)JTSAdapter.export( bankCurve );

          if( bankLine != null )
          {
            final Geometry upIntersection = bankLine.intersection( upProfile );
            final Geometry downIntersection = bankLine.intersection( downProfile );

            /* intersect it with the first (previous) profile of this segment */
            if( upIntersection instanceof Point && downIntersection instanceof Point )
            {
              final Pair<Point, Point> points = Pair.of( (Point)upIntersection, (Point)downIntersection );
              return Pair.of( bankLine, points );
            }
          }
        }
        catch( final GM_Exception e )
        {
          // TODO: error handling?

          e.printStackTrace();
        }
      }
    }

    return null;
  }

  public BankData getBankline( )
  {
    return m_bankData;
  }
}