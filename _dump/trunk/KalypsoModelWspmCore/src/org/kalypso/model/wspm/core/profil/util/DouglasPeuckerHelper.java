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
package org.kalypso.model.wspm.core.profil.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.changes.PointRemove;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

/**
 * Helper for thinning a profile with the Douglas Peucker algorithm.
 * 
 * @author Holger Albert
 * @author Thomas Jung
 */
public class DouglasPeuckerHelper
{
  private static final class ProfileSegmentData
  {
    public final IRecord[] segmPoints;

    public final int startInd;

    public final int endInd;

    public final double distance;

    public int distInd;

    public ProfileSegmentData( final IRecord[] points, final int start, final int end )
    {
      segmPoints = points;
      startInd = start;
      endInd = end;
      distance = maxSegmentDistance();
    }

    private final double maxSegmentDistance( )
    {
      double maxDistance = Double.NEGATIVE_INFINITY;
      final int deltaIndex = endInd - startInd;
      if( deltaIndex > 2 )
        for( int i = 1; i < endInd - startInd - 1; i++ )
        {
          final double currentDistance = calcDistance( segmPoints[startInd], segmPoints[endInd], segmPoints[startInd + i] );
          if( currentDistance > maxDistance )
          {
            maxDistance = currentDistance;
            distInd = startInd + i;
          }
        }
      else if( deltaIndex == 2 )
      {
        maxDistance = calcDistance( segmPoints[startInd], segmPoints[endInd], segmPoints[startInd + 1] );
        distInd = startInd + 1;
      }
      else if( deltaIndex == 1 )
      {
        maxDistance = 0;
        distInd = startInd;
      }

      return maxDistance;
    }
  }

  /**
   * The constructor.
   */
  private DouglasPeuckerHelper( )
  {
  }

  /**
   * This function starts the creation of the operation, which removes points from the profile. It uses the Douglas
   * Peucker algorythm, for finding the point to remove.
   * 
   * @param allowedDistance
   *            The allowed distance [m].
   * @param points
   *            The profile points.
   * @param profil
   *            The profile.
   * @return The profile changes.
   */
  public static IProfilChange[] reduce( double allowedDistance, IRecord[] points, IProfil profile )
  {
    /* Reduce points. */
    IRecord[] pointsToKeep = profile.getMarkedPoints();
    IRecord[] pointsToRemove = reducePoints( points, pointsToKeep, allowedDistance );
    if( pointsToRemove.length == 0 )
      return new IProfilChange[] {};

    /* Remove points. */
    IProfilChange[] removeChanges = new IProfilChange[pointsToRemove.length];
    for( int i = 0; i < pointsToRemove.length; i++ )
      removeChanges[i] = new PointRemove( profile, pointsToRemove[i] );

    return removeChanges;
  }

  /**
   * This function finds all points, which must be removed.
   * 
   * @param points
   *            All profile points.
   * @param pointsToKeep
   *            All points, which are important and should be kept.
   * @param allowedDistance
   *            The allowed distance.
   * @return The points to remove.
   */
  public static IRecord[] reducePoints( IRecord[] points, IRecord[] pointsToKeep, double allowedDistance )
  {
    /* Reduce segment wise. */
    Set<IRecord> pointsToKeepList = new HashSet<IRecord>( Arrays.asList( pointsToKeep ) );
    List<IRecord> pointsToRemove = new ArrayList<IRecord>( points.length - 2 );

    int segmentBegin = 0;
    for( int i = 0; i < points.length; i++ )
    {
      if( i == segmentBegin )
        continue;

      IRecord point = points[i];
      if( pointsToKeepList.contains( point ) || i == points.length - 1 )
      {
        IRecord[] toRemove = reduceIt( points, segmentBegin, i, allowedDistance );
        pointsToRemove.addAll( Arrays.asList( toRemove ) );
        segmentBegin = i;
      }
    }

    return pointsToRemove.toArray( new IRecord[pointsToRemove.size()] );
  }

  /** @return the points with are redundant */
  private static IRecord[] reduceIt( IRecord[] points, int begin, int end, double allowedDistance )
  {
    if( end - begin < 2 )
      return new IRecord[0];

    // für alle punkte abstand zu segment[begin-end] ausrechnen
    double[] distances = new double[end - (begin + 1)];
    double maxdistance = 0.0;
    int maxdistIndex = -1;
    for( int i = 0; i < distances.length; i++ )
    {
      double distance = calcDistance( points[begin], points[end], points[i + begin + 1] );
      distances[i] = distance;

      if( distance > maxdistance )
      {
        maxdistance = distance;
        maxdistIndex = i + begin + 1;
      }
    }

    // falls ein punkt dabei, dessen diff > maxdiff, splitten
    if( maxdistance > allowedDistance && maxdistIndex != -1 )
    {
      IRecord[] beginReduced = reduceIt( points, begin, maxdistIndex, allowedDistance );
      IRecord[] endReduced = reduceIt( points, maxdistIndex, end, allowedDistance );

      List<IRecord> reduced = new ArrayList<IRecord>( beginReduced.length + endReduced.length );

      reduced.addAll( Arrays.asList( beginReduced ) );
      reduced.addAll( Arrays.asList( endReduced ) );
      return reduced.toArray( new IRecord[reduced.size()] );
    }

    // kein Punkt mehr wichtig: alle zwischenpunkte zurückgeben
    IRecord[] reduced = new IRecord[end - (begin + 1)];
    for( int i = 0; i < reduced.length; i++ )
      reduced[i] = points[i + begin + 1];

    return reduced;
  }

  protected static double calcDistance( IRecord beginPoint, IRecord endPoint, IRecord middlePoint )
  {

    final IComponent breiteComp = ProfilObsHelper.getPropertyFromId( beginPoint, IWspmConstants.POINT_PROPERTY_BREITE );
    final IComponent hoeheComp = ProfilObsHelper.getPropertyFromId( beginPoint, IWspmConstants.POINT_PROPERTY_HOEHE );

    final TupleResult ownerBegin = beginPoint.getOwner();
    final int breiteIndexBegin = ownerBegin.indexOfComponent( breiteComp );
    final int hoeheIndexBegin = ownerBegin.indexOfComponent( hoeheComp );

    final TupleResult ownerMiddle = middlePoint.getOwner();
    final int breiteIndexMiddle = ownerMiddle.indexOfComponent( breiteComp );
    final int hoeheIndexMiddle = ownerMiddle.indexOfComponent( hoeheComp );

    final TupleResult ownerEnd = endPoint.getOwner();
    final int breiteIndexEnd = ownerEnd.indexOfComponent( breiteComp );
    final int hoeheIndexEnd = ownerEnd.indexOfComponent( hoeheComp );

    double bx = (Double) beginPoint.getValue( breiteIndexBegin );
    double by = (Double) beginPoint.getValue( hoeheIndexBegin );
    double ex = (Double) endPoint.getValue( breiteIndexMiddle );
    double ey = (Double) endPoint.getValue( hoeheIndexMiddle );
    double mx = (Double) middlePoint.getValue( breiteIndexEnd );
    double my = (Double) middlePoint.getValue( hoeheIndexEnd );

    double f = (ey - by) / (ex - bx);

    double distance = (f * mx - 1 * my - f * bx + by) / Math.sqrt( 1 + f * f );
    return Math.abs( distance );
  }

  /**
   * gets the most important profile points by sequentially adding the points with the maximum distance to the segment
   * initially defined by the start and end point of the profile.
   * 
   * @param points
   *            all profile points
   * @param allowedNumPoints
   *            max number of points.
   * @return points to keep
   */
  public static IRecord[] findIProfileVIPPoints( final IRecord[] points, final int allowedNumPoints )
  {
    final List<IRecord> pointsToKeep = new ArrayList<IRecord>( allowedNumPoints - 1 );

    // store the first point of the input profile in the profile point list.
    pointsToKeep.add( points[0] );

    final LinkedList<ProfileSegmentData> profSegmentList = new LinkedList<ProfileSegmentData>();

    /* begin with the start and end point of the profile */
    final ProfileSegmentData startSegment = new ProfileSegmentData( points, 0, points.length - 1 );
    profSegmentList.add( startSegment );

    for( int i = 1; i < allowedNumPoints - 1; i++ )
    {
      double maxDist = Double.NEGATIVE_INFINITY;
      int indexMax = 0;

      for( int j = 0; j < profSegmentList.size(); j++ )
      {
        // find the maxDistanceSegment

        final ProfileSegmentData currentProfSegment = profSegmentList.get( j );
        final double currentDist = currentProfSegment.distance;
        if( currentDist > maxDist )
        {
          maxDist = currentDist;
          indexMax = j;
        }
      }
      // store the found maximum in the profile point list
      pointsToKeep.add( points[profSegmentList.get( indexMax ).distInd] );

      // split the maxDistanceSegment
      final ProfileSegmentData firstSplittedSegment = new ProfileSegmentData( points, profSegmentList.get( indexMax ).startInd, profSegmentList.get( indexMax ).distInd );
      final ProfileSegmentData secondSplittedSegment = new ProfileSegmentData( points, profSegmentList.get( indexMax ).distInd, profSegmentList.get( indexMax ).endInd );

      // store the new segments in the list
      profSegmentList.set( indexMax, firstSplittedSegment );
      profSegmentList.add( indexMax + 1, secondSplittedSegment );
    }
    pointsToKeep.add( points[points.length - 1] );
    return pointsToKeep.toArray( new IRecord[pointsToKeep.size()] );
  }

}