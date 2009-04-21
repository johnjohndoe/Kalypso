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
package org.kalypso.model.wspm.tuhh.core.profile.importer.hw;

import java.io.File;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.math.geom.PolyLine;
import org.kalypso.jts.JTSUtilities;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateSequence;
import com.vividsolutions.jts.geom.impl.CoordinateArraySequenceFactory;

/**
 * @author belger
 */
public class BridgeResult extends HeightWidthResult
{
  private final Collection<Coordinate> m_upperCrds;

  private final Collection<Coordinate> m_lowerCrds;

  public BridgeResult( final String parentName, final String dataName, final String id, final String name, final Collection<Coordinate> lowerCrds, final Collection<Coordinate> upperCrds, final File tempDir )
  {
    super( parentName, dataName, id, name, tempDir );
    m_upperCrds = upperCrds;
    m_lowerCrds = lowerCrds;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.importer.hw.HeightWidthResult#buildPolygon()
   */
  @Override
  protected List<Coordinate> buildPolygon( )
  {
    final Coordinate[] lowerCrdArray = m_lowerCrds.toArray( new Coordinate[m_lowerCrds.size()] );
    final CoordinateSequence lowerSeq = CoordinateArraySequenceFactory.instance().create( lowerCrdArray );
    final List<Coordinate> bridgeCrdList = new LinkedList<Coordinate>( m_lowerCrds );
    final Coordinate[] upperCrdArray = m_upperCrds.toArray( new Coordinate[m_upperCrds.size()] );
    final CoordinateSequence upperSeq = CoordinateArraySequenceFactory.instance().create( upperCrdArray );

    /* Build ring from lower/upper coordinates */
    cropLower( bridgeCrdList, upperSeq );

    liftLower( lowerSeq, upperCrdArray );

    // Add upper in reverse order
    for( int i = upperCrdArray.length; i > 0; i-- )
    {
      final Coordinate upperCrd = upperCrdArray[i - 1];
      bridgeCrdList.add( upperCrd );
    }

    // Remove duplicates
    Coordinate lastCrd = null;
    for( final Iterator<Coordinate> iterator = bridgeCrdList.iterator(); iterator.hasNext(); )
    {
      final Coordinate coordinate = iterator.next();
      if( coordinate.equals( lastCrd ) )
        iterator.remove();
      else
        lastCrd = coordinate;
    }

    return bridgeCrdList;
  }

  /**
   * Crop lower by upper
   */
  private void cropLower( final List<Coordinate> bridgeCrdList, final CoordinateSequence upperSeq )
  {
    final double upperLeftX;
    final double upperRightX;

    /* First: check if we have identical points somewhere */
    final Coordinate upperLeft = upperSeq.getCoordinate( 0 );
    final Coordinate upperRight = upperSeq.getCoordinate( upperSeq.size() - 1 );

    final int leftIndex = crdIndexOfWithEps( bridgeCrdList, upperLeft );
    if( leftIndex == -1 )
    {
      // We will crop everything before that x
      upperLeftX = upperLeft.x;
      addStatus( IStatus.WARNING, String.format( "Unterkante start-point did not match river-bed: %s", upperRight ), null );
    }
    else
    {
      upperLeftX = Double.NEGATIVE_INFINITY; // prohibits crop by x value
      // remove all before that point
      for( int i = 0; i < leftIndex + 1; i++ )
        bridgeCrdList.remove( 0 );
    }

    final int rightIndex = crdIndexOfWithEps( bridgeCrdList, upperRight );
    if( rightIndex == -1 )
    {
      // We will crop everything before that x
      upperRightX = upperRight.x;
      addStatus( IStatus.WARNING, String.format( "Unterkante end-point did not match river-bed: %s", upperRight ), null );
    }
    else
    {
      upperRightX = Double.POSITIVE_INFINITY; // prohibits crop by x value
      // remove all before that point
      while( bridgeCrdList.size() > rightIndex )
        bridgeCrdList.remove( rightIndex );
    }

    for( final Iterator<Coordinate> iterator = bridgeCrdList.iterator(); iterator.hasNext(); )
    {
      final Coordinate coordinate = iterator.next();
      if( coordinate.x < upperLeftX || coordinate.x > upperRightX )
        iterator.remove();
    }
  }

  private int crdIndexOfWithEps( final List<Coordinate> crdList, final Coordinate crd )
  {
    final double EPS = 0.10;

    int bestIndex = -1;
    double bestDisttance = Double.POSITIVE_INFINITY;
    for( int i = 0; i < crdList.size(); i++ )
    {
      final Coordinate coordinate = crdList.get( i );
      final double dist = coordinate.distance( crd );
      if( dist < EPS && dist < bestDisttance )
      {
        bestIndex = i;
        bestDisttance = dist;
      }
    }

    return bestIndex;
  }

  /**
   * Lift upper above lower <br>
   * REMARK: only works if the lower-coordinates form a function (no 'jumping-back')
   */
  private void liftLower( final CoordinateSequence lowerSeq, final Coordinate[] upperCrdArray )
  {
    final double[] lowerXs = JTSUtilities.getXValues( lowerSeq );
    final double[] lowerYs = JTSUtilities.getYValues( lowerSeq );
    final double EPS = 0.01;
    final PolyLine polyLine = new PolyLine( lowerXs, lowerYs, EPS );

    for( final Coordinate upperCrd : upperCrdArray )
    {
      final double upperX = upperCrd.x;
      final double upperY = upperCrd.y;

      final double lowerY = polyLine.getYFor( upperX );
      final double diff = lowerY - upperY;
      if( diff > -EPS )
      {
        if( diff > 0.2 )
          addStatus( IStatus.ERROR, String.format( "Upper coordinate way below lower coorindate: %.2f [m]", Math.abs( diff + EPS ) ), null );
        else if( diff > EPS )
        {
          addStatus( IStatus.WARNING, String.format( "Small adaption of upper coordinate by %.3f [m]", Math.abs( diff + EPS ) ), null );

          // REMARK: we lift a bit above lower, so later we may get non self-intertsection rings
          upperCrd.y = lowerY + EPS;
        }
        else
        {
          addStatus( IStatus.INFO, String.format( "Tiny adaption of upper coordinate by: %.2f [m]", Math.abs( diff + EPS ) ), null );

          // REMARK: we lift a bit above lower, so later we may get non self-intertsection rings
          upperCrd.y = lowerY + EPS;
        }
      }
    }
  }

}
