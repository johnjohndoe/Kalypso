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
package org.kalypso.model.wspm.tuhh.core.simulation.steadyuniform;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.Assert;
import org.kalypso.jts.JTSUtilities;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineSegment;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Wrapper around a {@link org.kalypso.model.wspm.core.profil.IProfil} that calculates parameters for the steady-uniform
 * runoff calculation. <br/>
 * The parameters are calculated for a channel (i.e. part of the cross section), typically for left and rigfht foreland
 * and the main channel. <br/>
 * TODO: water level always covers the whole cross section, regardless if the water can reach the part of the section or
 * not.
 *
 * @author Gernot Belger
 */
public class SteadyUniformChannel
{
  private final GeometryFactory m_factory = new GeometryFactory();

  private final IProfile m_crossSection;

  private final int m_channelStart;

  private final int m_channelEnd;

  public SteadyUniformChannel( final IProfile crossSection, final int channelStart, final int channelEnd )
  {
    Assert.isTrue( channelStart < channelEnd );

    Assert.isTrue( channelStart > 0 );
    Assert.isTrue( channelEnd > 0 );

    Assert.isTrue( channelStart < crossSection.getResult().size() );
    Assert.isTrue( channelEnd < crossSection.getResult().size() );

    m_crossSection = crossSection;
    m_channelStart = channelStart;
    m_channelEnd = channelEnd;
  }

  /** Calculate the area of the channel for a given water level. */
  public double calculateArea( final double waterLevel )
  {
    final Coordinate[] line = extentChannelCoordinates( waterLevel );

    final Polygon halfPlaneUpwards = buildHalfplane( line, waterLevel );

    final Polygon crossSectionArea = createCrossSectionArea( line );

    final Geometry difference = crossSectionArea.difference( halfPlaneUpwards );
    return difference.getArea();
  }

  private Polygon createCrossSectionArea( final Coordinate[] line )
  {
    /* Build closed ring from line */
    final Coordinate[] closedRing = new Coordinate[line.length + 1];
    System.arraycopy( line, 0, closedRing, 0, line.length );
    closedRing[line.length] = line[0];

    final LinearRing crossSectionShell = m_factory.createLinearRing( closedRing );
    return m_factory.createPolygon( crossSectionShell, null );
  }

  public double calculateWetRadius( final double waterLevel )
  {
    final Coordinate[] line = extentChannelCoordinates( waterLevel );

    final Polygon halfPlaneUpwards = buildHalfplane( line, waterLevel );

    final LineString crossSectionLine = m_factory.createLineString( line );

    // TODO: check: if waterlevel was higher than start or end point, the wet radius contains a part of the virtual
    // vertical wall added to start and/or end of the cross section.
    // This might be ok for 'trennfl‰chen' but might not be ok for the model boundary

    final Geometry difference = crossSectionLine.difference( halfPlaneUpwards );
    return difference.getLength();
  }

  public double calculateHydraulicRadius( final double waterLevel )
  {
    final double U_h = calculateWetRadius( waterLevel );
    final double A_h = calculateArea( waterLevel );

    return A_h / U_h;
  }

  /**
   * Extracts the profile line of this channel as jts {@link Coordinate}s.
   */
  private Coordinate[] getChannelCoordinates( )
  {
    final Collection<Coordinate> crds = new ArrayList<>( m_channelEnd - m_channelStart + 1 );

    final TupleResult result = m_crossSection.getResult();

    final int widhtComponent = result.indexOfComponent( IWspmConstants.POINT_PROPERTY_BREITE );
    final int heightComponent = result.indexOfComponent( IWspmConstants.POINT_PROPERTY_HOEHE );

    for( int i = m_channelStart; i < m_channelEnd + 1; i++ )
    {
      final IRecord point = result.get( i );

      final BigDecimal width = (BigDecimal) point.getValue( widhtComponent );
      final BigDecimal height = (BigDecimal) point.getValue( heightComponent );

      crds.add( new Coordinate( width.doubleValue(), height.doubleValue() ) );
    }

    return crds.toArray( new Coordinate[crds.size()] );
  }

  public LineString toLine( )
  {
    return m_factory.createLineString( getChannelCoordinates() );
  }

  private Coordinate[] extentChannelCoordinates( final double waterLevel )
  {
    /* Get the soil */
    final Coordinate[] line = getChannelCoordinates();

    final Coordinate startPoint = line[0];
    final Coordinate endPoint = line[line.length - 1];

    /* Make sure, soil is higher than water level: build vertical wall at first and/or last point */
    if( startPoint.y > waterLevel && endPoint.y > waterLevel )
      return line;

    final double diffStart = waterLevel - startPoint.y;
    final double diffEnd = waterLevel - endPoint.y;

    final double difference = Math.max( diffStart, diffEnd );
    /* Add a little bit, so we are really a bit higher than the water level */
    final double delta = difference + 1.0;

    final List<Coordinate> extendedLine = new LinkedList<>();

    /* vertical wall @ start */
    if( startPoint.y > waterLevel )
      extendedLine.add( new Coordinate( startPoint.x, startPoint.y + delta ) );

    /* all normal points */
    extendedLine.addAll( Arrays.asList( line ) );

    /* vertical wall @ end */
    if( endPoint.y > waterLevel )
      extendedLine.add( new Coordinate( startPoint.x, endPoint.y + delta ) );

    return extendedLine.toArray( new Coordinate[extendedLine.size()] );
  }

  /**
   * Build a polygon with bottom line the waterlevel, left and right and top at least covering the complete profile
   * line.
   */
  private Polygon buildHalfplane( final Coordinate[] line, final double waterLevel )
  {
    final LineString helperLine = m_factory.createLineString( line );
    final Envelope boundingBox = helperLine.getEnvelopeInternal();

    final double maxY = boundingBox.getMaxY() + 1.0;
    final double minX = boundingBox.getMinX() - 1.0;
    final double maxX = boundingBox.getMaxX() + 1.0;
    final double minY = waterLevel;

    final Envelope halfPlane = new Envelope( minX, maxX, minY, maxY );

    return JTSUtilities.convertEnvelopeToPolygon( halfPlane, m_factory );
  }

  // TODO: for now a very simple average, as this is not correct anyways... (see DWK or similar how to build average
  // hydraulic radius) (i.e. the weight should be the corresponding wet-area, not the length of the soil-section
  public double getAverageKST( final double waterLevel )
  {
    final TupleResult result = m_crossSection.getResult();

    final int widhtComponent = result.indexOfComponent( IWspmConstants.POINT_PROPERTY_BREITE );
    final int heightComponent = result.indexOfComponent( IWspmConstants.POINT_PROPERTY_HOEHE );
    final int roughnessComponent = result.indexOfComponent( IWspmConstants.POINT_PROPERTY_RAUHEIT_KST );

    double totalLength = 0.0;
    double totalWeightedRoughness = 0.0;

    for( int i = m_channelStart; i < m_channelEnd; i++ )
    {
      final IRecord point1 = result.get( i );
      final IRecord point2 = result.get( i + 1 );

      final double roughness = ((BigDecimal) point1.getValue( roughnessComponent )).doubleValue();

      final double width1 = ((BigDecimal) point1.getValue( widhtComponent )).doubleValue();
      final double height1 = ((BigDecimal) point1.getValue( heightComponent )).doubleValue();

      final double width2 = ((BigDecimal) point2.getValue( widhtComponent )).doubleValue();
      final double height2 = ((BigDecimal) point2.getValue( heightComponent )).doubleValue();

      if( height1 < waterLevel && height2 < waterLevel )
      {
        final double length = new LineSegment( width1, height1, width2, height2 ).getLength();

        final double weightedRoughness = length * roughness;

        totalWeightedRoughness += weightedRoughness;
        totalLength += length;
      }
    }

    return totalWeightedRoughness / totalLength;
  }
}