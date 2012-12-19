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

import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.Collection;

import org.kalypso.commons.math.geom.PolyLine;
import org.kalypso.model.wspm.core.profil.IProfile;

import com.vividsolutions.jts.geom.Envelope;

/**
 * Calculates the steady uniform runoff for a given cross section.
 *
 * @author Gernot Belger
 */
public class SteadyUniformCalculation
{
  private final IProfile m_crossSection;

  private final double m_slope;

  public SteadyUniformCalculation( final IProfile crossSection, final double slope )
  {
    m_crossSection = crossSection;
    m_slope = slope;
  }

  public double calculateRunoff( final double waterLevel )
  {
    // TODO: for now, we do not consider any channels

    final SteadyUniformChannel channel = new SteadyUniformChannel( m_crossSection, 0, m_crossSection.getResult().size() - 1 );

    final double area = channel.calculateArea( waterLevel );
    final double hydraulicRadius = channel.calculateHydraulicRadius( waterLevel );

    // TODO: kst - average for this water level; i.e. average for all wet parts of the channel; probably not good!
    final double kst = channel.getAverageKST( waterLevel );

    return SteadyUniformFormulas.calculateManningRunoff( kst, hydraulicRadius, m_slope, area );
  }

  /**
   * Calculates the runoff for the complete possible water level range of the cross section.
   * 
   * @return A {@link PolyLine} representing the rating curve. x values are water levels; y values is runoff.
   */
  public PolyLine calculateRatingCurve( )
  {
    /* Determine min/max height of river bottom */
    final SteadyUniformChannel channel = new SteadyUniformChannel( m_crossSection, 0, m_crossSection.getResult().size() - 1 );
    final Envelope channelBox = channel.toLine().getEnvelopeInternal();

    final double minGround = channelBox.getMinY();
    final double maxGround = channelBox.getMaxX();

    /* Step in 1cm steps */
    final double step = 0.01;

    final Collection<Point2D> points = new ArrayList<>();

    for( double waterLevel = minGround; waterLevel < maxGround; waterLevel += step )
    {
      final double runoff = calculateRunoff( waterLevel );
      points.add( new Point2D.Double( waterLevel, runoff ) );
    }

    return new PolyLine( points.toArray( new Point2D[points.size()] ), 0.001 );
  }
}