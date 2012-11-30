/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Bj�rnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universit�t Hamburg-Harburg, Institut f�r Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.ewawi.utils.profiles;

import java.math.BigDecimal;

import org.apache.commons.math3.geometry.euclidean.twod.Vector2D;
import org.kalypso.model.wspm.ewawi.data.EwawiProLine;
import org.kalypso.model.wspm.ewawi.data.EwawiStaLine;
import org.kalypso.shape.geometry.SHPPoint;

/**
 * A EWAWI+ profile point.
 * 
 * @author Holger Albert
 */
public class EwawiProfilePoint
{
  /**
   * The left fix point.
   */
  private final EwawiStaLine m_left;

  /**
   * The right fix point.
   */
  private final EwawiStaLine m_right;

  /**
   * The profile point.
   */
  private final EwawiProLine m_proLine;

  /**
   * The constructor.
   * 
   * @param left
   *          The left fix point.
   * @param right
   *          The right fix point.
   * @param proLine
   *          The profile point.
   */
  public EwawiProfilePoint( final EwawiStaLine left, final EwawiStaLine right, final EwawiProLine proLine )
  {
    m_left = left;
    m_right = right;
    m_proLine = proLine;
  }

  public Short getProfilNummer( )
  {
    return m_left.getProfilNummer();
  }

  public BigDecimal getBreite( )
  {
    return m_proLine.getRechtswert();
  }

  public BigDecimal getHoehe( )
  {
    return m_left.getHoehe().add( m_proLine.getHoehe() );
  }

  /**
   * This function returns the point as shape.
   * 
   * @return The point as shape.
   */
  public SHPPoint getShape( )
  {
    /* g Rechtswert (Projektion auf Abszisse) von links nach rechts aufsteigend */
    /* h Hochwert (Abweichung von der Achse) rechts positiv, links negativer Wert. */
    final double rechtswert = m_proLine.getRechtswert().doubleValue();
    final double hochwert = m_proLine.getHochwert().doubleValue();

    /* Get the normalized vector of the fix points (length 1). */
    final Vector2D normalizedProfileAxis = getNormalizedProfileAxis();

    /* Get the projection vector. */
    final Vector2D projectionVector = normalizedProfileAxis.scalarMultiply( rechtswert );

    /* Create the start point. */
    final double xLeft = m_left.getRechtswert().doubleValue();
    final double yLeft = m_left.getHochwert().doubleValue();
    final Vector2D startPoint = new Vector2D( xLeft, yLeft );

    /* Create the projection point. */
    final Vector2D projectionPoint = startPoint.add( projectionVector );

    /* Create the orthogonal vector from the normalized profile axis (has then also length 1). */
    final Vector2D orthVector = new Vector2D( normalizedProfileAxis.getY(), -normalizedProfileAxis.getX() );

    /* Add hochwert * orthVector to the projection point. */
    final Vector2D targetPoint = projectionPoint.add( hochwert, orthVector );

    return new SHPPoint( targetPoint.getX(), targetPoint.getY() );
  }

  /**
   * This function returns the point as shape.
   * It ignores the hochwert, so that the point will be the projected point on the idealized line (left fixpoint to right fixpoint).
   * 
   * @return The point as shape.
   */
  public SHPPoint getShapeIdealized( )
  {
    /* g Rechtswert (Projektion auf Abszisse) von links nach rechts aufsteigend */
    final double rechtswert = m_proLine.getRechtswert().doubleValue();

    /* Get the normalized vector of the fix points (length 1). */
    final Vector2D normalizedProfileAxis = getNormalizedProfileAxis();

    /* Get the projection vector. */
    final Vector2D projectionVector = normalizedProfileAxis.scalarMultiply( rechtswert );

    /* Create the start point. */
    final double xLeft = m_left.getRechtswert().doubleValue();
    final double yLeft = m_left.getHochwert().doubleValue();
    final Vector2D startPoint = new Vector2D( xLeft, yLeft );

    /* Create the projection point. */
    final Vector2D projectionPoint = startPoint.add( projectionVector );

    return new SHPPoint( projectionPoint.getX(), projectionPoint.getY() );
  }

  private Vector2D getNormalizedProfileAxis( )
  {
    final double xLeft = m_left.getRechtswert().doubleValue();
    final double yLeft = m_left.getHochwert().doubleValue();

    final double xRight = m_right.getRechtswert().doubleValue();
    final double yRight = m_right.getHochwert().doubleValue();

    final Vector2D profileAxis = new Vector2D( xRight - xLeft, yRight - yLeft );

    return profileAxis.normalize();
  }
}