package com.bce.eind.core.profil.util.math;

import java.awt.geom.Point2D;


/**
 *
 * @author schlienger
 */
public final class Utilities
{
  /**
   * Computes the distance between two points using Thales Theorem.
   *
   * @param p1
   * @param p2
   *
   * @return
   */
  public static double distance( Point2D p1, Point2D p2 )
  {
    double dx = Math.abs( p1.getX(  ) - p2.getX(  ) );
    double dy = Math.abs( p1.getY(  ) - p2.getY(  ) );

    return Math.sqrt( dx * dx + dy * dy );
  }
}
