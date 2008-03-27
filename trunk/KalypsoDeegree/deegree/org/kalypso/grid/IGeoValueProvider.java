package org.kalypso.grid;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Common interface to access double-values from a georeferences position.
 * 
 * @author Gernot Belger
 */
public interface IGeoValueProvider
{
  /**
   * This function should return a value of a grid cell, which lies at the given coordinate.
   * 
   * @param crd
   *            The coordinate must be in the same coordinate system as this grid.
   * @return The value of the cell, if it exists.
   */
  public double getValue( final Coordinate crd ) throws GeoGridException;
}