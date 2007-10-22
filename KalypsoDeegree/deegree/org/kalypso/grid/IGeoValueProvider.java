package org.kalypso.grid;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Common interface to access double-values from a georeferences position.
 * 
 * @author Gernot Belger
 */
public interface IGeoValueProvider
{
  public double getValue( final Coordinate crd ) throws GeoGridException;
}
