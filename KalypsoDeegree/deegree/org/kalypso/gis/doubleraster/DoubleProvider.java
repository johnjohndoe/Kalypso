package org.kalypso.gis.doubleraster;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Gibt zu einer Geo-Coordinate ein Double zurück.
 * 
 * @author belger
 */
public interface DoubleProvider
{
  public double getDouble( final Coordinate crd );
}
