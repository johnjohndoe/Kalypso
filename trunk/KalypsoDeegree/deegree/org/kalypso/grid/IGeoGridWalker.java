package org.kalypso.grid;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Interface to iterate through all cells of a {@link IDoubleGeoGrid}.
 * 
 * @author Gernot Belger
 */
public interface IGeoGridWalker
{
  /**
   * Wird vor der Iteration durch die Punkte aufgerufen
   */
  void start( final IGeoGrid r ) throws GeoGridException;

  /**
   * Wird f�r jeden Punkt des Rasters aufgerufen
   * 
   * @param c
   *            Hier wird die Geocoordinate des Punkt �bergeben. Vorsicht: das Objekt wird wiederverwendet und wird
   *            daher sp�ter ver�ndert
   */
  void operate( final int x, final int y, final Coordinate c ) throws GeoGridException;

  /**
   * Will be called at the end of the walk.
   * <p>
   * Implementations can return some result here.
   * </p>
   * 
   * @return The result of this operation Gibt das Ergebnis der Operation zur�ck
   */
  Object finish( ) throws GeoGridException;
}
