package org.kalypso.gis.doubleraster;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Interface to Read points from Grid
 * 
 * @author belger
 */
public interface DoubleRasterWalker
{
  /**
   * Wird vor der Iteration durch die Punkte aufgerufen
   */
  void start( final DoubleRaster r ) throws DoubleRasterException;

  /**
   * Wird f�r jeden Punkt des Rasters aufgerufen
   * 
   * @param c
   *          Hier wird die Geocoordinate des Punkt �bergeben. Vorsicht: das Objekt wird wiederverwendet und wird daher
   *          sp�ter ver�ndert
   */
  void operate( final int x, final int y, final Coordinate c ) throws DoubleRasterException;

  /**
   * Wird nach jeder Zeile des Rasters aufgerufen
   */
  void afterLine( final int y ) throws DoubleRasterException;

  /**
   * Gibt das Ergebnis der Operation zur�ck
   */
  Object getResult( ) throws DoubleRasterException;
}
