package org.kalypso.grid;

import java.math.BigDecimal;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;

/**
 * Implementations of this interface represent georeferences grids.
 * <p>
 * Each cell of the grid has a georeferences position as well as a double-value.
 * </p>
 * 
 * @author Gernot Belger
 */
public interface IGeoGrid extends IGeoValueProvider
{
  /** Call this after this grid is nor more used. Disposes of any use resources. */
  public void dispose( );

  /** Size of the raster. Amount of raster-cells in direction of x. */
  public int getSizeX( ) throws GeoGridException;

  /** Size of the raster. Amount of raster-cells in direction of y. */
  public int getSizeY( ) throws GeoGridException;

  /**
   * Returns the value of the raster at (cell-)coordinates x-y. Does not check, if x or y lie within the raster bounds.
   * <p>
   * The behaviour is undefined, if x-y lie out of bounds. (May throw an exception or return random numbers).
   * </p>
   */
  public double getValue( final int x, final int y ) throws GeoGridException;

  /**
   * Simliar to {@link #getValue(int, int)} but checks if the given cell coordinates lie in the raster. If not, return
   * {@link Double#NaN}.
   */
  public double getValueChecked( final int x, final int y ) throws GeoGridException;

  /** The bounding box of this grid */
  public Envelope getBoundingBox( ) throws GeoGridException;

  /**
   * The origin point of this grid
   */
  public Coordinate getOrigin( ) throws GeoGridException;

  /** Offset from the origin in direction of x. Add (as vector) to origin to to walk along the raster alon the x-Axis. */
  public Coordinate getOffsetX( ) throws GeoGridException;

  /** Offset from the origin in direction of y. Add (as vector) to origin to to walk along the raster alon the y-Axis. */
  public Coordinate getOffsetY( ) throws GeoGridException;

  /**
   * Returns a walking strategy suitable to iterate thorugh this grid.
   */
  public IGeoWalkingStrategy getWalkingStrategy( ) throws GeoGridException;

  public BigDecimal getMin( ) throws GeoGridException;

  public BigDecimal getMax( ) throws GeoGridException;
}