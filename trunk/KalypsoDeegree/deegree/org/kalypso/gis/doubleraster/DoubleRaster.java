package org.kalypso.gis.doubleraster;

import org.eclipse.core.runtime.IProgressMonitor;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * @author belger
 */
public interface DoubleRaster
{
  /**
   * Returns the value of the raster at (cell-)coordinates x-y. Does not check, if x or y lie within the raster bounds.
   * <p>
   * The behaviour is undefined, if x-y lie out of bounds. (May throw an exception or return random numbers).
   * </p>
   */
  public double getValue( final int x, final int y );

  /**
   * Simliar to {@link #getValue(int, int)} but checks if the given cell coordinates lie in the raster. If not, return
   * {@link Double#NaN}.
   */
  public double getValueChecked( final int x, final int y );

  public Coordinate getOrigin( );

  /** Offset from the origin in direction of x. Add (as vector) to origin to to walk along the raster alon the x-Axis. */
  public Coordinate getOffsetX( );

  /** Offset from the origin in direction of y. Add (as vector) to origin to to walk along the raster alon the y-Axis. */
  public Coordinate getOffsetY( );

  /** Size of the raster. Amount of raster-cells in direction of x. */
  public int getSizeX( );

  /** Size of the raster. Amount of raster-cells in direction of y. */
  public int getSizeY( );

  public Object walk( final DoubleRasterWalker pwo, final IProgressMonitor monitor ) throws DoubleRasterException;
}
