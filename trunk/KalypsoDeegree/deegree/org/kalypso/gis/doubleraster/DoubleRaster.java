package org.kalypso.gis.doubleraster;

import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.gis.doubleraster.grid.DoubleGrid;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * @author belger
 */
public interface DoubleRaster extends DoubleGrid
{

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

  public Object walk( final DoubleRasterWalker pwo, final IProgressMonitor monitor ) throws DoubleRasterException;
}
