/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.grid;

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.net.URL;

import junit.framework.Assert;

import org.deegree.crs.transformations.CRSTransformation;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.commons.math.LinearEquation;
import org.kalypso.commons.math.LinearEquation.SameXValuesException;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.transformation.CachedTransformationFactory;
import org.kalypso.transformation.TransformUtilities;
import org.kalypsodeegree.model.coverage.GridRange;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Ring;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree_impl.gml.binding.commons.CoverageCollection;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridCoverage;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain.OffsetVector;
import org.kalypsodeegree_impl.model.cv.GridRange_Impl;
import org.kalypsodeegree_impl.model.geometry.GM_Ring_Impl;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Point;

/**
 * Helper class for {@link IGeoGrid}s.
 * 
 * @author Gernot Belger
 */
public class GeoGridUtilities
{
  private GeoGridUtilities( )
  {
    throw new UnsupportedOperationException( "Helper class, do not instantiate." );
  }

  /**
   * Calclates the geo-position of the given cell.
   * 
   * @param c
   *            If c is null, a new coordinate is returned, else its values are changed.
   */
  public static Coordinate toCoordinate( final IGeoGrid grid, final int x, final int y, final Coordinate c ) throws GeoGridException
  {
    final Coordinate origin = grid.getOrigin();

    // final double rasterSize = raster.getRasterSize();
    final Coordinate offsetX = grid.getOffsetX();
    final Coordinate offsetY = grid.getOffsetY();

    final double cx = origin.x + x * offsetX.x + y * offsetY.x;
    final double cy = origin.y + x * offsetX.y + y * offsetY.y;

    if( c == null )
      return new Coordinate( cx, cy );

    c.x = cx;
    c.y = cy;
    return c;
  }

  /**
   * Calculates the cell within a {@link IGeoGrid} from a geo position.
   */
  public static GeoGridCell cellFromPosition( final IGeoGrid raster, final Coordinate pos ) throws GeoGridException
  {
    final Coordinate origin = raster.getOrigin();
    final Coordinate offsetX = raster.getOffsetX();
    final Coordinate offsetY = raster.getOffsetY();

    final double dx = pos.x - origin.x;
    final double dy = pos.y - origin.y;

    final double det = offsetX.x * offsetY.y - offsetY.x * offsetX.y;
    final double cellx = (dx * offsetY.y - dy * offsetX.y) / det;
    final double celly = (dy * offsetX.x - dx * offsetY.x) / det;

    return new GeoGridCell( (int) Math.floor( cellx ), (int) Math.floor( celly ) );
  }

  /**
   * Returns the origin cell of a grid.
   */
  public static GeoGridCell originAsCell( @SuppressWarnings("unused")
  final IGeoGrid grid )
  {
    return new GeoGridCell( 0, 0 );
  }

  /**
   * Returns the maximum cell (i.e. the cell with the maximum index) of a grid.
   */
  public static GeoGridCell maxCell( final IGeoGrid grid ) throws GeoGridException
  {
    return new GeoGridCell( grid.getSizeX(), grid.getSizeY() );
  }

  /**
   * Calculates the envelope of one cell.
   */
  public static Envelope asEnvelope( final IGeoGrid grid, final int i, final int j ) throws GeoGridException
  {
    final Coordinate origin = grid.getOrigin();
    final Coordinate offsetX = grid.getOffsetX();
    final Coordinate offsetY = grid.getOffsetY();
    final double x1 = origin.x + offsetX.x * i;
    final double y1 = origin.y + offsetY.y * j;

    final double x2 = x1 + offsetX.x;
    final double y2 = y1 + offsetY.y;

    return new Envelope( x1, x2, y1, y2 );
  }

  /**
   * Creates a {@link IGeoGrid} for a resource of a given mime-type.
   */
  public static IGeoGrid createGrid( final String mimeType, final URL url, final Coordinate origin, final Coordinate offsetX, final Coordinate offsetY, String sourceCRS ) throws IOException
  {
    // HACK: internal binary grid
    if( mimeType.endsWith( "/bin" ) )
      return BinaryGeoGrid.openGrid( url, origin, offsetX, offsetY, sourceCRS );

    if( mimeType.endsWith( "/asc" ) || mimeType.endsWith( "/asg" ) )
      return new AsciiRandomAccessGeoGrid( url, origin, offsetX, offsetY, sourceCRS );

    if( mimeType.startsWith( "image" ) )
      return new ImageGeoGrid( url, origin, offsetX, offsetY, sourceCRS );

    throw new UnsupportedOperationException( "Unknown file type: " + mimeType );
  }

  /**
   * Computes the bounding box of a grid.
   */
  public static Envelope toEnvelope( final IGeoGrid grid ) throws GeoGridException
  {
    final Coordinate origin = grid.getOrigin();
    final Coordinate offsetX = grid.getOffsetX();
    final Coordinate offsetY = grid.getOffsetY();

    final double x1 = origin.x;
    final double y1 = origin.y;

    final double x2 = x1 + offsetX.x * grid.getSizeX();
    final double y2 = y1 + offsetY.y * grid.getSizeY();

    return new Envelope( x1, x2, y1, y2 );
  }

  /**
   * This function creates the surface of a grid.
   * 
   * @param grid
   *            The grid.
   * @param targetCRS
   *            The coordinate system will be used to transform the surface, after it was created and before it is
   *            returned.
   * @return The surface of the given grid.
   */
  public static GM_Surface< ? > createSurface( IGeoGrid grid, final String targetCRS ) throws GeoGridException
  {
    try
    {
      final Coordinate origin = grid.getOrigin();
      final Coordinate offsetX = grid.getOffsetX();
      final Coordinate offsetY = grid.getOffsetY();

      final double x1 = origin.x;
      final double y1 = origin.y;

      final double x2 = x1 + offsetX.x * grid.getSizeX();
      final double y2 = y1 + offsetY.y * grid.getSizeY();

      /* Create the coordinates for the outer ring. */
      GM_Position c1 = GeometryFactory.createGM_Position( x1, y1 );
      GM_Position c2 = GeometryFactory.createGM_Position( x2, y1 );
      GM_Position c3 = GeometryFactory.createGM_Position( x2, y2 );
      GM_Position c4 = GeometryFactory.createGM_Position( x1, y2 );

      /* Create the outer ring. */
      GM_Ring_Impl shell = GeometryFactory.createGM_Ring( new GM_Position[] { c1, c2, c3, c4, c1 }, grid.getSourceCRS() );

      /* Create the surface patch. */
      GM_SurfacePatch patch = GeometryFactory.createGM_SurfacePatch( shell, new GM_Ring[] {}, grid.getSourceCRS() );

      /* Create the surface. */
      GM_Surface<GM_SurfacePatch> surface = GeometryFactory.createGM_Surface( patch );

      /* Transform it. */
      Assert.assertNotNull( "The target coordinate system is not allowed to be null ...", targetCRS );

      if( grid.getSourceCRS() != null && (!grid.getSourceCRS().equals( targetCRS )) )
        return (GM_Surface< ? >) surface.transform( CachedTransformationFactory.getInstance().createFromCoordinateSystems( grid.getSourceCRS(), targetCRS ), targetCRS );

      return surface;
    }
    catch( Exception ex )
    {
      throw new GeoGridException( "Error in creating the surface ...", ex );
    }
  }

  /**
   * This function creates the cell at the given (cell-)coordinates in a grid.
   * 
   * @param grid
   *            The grid.
   * @param x
   *            The (cell-)coordinate x.
   * @param y
   *            The (cell-)coordinate y.
   * @param targetCRS
   *            The coordinate system will be used to transform the cell, after it was created and before it is
   *            returned.
   * @return The cell at the given (cell-)coordinates in the grid.
   */
  public static GM_Surface< ? > createCell( IGeoGrid grid, int x, int y, String targetCRS ) throws GeoGridException
  {
    try
    {
      Coordinate cellCoordinate = GeoGridUtilities.toCoordinate( grid, x, y, null );

      double cellX1 = cellCoordinate.x;
      double cellY1 = cellCoordinate.y;

      double offsetX = grid.getOffsetX().x;
      double offsetY = grid.getOffsetY().y;

      double cellX2 = cellX1 + offsetX;
      double cellY2 = cellY1 + offsetY;

      /* Create the coordinates for the outer ring. */
      GM_Position c1 = GeometryFactory.createGM_Position( cellX1, cellY1 );
      GM_Position c2 = GeometryFactory.createGM_Position( cellX2, cellY1 );
      GM_Position c3 = GeometryFactory.createGM_Position( cellX2, cellY2 );
      GM_Position c4 = GeometryFactory.createGM_Position( cellX1, cellY2 );

      /* Create the outer ring. */
      GM_Ring_Impl shell = GeometryFactory.createGM_Ring( new GM_Position[] { c1, c2, c3, c4, c1 }, grid.getSourceCRS() );

      /* Create the surface patch. */
      GM_SurfacePatch patch = GeometryFactory.createGM_SurfacePatch( shell, new GM_Ring[] {}, grid.getSourceCRS() );

      /* Create the surface. */
      GM_Surface<GM_SurfacePatch> surface = GeometryFactory.createGM_Surface( patch );

      /* Transform it. */
      Assert.assertNotNull( "The target coordinate system is not allowed to be null ...", targetCRS );

      if( grid.getSourceCRS() != null && (!grid.getSourceCRS().equals( targetCRS )) )
        return (GM_Surface< ? >) surface.transform( CachedTransformationFactory.getInstance().createFromCoordinateSystems( grid.getSourceCRS(), targetCRS ), targetCRS );

      return surface;

    }
    catch( Exception ex )
    {
      throw new GeoGridException( "Error in creating the cell ...", ex );
    }
  }

  /**
   * Converts a gml-coverage to a {@link IGeoGrid}.<br>
   * After use, the grid has to be disposed.
   */
  public static IGeoGrid toGrid( final ICoverage coverage ) throws Exception
  {
    // REMARK: at the moment, only RectifiedGridCoverages are supported
    return new RectifiedGridCoverageGeoGrid( coverage.getFeature() );
  }

  /**
   * Applies a {@link IGeoGridWalker} to all members of a {@link ICoverageCollection}.<br>
   * Calls {@link IGeoGridWalker#start(IGeoGrid)} for every visited grid. <br>
   * ATTENTION: this does not work for every walker implementation!
   */
  public static void walkCoverages( final ICoverageCollection coverages, final IGeoGridWalker walker, final IProgressMonitor monitor ) throws Exception
  {
    monitor.beginTask( "Visiting coverages", coverages.size() );

    try
    {
      for( final ICoverage coverage : coverages )
      {
        final IGeoGrid grid = GeoGridUtilities.toGrid( coverage );

        grid.getWalkingStrategy().walk( grid, walker, monitor );

        ProgressUtilities.worked( monitor, 1 );
      }
    }
    finally
    {
      monitor.done();
    }
  }

  public static IWriteableGeoGrid createWriteableGrid( final String mimeType, final File file, final int sizeX, final int sizeY, final int scale, final Coordinate origin, final Coordinate offsetX, final Coordinate offsetY, final String sourceCRS ) throws IOException
  {
    // HACK: internal binary grid
    if( mimeType.endsWith( "/bin" ) )
      return BinaryGeoGrid.createGrid( file, sizeX, sizeY, scale, origin, offsetX, offsetY, sourceCRS );

    throw new UnsupportedOperationException( "Mime-Type not supported for writing: " + mimeType );
  }

  /**
   * Reads values from the given {@link IGeoGrid} and write it out into a new file which is then added as a new coverage
   * to the outputCoverages.
   * 
   * @param coverages
   *            The new coverage will be added to this collection
   * @param grid
   *            The values of the new coverage will be read from this grid.
   * @param file
   *            The new coverage will be serialized to this file.
   * @param filePath
   *            the (maybe relative) url to the file. This path will be put into the gml as address of the underlying
   *            file.
   * @param mimeType
   *            The mime type of the created underlying file.
   * @throws GeoGridException
   *             If the access to the given grid fails.
   * @throws IOException
   *             If writing to the output file fails.
   * @throws CoreException
   *             If the monitor is canceled.
   */
  public static ICoverage addCoverage( final ICoverageCollection coverages, final IGeoGrid grid, final File file, final String filePath, final String mimeType, final IProgressMonitor monitor ) throws Exception
  {
    final SubMonitor progress = SubMonitor.convert( monitor, 100 );

    /* Create new grid file and copy all values */
    final int scale = 2;
    IWriteableGeoGrid outputGrid = null;
    try
    {
      outputGrid = createWriteableGrid( mimeType, file, grid.getSizeX(), grid.getSizeY(), scale, grid.getOrigin(), grid.getOffsetX(), grid.getOffsetY(), grid.getSourceCRS() );

      ProgressUtilities.worked( monitor, 20 );

      final CopyGeoGridWalker walker = new CopyGeoGridWalker( outputGrid );

      grid.getWalkingStrategy().walk( grid, walker, progress.newChild( 70 ) );

      final BigDecimal min = walker.getMin();
      final BigDecimal max = walker.getMax();

      if( min != null && max != null )
        outputGrid.setStatistically( min, max );

      outputGrid.dispose();

      /* create new coverage and fill domain/range */
      final ICoverage coverage = CoverageCollection.addCoverage( coverages, toGridDomain( grid ), filePath, mimeType );

      ProgressUtilities.worked( progress, 10 );

      return coverage;
    }
    finally
    {
      if( outputGrid != null )
        outputGrid.dispose();

      progress.done();
    }
  }

  /**
   * Reads values from the given {@link IGeoGrid} and write it out into a new file which is referenced by given coverage
   * 
   * @param coverage
   *            The coverage that refers the grid
   * @param grid
   *            The values of the new coverage will be read from this grid.
   * @param file
   *            The new coverage will be serialized to this file.
   * @param filePath
   *            the (maybe relative) url to the file. This path will be put into the gml as address of the underlying
   *            file.
   * @param mimeType
   *            The mime type of the created underlying file.
   * @throws GeoGridException
   *             If the acces to the given grid fails.
   * @throws IOException
   *             If writing to the output file fails.
   * @throws CoreException
   *             If the monitor is cancelled.
   */
  public static void setCoverage( final RectifiedGridCoverage coverage, final IGeoGrid grid, final File file, final String filePath, final String mimeType, final IProgressMonitor monitor ) throws Exception
  {
    final SubMonitor progress = SubMonitor.convert( monitor, "Coverage wird erzeugt", 100 );

    final int scale = 2;
    IWriteableGeoGrid outputGrid = null;
    try
    {
      outputGrid = createWriteableGrid( mimeType, file, grid.getSizeX(), grid.getSizeY(), scale, grid.getOrigin(), grid.getOffsetX(), grid.getOffsetY(), grid.getSourceCRS() );
      ProgressUtilities.worked( monitor, 20 );
      final IGeoGridWalker walker = new CopyGeoGridWalker( outputGrid );
      grid.getWalkingStrategy().walk( grid, walker, progress.newChild( 70 ) );
      outputGrid.dispose();
      CoverageCollection.setCoverage( coverage, toGridDomain( grid ), filePath, mimeType );
      ProgressUtilities.worked( progress, 10 );
    }
    finally
    {
      if( outputGrid != null )
        outputGrid.dispose();
      progress.done();
    }
  }

  private static RectifiedGridDomain toGridDomain( final IGeoGrid grid ) throws Exception
  {
    final Point jtsOrigin = JTSAdapter.jtsFactory.createPoint( grid.getOrigin() );
    final GM_Point gmOrigin = (GM_Point) JTSAdapter.wrap( jtsOrigin );

    final Coordinate jtsOffsetX = grid.getOffsetX();
    final Coordinate jtsOffsetY = grid.getOffsetY();
    final OffsetVector offsetX = new RectifiedGridDomain.OffsetVector( jtsOffsetX.x, jtsOffsetX.y );
    final OffsetVector offsetY = new RectifiedGridDomain.OffsetVector( jtsOffsetY.x, jtsOffsetY.y );

    final double[] lows = new double[] { 0, 0 };
    final double[] highs = new double[] { grid.getSizeX(), grid.getSizeY() };

    final GridRange gridRange = new GridRange_Impl( lows, highs );

    return new RectifiedGridDomain( gmOrigin, offsetX, offsetY, gridRange );
  }

  /**
   * @return Midpoint of Rasterposition x,y and sets its value to the corresponding cell value.
   */
  public static final Coordinate calcCoordinate( final IGeoGrid grid, final int x, final int y, final Coordinate c ) throws GeoGridException
  {
    final Coordinate coordinate = GeoGridUtilities.toCoordinate( grid, x, y, c );

    final double value = grid.getValueChecked( x, y );
    coordinate.z = value;
    return coordinate;
  }

  /**
   * @return Midpoint of raster position x,y and sets its value to the given cell value.
   */
  public static final Coordinate calcCoordinateWithoutZ( final IGeoGrid grid, final int x, final int y, final double z, final Coordinate c ) throws GeoGridException
  {
    final Coordinate coordinate = GeoGridUtilities.toCoordinate( grid, x, y, c );
    coordinate.z = z;

    return coordinate;
  }

  public enum Interpolation
  {
    none, // no interpolation: grid values are of constant value in a grid cell with middle-point the given coordinate
    bilinear, // bilinear interpolation
    bicubic; // bilinear interpolation
  }

  public static double getValue( final IGeoGrid grid, final Coordinate crd, final Interpolation interpolation ) throws GeoGridException
  {
    switch( interpolation )
    {
      case none:
        final GeoGridCell cell = GeoGridUtilities.cellFromPosition( grid, crd );
        return grid.getValueChecked( cell.x, cell.y );

      case bilinear:
        return interpolateBilinear( grid, crd );

      default:
        throw new UnsupportedOperationException( "Unsupported interpolation method: " + interpolation );
    }
  }

  private static double interpolateBilinear( final IGeoGrid grid, final Coordinate crd ) throws GeoGridException
  {
    // Find four adjacent cells
    final GeoGridCell c11 = cellFromPosition( grid, crd );
    final GeoGridCell c12 = new GeoGridCell( c11.x + 1, c11.y );
    final GeoGridCell c21 = new GeoGridCell( c11.x, c11.y + 1 );
    final GeoGridCell c22 = new GeoGridCell( c11.x + 1, c11.y + 1 );

    final double v11 = grid.getValueChecked( c11.x, c11.y );
    final double v12 = grid.getValueChecked( c12.x, c12.y );
    final double v21 = grid.getValueChecked( c21.x, c21.y );
    final double v22 = grid.getValueChecked( c22.x, c22.y );

    final Coordinate crd11 = toCoordinate( grid, c11 );
    final Coordinate crd12 = toCoordinate( grid, c12 );
    final Coordinate crd21 = toCoordinate( grid, c21 );
    final Coordinate crd22 = toCoordinate( grid, c22 );

    try
    {
      // interpolate in x direction
      final LinearEquation lex1 = new LinearEquation( crd11.x, v11, crd12.x, v12 );
      final LinearEquation lex2 = new LinearEquation( crd21.x, v21, crd22.x, v22 );

      final double vx1 = lex1.computeY( crd.x );
      final double vx2 = lex2.computeY( crd.x );

      // interpolate in y direction
      final LinearEquation ley = new LinearEquation( crd11.y, vx1, crd22.y, vx2 );
      return ley.computeY( crd.y );
    }
    catch( final SameXValuesException e )
    {
      // should never happen...
      e.printStackTrace();

      return Double.NaN;
    }
  }

  private static Coordinate toCoordinate( final IGeoGrid grid, final GeoGridCell cell ) throws GeoGridException
  {
    return toCoordinate( grid, cell.x, cell.y, null );
  }

  /**
   * gets the min and max values for the given {@link ICoverageCollection}
   */
  public static BigDecimal[] getMinMax( ICoverageCollection covCollection ) throws Exception
  {
    BigDecimal[] minmax = new BigDecimal[2];

    BigDecimal minValue = new BigDecimal( Double.MAX_VALUE ).setScale( 4, BigDecimal.ROUND_HALF_UP );
    BigDecimal maxValue = new BigDecimal( Double.MIN_VALUE ).setScale( 4, BigDecimal.ROUND_HALF_UP );

    for( ICoverage coverage : covCollection )
    {
      IGeoGrid grid = GeoGridUtilities.toGrid( coverage );

      BigDecimal min = grid.getMin();
      BigDecimal max = grid.getMax();

      minValue = minValue.min( min );
      maxValue = maxValue.max( max );
    }

    minmax[0] = minValue;
    minmax[1] = maxValue;

    return minmax;
  }

  /**
   * This function transforms the coordinate crd from its coordinate system to the grid coordinate system.
   * 
   * @param grid
   *            The grid.
   * @param crd
   *            The coordinate.
   * @param positionCRS
   *            The coordinate system of the position.
   * @return The transformed coordinate.
   */
  public static Coordinate transformCoordinate( final IGeoGrid grid, final Coordinate crd, final String positionCRS ) throws GeoGridException
  {
    try
    {
      CRSTransformation transformation = CachedTransformationFactory.getInstance().createFromCoordinateSystems( positionCRS, grid.getSourceCRS() );
      GM_Position position = TransformUtilities.transform( JTSAdapter.wrap( crd ), transformation );

      return JTSAdapter.export( position );
    }
    catch( Exception ex )
    {
      throw new GeoGridException( "Could not transform the coordinate ...", ex );
    }
  }
}