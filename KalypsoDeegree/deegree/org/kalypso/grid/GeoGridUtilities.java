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

import ogc31.www.opengis.net.gml.FileType;
import ogc31.www.opengis.net.gml.FileValueModelType;
import ogc31.www.opengis.net.gml.RangeSetType;

import org.deegree.crs.transformations.coordinate.CRSTransformation;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.commons.math.LinearEquation;
import org.kalypso.commons.math.LinearEquation.SameXValuesException;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.contribs.ogc31.KalypsoOGC31JAXBcontext;
import org.kalypso.transformation.CachedTransformationFactory;
import org.kalypso.transformation.TransformUtilities;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.coverage.GridRange;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
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
import com.vividsolutions.jts.geom.Geometry;
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
   *          If c is null, a new coordinate is returned, else its values are changed.
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
   * Calculates the cell within a {@link IGeoGrid} from a geo position. We use a gird point as a center point
   * representation.
   */
  public static GeoGridCell cellFromPosition( final IGeoGrid raster, final Coordinate pos ) throws GeoGridException
  {
    // TODO Eventually transform pos always to the coordinate system of the grid.
    final Coordinate origin = raster.getOrigin();
    final Coordinate offsetX = raster.getOffsetX();
    final Coordinate offsetY = raster.getOffsetY();

    // fit to the center point grid representation
    final double dx = pos.x - (origin.x - offsetX.x / 2 - offsetY.x / 2);
    final double dy = pos.y - (origin.y - offsetX.y / 2 - offsetY.y / 2);

    final double det = offsetX.x * offsetY.y - offsetY.x * offsetX.y;
    final double cellx = (dx * offsetY.y - dy * offsetX.y) / det;
    final double celly = (dy * offsetX.x - dx * offsetY.x) / det;

    return new GeoGridCell( (int) Math.floor( cellx ), (int) Math.floor( celly ) );
  }

  /**
   * Returns the origin cell of a grid.
   */
  public static GeoGridCell originAsCell( @SuppressWarnings("unused") final IGeoGrid grid )
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
    final double x1 = origin.x + offsetX.x * (i - 1 / 2);
    final double y1 = origin.y + offsetY.y * (j - 1 / 2);

    final double x2 = x1 + offsetX.x;
    final double y2 = y1 + offsetY.y;

    return new Envelope( x1, x2, y1, y2 );
  }

  /**
   * Opens a {@link IGeoGrid} for a resource of a given mime-type.<br/> The grid is opened read-only.
   */
  public static IGeoGrid openGrid( final String mimeType, final URL url, final Coordinate origin, final Coordinate offsetX, final Coordinate offsetY, final String sourceCRS ) throws IOException
  {
    return openGrid( mimeType, url, origin, offsetX, offsetY, sourceCRS, false );
  }

  /**
   * Opens a {@link IGeoGrid} for a resource of a given mime-type.
   * 
   * @param writeable
   *          if <code>true</code>, the grid is opened for write-access. In that case a {@link IWriteableGeoGrid} will
   *          be returned.
   * @throws UnsupportedOperationException
   *           If a grid is opened for write access that does not support it.
   */
  public static IGeoGrid openGrid( final String mimeType, final URL url, final Coordinate origin, final Coordinate offsetX, final Coordinate offsetY, final String sourceCRS, final boolean writeable ) throws IOException
  {
    // HACK: internal binary grid
    if( mimeType.endsWith( "/bin" ) )
      return BinaryGeoGrid.openGrid( url, origin, offsetX, offsetY, sourceCRS, writeable );

    if( writeable )
      throw new UnsupportedOperationException( "Cannot open this grid for write access." );

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

    final double x2 = x1 + offsetX.x * (grid.getSizeX() - 1);
    final double y2 = y1 + offsetY.y * (grid.getSizeY() - 1);

    return new Envelope( x1, x2, y1, y2 );
  }

  /**
   * This function creates the surface of a grid.
   * 
   * @param grid
   *          The grid.
   * @param targetCRS
   *          The coordinate system will be used to transform the surface, after it was created and before it is
   *          returned.
   * @return The surface of the given grid.
   */
  public static GM_Surface< ? > createSurface( final IGeoGrid grid, final String targetCRS ) throws GeoGridException
  {
    try
    {
      final Coordinate origin = grid.getOrigin();
      final Coordinate offsetX = grid.getOffsetX();
      final Coordinate offsetY = grid.getOffsetY();

      final double x1 = origin.x - (offsetX.x + offsetY.x) / 2;
      final double y1 = origin.y - (offsetX.y + offsetY.y) / 2;

      final double x2 = x1 + offsetX.x * grid.getSizeX();
      final double y2 = y1 + offsetY.y * grid.getSizeY();

      /* Create the coordinates for the outer ring. */
      final GM_Position c1 = GeometryFactory.createGM_Position( x1, y1 );
      final GM_Position c2 = GeometryFactory.createGM_Position( x2, y1 );
      final GM_Position c3 = GeometryFactory.createGM_Position( x2, y2 );
      final GM_Position c4 = GeometryFactory.createGM_Position( x1, y2 );

      /* Create the outer ring. */
      final GM_Ring_Impl shell = GeometryFactory.createGM_Ring( new GM_Position[] { c1, c2, c3, c4, c1 }, grid.getSourceCRS() );

      /* Create the surface patch. */
      final GM_SurfacePatch patch = GeometryFactory.createGM_SurfacePatch( shell, new GM_Ring[] {}, grid.getSourceCRS() );

      /* Create the surface. */
      final GM_Surface<GM_SurfacePatch> surface = GeometryFactory.createGM_Surface( patch );

      /* Transform it. */
      Assert.isNotNull( "The target coordinate system is not allowed to be null ...", targetCRS );
      if( grid.getSourceCRS() != null && (!grid.getSourceCRS().equals( targetCRS )) )
        return (GM_Surface< ? >) surface.transform( CachedTransformationFactory.getInstance().createFromCoordinateSystems( grid.getSourceCRS(), targetCRS ), targetCRS );

      return surface;
    }
    catch( final Exception ex )
    {
      throw new GeoGridException( "Error in creating the surface ...", ex );
    }
  }

  /**
   * This function creates the cell at the given (cell-)coordinates in a grid. We interpret the grid cell as a surface
   * with the grid point as center point of the surface.
   * 
   * @param grid
   *          The grid.
   * @param x
   *          The (cell-)coordinate x.
   * @param y
   *          The (cell-)coordinate y.
   * @param targetCRS
   *          The coordinate system will be used to transform the cell, after it was created and before it is returned.
   * @return The cell at the given (cell-)coordinates in the grid.
   */
  public static GM_Surface< ? > createCell( final IGeoGrid grid, final int x, final int y, final String targetCRS ) throws GeoGridException
  {
    try
    {
      final Coordinate cellCoordinate = GeoGridUtilities.toCoordinate( grid, x, y, null );

      final double offsetX = grid.getOffsetX().x;
      final double offsetY = grid.getOffsetY().y;

      final double cellX1 = cellCoordinate.x - offsetX / 2;
      final double cellY1 = cellCoordinate.y - offsetY / 2;

      final double cellX2 = cellX1 + offsetX;
      final double cellY2 = cellY1 + offsetY;

      /* Create the coordinates for the outer ring. */
      final GM_Position c1 = GeometryFactory.createGM_Position( cellX1, cellY1 );
      final GM_Position c2 = GeometryFactory.createGM_Position( cellX2, cellY1 );
      final GM_Position c3 = GeometryFactory.createGM_Position( cellX2, cellY2 );
      final GM_Position c4 = GeometryFactory.createGM_Position( cellX1, cellY2 );

      /* Create the outer ring. */
      final GM_Ring_Impl shell = GeometryFactory.createGM_Ring( new GM_Position[] { c1, c2, c3, c4, c1 }, grid.getSourceCRS() );

      /* Create the surface patch. */
      final GM_SurfacePatch patch = GeometryFactory.createGM_SurfacePatch( shell, new GM_Ring[] {}, grid.getSourceCRS() );

      /* Create the surface. */
      final GM_Surface<GM_SurfacePatch> surface = GeometryFactory.createGM_Surface( patch );

      /* Transform it. */
      Assert.isNotNull( "The target coordinate system is not allowed to be null ...", targetCRS );
      if( grid.getSourceCRS() != null && (!grid.getSourceCRS().equals( targetCRS )) )
        return (GM_Surface< ? >) surface.transform( CachedTransformationFactory.getInstance().createFromCoordinateSystems( grid.getSourceCRS(), targetCRS ), targetCRS );

      return surface;

    }
    catch( final Exception ex )
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
   * Converts a gml-coverage to a {@link IWriteableGeoGrid}.<br>
   * After use, the grid has to be disposed.
   */
  public static IWriteableGeoGrid toWriteableGrid( final ICoverage coverage ) throws Exception
  {
    // REMARK: at the moment, only RectifiedGridCoverages are supported
    return new WriteableRectifiedGridCoverageGeoGrid( coverage.getFeature(), null );
  }

  /**
   * Applies a {@link IGeoGridWalker} to all members of a {@link ICoverageCollection}.<br>
   * Calls {@link IGeoGridWalker#start(IGeoGrid)} for every visited grid. <br>
   * ATTENTION: this does not work for every walker implementation! *
   */
  public static void walkCoverages( final ICoverageCollection coverages, final IGeoGridWalker walker, final IProgressMonitor monitor ) throws Exception
  {
    walkCoverages( coverages, walker, null, monitor );
  }

  /**
   * Applies a {@link IGeoGridWalker} to all members of a {@link ICoverageCollection} that lie inside a certain
   * geometry.<br>
   * Calls {@link IGeoGridWalker#start(IGeoGrid)} for every visited grid. <br>
   * ATTENTION: this does not work for every walker implementation! *
   * 
   * @param walkingArea
   *          If non-<code>null</code>, Only grid cells are visited that lie inside this geometry.
   */
  public static void walkCoverages( final ICoverageCollection coverages, final IGeoGridWalker walker, final Geometry walkingArea, final IProgressMonitor monitor ) throws Exception
  {
    monitor.beginTask( "Visiting coverages", coverages.size() );

    try
    {
      for( final ICoverage coverage : coverages )
      {
        final IGeoGrid grid = GeoGridUtilities.toGrid( coverage );

        grid.getWalkingStrategy().walk( grid, walker, walkingArea, monitor );
        ProgressUtilities.worked( monitor, 1 );
        grid.dispose();
      }
    }
    finally
    {
      monitor.done();
    }
  }

  public static IWriteableGeoGrid createWriteableGrid( final String mimeType, final File file, final int sizeX, final int sizeY, final int scale, final Coordinate origin, final Coordinate offsetX, final Coordinate offsetY, final String sourceCRS, final boolean fillGrid ) throws GeoGridException
  {
    // HACK: internal binary grid
    if( mimeType.endsWith( "/bin" ) )
      return BinaryGeoGrid.createGrid( file, sizeX, sizeY, scale, origin, offsetX, offsetY, sourceCRS, fillGrid );

    throw new UnsupportedOperationException( "Mime-Type not supported for writing: " + mimeType );
  }

  /**
   * Reads values from the given {@link IGeoGrid} and write it out into a new file which is then added as a new coverage
   * to the outputCoverages.
   * 
   * @param coverages
   *          The new coverage will be added to this collection
   * @param grid
   *          The values of the new coverage will be read from this grid.
   * @param file
   *          The new coverage will be serialized to this file.
   * @param filePath
   *          the (maybe relative) url to the file. This path will be put into the gml as address of the underlying
   *          file.
   * @param mimeType
   *          The mime type of the created underlying file.
   * @throws GeoGridException
   *           If the access to the given grid fails.
   * @throws IOException
   *           If writing to the output file fails.
   * @throws CoreException
   *           If the monitor is canceled.
   */
  public static ICoverage addCoverage( final ICoverageCollection coverages, final IGeoGrid grid, final File file, final String filePath, final String mimeType, final IProgressMonitor monitor ) throws Exception
  {
    final SubMonitor progress = SubMonitor.convert( monitor, 100 );

    /* Create new grid file and copy all values */
    final int scale = 2;
    IWriteableGeoGrid outputGrid = null;
    try
    {
      outputGrid = createWriteableGrid( mimeType, file, grid.getSizeX(), grid.getSizeY(), scale, grid.getOrigin(), grid.getOffsetX(), grid.getOffsetY(), grid.getSourceCRS(), false );

      ProgressUtilities.worked( monitor, 20 );

      final CopyGeoGridWalker walker = new CopyGeoGridWalker( outputGrid );

      grid.getWalkingStrategy().walk( grid, walker, null, progress.newChild( 70 ) );

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
   *          The coverage that refers the grid
   * @param grid
   *          The values of the new coverage will be read from this grid.
   * @param file
   *          The new coverage will be serialized to this file.
   * @param filePath
   *          the (maybe relative) url to the file. This path will be put into the gml as address of the underlying
   *          file.
   * @param mimeType
   *          The mime type of the created underlying file.
   * @throws GeoGridException
   *           If the acces to the given grid fails.
   * @throws IOException
   *           If writing to the output file fails.
   * @throws CoreException
   *           If the monitor is cancelled.
   */
  public static void setCoverage( final RectifiedGridCoverage coverage, final IGeoGrid grid, final File file, final String filePath, final String mimeType, final IProgressMonitor monitor ) throws Exception
  {
    final SubMonitor progress = SubMonitor.convert( monitor, "Coverage wird erzeugt", 100 );

    final int scale = 2;
    IWriteableGeoGrid outputGrid = null;
    try
    {
      outputGrid = createWriteableGrid( mimeType, file, grid.getSizeX(), grid.getSizeY(), scale, grid.getOrigin(), grid.getOffsetX(), grid.getOffsetY(), grid.getSourceCRS(), false );
      ProgressUtilities.worked( monitor, 20 );
      final IGeoGridWalker walker = new CopyGeoGridWalker( outputGrid );
      grid.getWalkingStrategy().walk( grid, walker, null, progress.newChild( 70 ) );
      outputGrid.dispose();
      setCoverage( coverage, toGridDomain( grid ), filePath, mimeType );
      ProgressUtilities.worked( progress, 10 );
    }
    finally
    {
      if( outputGrid != null )
        outputGrid.dispose();
      progress.done();
    }
  }

  private static void setCoverage( final RectifiedGridCoverage coverage, final RectifiedGridDomain domain, final String externalResource, final String mimeType )
  {
    final FileType rangeSetFile = KalypsoOGC31JAXBcontext.GML3_FAC.createFileType();

    // file name relative to the gml
    rangeSetFile.setFileName( externalResource );
    rangeSetFile.setMimeType( mimeType );
    rangeSetFile.setFileStructure( FileValueModelType.RECORD_INTERLEAVED );

    final RangeSetType rangeSet = KalypsoOGC31JAXBcontext.GML3_FAC.createRangeSetType();
    rangeSet.setFile( rangeSetFile );

    coverage.setDescription( "Imported via Kalypso" );
    coverage.setGridDomain( domain );
    coverage.setRangeSet( rangeSet );
  }

  private static RectifiedGridDomain toGridDomain( final IGeoGrid grid ) throws Exception
  {
    final Point jtsOrigin = JTSAdapter.jtsFactory.createPoint( grid.getOrigin() );
    final GM_Point gmOrigin = (GM_Point) JTSAdapter.wrap( jtsOrigin );
    gmOrigin.setCoordinateSystem( grid.getSourceCRS() );

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
    /* Find four adjacent cells */
    final GeoGridCell c11 = cellFromPosition( grid, crd );

    // check on which side of the cell we are
    final Coordinate crd11 = toCoordinate( grid, c11 );

    final double centerX = crd11.x;
    final double centerY = crd11.y;

    int cellShiftX = 0;
    int cellShiftY = 0;

    if( crd.x < centerX && crd.y > centerY )
    {
      cellShiftX = -1;
      cellShiftY = -1;
    }
    else if( crd.x < centerX && crd.y < centerY )
    {
      cellShiftX = -1;
      cellShiftY = 1;
    }
    else if( crd.x > centerX && crd.y > centerY )
    {
      cellShiftX = 1;
      cellShiftY = -1;
    }
    else if( crd.x > centerX && crd.y < centerY )
    {
      cellShiftX = 1;
      cellShiftY = 1;
    }

    final GeoGridCell c12 = new GeoGridCell( c11.x + cellShiftX, c11.y );
    final GeoGridCell c21 = new GeoGridCell( c11.x, c11.y + cellShiftY );
    final GeoGridCell c22 = new GeoGridCell( c11.x + cellShiftX, c11.y + cellShiftY );

    final double v11 = grid.getValueChecked( c11.x, c11.y );
    final double v12 = grid.getValueChecked( c12.x, c12.y );
    final double v21 = grid.getValueChecked( c21.x, c21.y );
    final double v22 = grid.getValueChecked( c22.x, c22.y );

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
  public static BigDecimal[] getMinMax( final ICoverageCollection covCollection ) throws Exception
  {
    final BigDecimal[] minmax = new BigDecimal[2];

    BigDecimal minValue = new BigDecimal( Double.MAX_VALUE ).setScale( 4, BigDecimal.ROUND_HALF_UP );
    BigDecimal maxValue = new BigDecimal( Double.MIN_VALUE ).setScale( 4, BigDecimal.ROUND_HALF_UP );

    for( final ICoverage coverage : covCollection )
    {
      final IGeoGrid grid = GeoGridUtilities.toGrid( coverage );

      final BigDecimal min = grid.getMin();
      final BigDecimal max = grid.getMax();

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
   *          The grid.
   * @param crd
   *          The coordinate.
   * @param positionCRS
   *          The coordinate system of the position.
   * @return The transformed coordinate.
   */
  public static Coordinate transformCoordinate( final IGeoGrid grid, final Coordinate crd, final String positionCRS ) throws GeoGridException
  {
    try
    {
      if( grid.getSourceCRS() == null || positionCRS == null )
        return crd;

      final CRSTransformation transformation = CachedTransformationFactory.getInstance().createFromCoordinateSystems( positionCRS, grid.getSourceCRS() );
      final GM_Position position = TransformUtilities.transform( JTSAdapter.wrap( crd ), transformation );

      return JTSAdapter.export( position );
    }
    catch( final Exception ex )
    {
      throw new GeoGridException( "Could not transform the coordinate ...", ex );
    }
  }

  public static void walkGeoGrid( final IWriteableGeoGrid grid, final IGeoGridWalker walker, final Geometry walkingArea, final IProgressMonitor monitor ) throws Exception
  {
    monitor.beginTask( "Visiting geoGrid", 1 );

    grid.getWalkingStrategy().walk( grid, walker, walkingArea, monitor );
    ProgressUtilities.worked( monitor, 1 );
  }

  public static double calcCellArea( final Coordinate offsetX, final Coordinate offsetY )
  {
    final Coordinate a = new Coordinate( 0, 0 );
    final Coordinate d = new Coordinate( offsetX.x + offsetY.x, offsetX.y + offsetY.y );

    final double ac = offsetX.distance( offsetY );
    final double bd = a.distance( d );

    return 0.5 * ac * bd;
  }

  /**
   * calculates the common envelope for an array of {@link ICoverageCollection}s.
   * 
   * @param collections
   *          the collections
   * @param intersection
   *          if true, the envelope results from an intersection of all coverages of the collections. If false, the
   *          envelope gets calculated by union of the several envelopes.
   */
  public static Geometry getCommonGridEnvelopeForCollections( final ICoverageCollection[] collections, boolean intersection ) throws Exception, GeoGridException, GM_Exception
  {
    Geometry globalEnv = null;

    for( int i = 0; i < collections.length; i++ )
    {
      final ICoverageCollection collection = collections[i];

      Geometry unionGeom = null;
      for( int j = 0; j < collection.size(); j++ )
      {
        final ICoverage coverage = collection.get( j );
        final IGeoGrid grid = GeoGridUtilities.toGrid( coverage );
        final Envelope envelope = grid.getEnvelope();

        final double minX = envelope.getMinX();
        final double minY = envelope.getMinY();
        final double maxX = envelope.getMaxX();
        final double maxY = envelope.getMaxY();

        /* Create the coordinates for the outer ring. */
        final GM_Position c1 = GeometryFactory.createGM_Position( minX, minY );
        final GM_Position c2 = GeometryFactory.createGM_Position( maxX, minY );
        final GM_Position c3 = GeometryFactory.createGM_Position( maxX, maxY );
        final GM_Position c4 = GeometryFactory.createGM_Position( minX, maxY );

        /* Create the outer ring. */
        final GM_Ring_Impl shell = GeometryFactory.createGM_Ring( new GM_Position[] { c1, c2, c3, c4, c1 }, grid.getSourceCRS() );

        /* Create the surface patch. */
        final GM_SurfacePatch patch = GeometryFactory.createGM_SurfacePatch( shell, new GM_Ring[] {}, grid.getSourceCRS() );

        /* Create the surface. */
        final GM_Surface<GM_SurfacePatch> surface = GeometryFactory.createGM_Surface( patch );

        final Geometry geometry = JTSAdapter.export( surface );

        if( unionGeom == null )
          unionGeom = geometry;
        else
          unionGeom = unionGeom.union( geometry );
      }

      if( intersection == true )
      {
        if( globalEnv == null )
          globalEnv = unionGeom;
        else
          globalEnv = globalEnv.intersection( unionGeom );
      }
      else
      {
        if( globalEnv == null )
          globalEnv = unionGeom;
        else
          globalEnv = globalEnv.union( unionGeom );
      }
    }
    return globalEnv;
  }

  /**
   * Flattens several grids into one grid, that has the value set for the category as cell value
   * 
   * @param gridCategories
   *          the categories with which the grid get flattened
   * @param intersection
   *          if true, the envelope results from an intersection of all grids of the categories. If false, the envelope
   *          gets calculated by union of the several envelopes.
   */
  public static FlattenToCategoryGrid getFlattedGrid( GridCategoryWrapper[] gridCategories, final boolean intersection ) throws GM_Exception, GeoGridException
  {
    Geometry globalGridSurfaceBoundary = null;
    Geometry unionGeom = null;

    /* calculate min cell sizes */
    double minCellSizeX = Double.MAX_VALUE;
    double minCellSizeY = Double.MAX_VALUE;
    for( int i = 0; i < gridCategories.length; i++ )
    {
      final GridCategoryWrapper category = gridCategories[i];

      final IGeoGrid[] grids = category.getGrids();
      for( int j = 0; j < grids.length; j++ )
      {
        final IGeoGrid grid = grids[j];

        minCellSizeX = Math.min( Math.abs( grid.getOffsetX().x + grid.getOffsetY().x ), minCellSizeX );
        minCellSizeY = Math.min( Math.abs( grid.getOffsetX().y + grid.getOffsetY().y ), minCellSizeY );

        final String targetCRS = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
        final GM_Surface< ? > surface = GeoGridUtilities.createSurface( grid, targetCRS );

        final Geometry geometry = JTSAdapter.export( surface );

        if( unionGeom == null )
          unionGeom = geometry;
        else
          unionGeom = unionGeom.union( geometry );
      }

      if( intersection == true )
      {
        if( globalGridSurfaceBoundary == null )
          globalGridSurfaceBoundary = unionGeom;
        else
          globalGridSurfaceBoundary = globalGridSurfaceBoundary.intersection( unionGeom );
      }
      else
      {
        if( globalGridSurfaceBoundary == null )
          globalGridSurfaceBoundary = unionGeom;
        else
          globalGridSurfaceBoundary = globalGridSurfaceBoundary.union( unionGeom );
      }
    }

    final GM_Object gmObject = JTSAdapter.wrap( globalGridSurfaceBoundary );
    final GM_Envelope newGridEnv = gmObject.getEnvelope();

    /* create grid */
    // get Bounding box, +1 zelle
    double originX = newGridEnv.getMin().getX() + minCellSizeX / 2;
    double originY = newGridEnv.getMin().getY() + minCellSizeY / 2;

    /* calculate necessary number of cells */
    final double dX = newGridEnv.getMax().getX() - minCellSizeX / 2 - originX;
    final double dY = newGridEnv.getMax().getY() - minCellSizeY / 2 - originY;

    final int numOfColumns = (int) Math.round( dX / minCellSizeX ) + 1;
    final int numOfRows = (int) Math.round( dY / minCellSizeY ) + 1;

    final Double CornerY = originY + (numOfRows - 1) * minCellSizeY;

    final String sourceCRS = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
    final GM_Point gmOrigin = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Point( originX, CornerY, sourceCRS );

    final Coordinate origin = JTSAdapter.export( gmOrigin ).getCoordinate();
    final Coordinate offsetX = new Coordinate( minCellSizeX, 0 );
    final Coordinate offsetY = new Coordinate( 0, -minCellSizeY );

    return new FlattenToCategoryGrid( gridCategories, globalGridSurfaceBoundary, origin, offsetX, offsetY, sourceCRS, numOfColumns, numOfRows );
  }
}