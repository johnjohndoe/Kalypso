/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 *
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always.
 *
 * If you intend to use this software in other ways than in kalypso
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree,
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree_impl.graphics.displayelements;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.lang.ref.WeakReference;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.grid.GeoGridCell;
import org.kalypso.grid.GeoGridException;
import org.kalypso.grid.GeoGridUtilities;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.grid.RectifiedGridCoverageGeoGrid;
import org.kalypso.grid.GeoGridUtilities.Interpolation;
import org.kalypso.transformation.GeoTransformer;
import org.kalypsodeegree.graphics.displayelements.PointDisplayElement;
import org.kalypsodeegree.graphics.displayelements.PolygonDisplayElement;
import org.kalypsodeegree.graphics.displayelements.RasterDisplayElement;
import org.kalypsodeegree.graphics.sld.Graphic;
import org.kalypsodeegree.graphics.sld.Mark;
import org.kalypsodeegree.graphics.sld.PointSymbolizer;
import org.kalypsodeegree.graphics.sld.PolygonSymbolizer;
import org.kalypsodeegree.graphics.sld.RasterSymbolizer;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.graphics.sld.PointSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.PolygonSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.StyleFactory;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;

/**
 * @author Gernot Belger
 * @author Thomas Jung
 */
public class RasterDisplayElement_Impl extends GeometryDisplayElement_Impl implements RasterDisplayElement
{
  private static final int CLUSTER_CELL_COUNT = 1;

  private IGeoGrid m_grid = null;

  RasterDisplayElement_Impl( final Feature feature, final GM_Object[] geometry, final RasterSymbolizer symbolizer )
  {
    super( feature, geometry, symbolizer );
  }

  private IGeoGrid getGrid( ) throws Exception
  {
    if( m_grid == null )
    {
      final Feature feature = getFeature();
      if( feature.getWorkspace() == null )
        return null;

      m_grid = getCachedGrid( feature );
    }

    return m_grid;
  }

  private void releaseGrid( )
  {
    if( m_grid != null )
    {
      // m_grid.dispose();
      m_grid = null;
    }
  }

  /**
   * renders the DisplayElement to the submitted graphic context
   */
  @Override
  public void paint( final Graphics g, final GeoTransform projection, final IProgressMonitor monitor ) throws CoreException
  {
    try
    {
      final IGeoGrid grid = getGrid();
      if( grid != null )
      {
        final String targetCrs = projection.getSourceRect().getCoordinateSystem();
        Assert.isNotNull( targetCrs );
        final RasterSymbolizer symbolizer = (RasterSymbolizer) getSymbolizer();
        paintGrid( (Graphics2D) g, grid, projection, symbolizer, targetCrs, monitor );
      }
    }
    catch( final CoreException ce )
    {
      throw ce;
    }
    catch( final Exception e )
    {
      final IStatus status = StatusUtilities.createStatus( IStatus.ERROR, "Failed to paint grid", e );
      throw new CoreException( status );
    }
    finally
    {
      releaseGrid();
    }
  }

  private void paintGrid( final Graphics2D g, final IGeoGrid grid, final GeoTransform projection, final RasterSymbolizer symbolizer, final String targetCRS, final IProgressMonitor monitor ) throws GeoGridException, CoreException
  {
    /* Progress monitor. */
    final SubMonitor progress = SubMonitor.convert( monitor, "Painting grid", 100 );

    /* Get the envelope of the surface of the grid (it is transformed). */
    final GM_Envelope envelope = grid.getSurface( targetCRS ).getEnvelope();

    /* Convert this to an JTS envelope. */
    final Envelope gridEnvelope = JTSAdapter.export( envelope );

    /* Paint the envelope. */
    // paintEnvelope( g, projection, gridEnvelope, new Color( 128, 128, 128, 20 ) );
    /* Calculate cluster size */

    /* The width of the envelope (in pixel) on the screen. */
    final double gridPixelWidth = projection.getDestX( gridEnvelope.getMaxX() ) - projection.getDestX( gridEnvelope.getMinX() );

    /* The cell width (in pixel). */
    final double cellPixelWidth = gridPixelWidth / grid.getSizeX();

    /* The cluster size. */
    final int clusterSize = (int) Math.ceil( CLUSTER_CELL_COUNT / cellPixelWidth );

    /* The enevlope of the map extent (in geo coordinates). */
    final Envelope paintEnvelope = JTSAdapter.export( projection.getSourceRect() );

    /* The intersection of the map extent envelope and the evelope of the surface (both in geo coordinates). */
    final Envelope env = gridEnvelope.intersection( paintEnvelope );

    /* This coordinates (in geo coordinates) are in the target coordinate system. */
    final Coordinate min = new Coordinate( env.getMinX(), env.getMinY() );
    final Coordinate max = new Coordinate( env.getMaxX(), env.getMaxY() );

    /* Find the cells (transforming the coordinates in the coordinate system of the grid). */
    final GeoGridCell minCell = GeoGridUtilities.cellFromPosition( grid, GeoGridUtilities.transformCoordinate( grid, min, targetCRS ) );
    final GeoGridCell maxCell = GeoGridUtilities.cellFromPosition( grid, GeoGridUtilities.transformCoordinate( grid, max, targetCRS ) );

    /* Normalize them. */
    final GeoGridCell normalizedMinCell = new GeoGridCell( Math.min( minCell.x, maxCell.x ), Math.min( minCell.y, maxCell.y ) );
    final GeoGridCell normaliedMaxCell = new GeoGridCell( Math.max( minCell.x, maxCell.x ), Math.max( minCell.y, maxCell.y ) );

    /* Some default cells. */
    final GeoGridCell originCell = GeoGridUtilities.originAsCell( grid );
    final GeoGridCell maxGridCell = GeoGridUtilities.maxCell( grid );

    /* They are clipped. */
    final GeoGridCell clippedMinCell = normalizedMinCell.max( originCell );
    final GeoGridCell clippedMaxCell = normaliedMaxCell.min( maxGridCell );

    // Experimental: change interpolation method for better rendering; is quit slow however
    final GeoGridUtilities.Interpolation interpolation = Interpolation.none;

    if( cellPixelWidth < 1 || interpolation != Interpolation.none )
    {
      /* Cell is smaller than one pixel, we iterate through all pixels and get their values. */

      final int screenXfrom = (int) projection.getDestX( env.getMinX() );
      final int screenXto = (int) projection.getDestX( env.getMaxX() );
      final int screenYfrom = (int) projection.getDestY( env.getMaxY() );
      final int screenYto = (int) projection.getDestY( env.getMinY() );

      final int screenYheight = screenYto - screenYfrom;

      progress.setWorkRemaining( screenYheight );

      for( int y = screenYfrom; y < screenYto; y++ )
      {
        for( int x = screenXfrom; x < screenXto; x++ )
        {
          /* These coordinates should be in the target coordinate system. */
          final double geoX = projection.getSourceX( x );
          final double geoY = projection.getSourceY( y );

          final Coordinate crd = new Coordinate( geoX, geoY );

          final double value;

          /* If cell size is lower than pixel size, we do not need to interpolate. */
          if( cellPixelWidth < 1 )
          {
            /* Transform the coordinate into the coordinate system of the grid, in order to find the cell. */
            final GeoGridCell cell = GeoGridUtilities.cellFromPosition( grid, GeoGridUtilities.transformCoordinate( grid, crd, targetCRS ) );

            value = grid.getValueChecked( cell.x, cell.y );
          }
          else
          {
            /* Transform the coordinate into the coordinate system of the grid, in order to find the cell. */
            value = GeoGridUtilities.getValue( grid, GeoGridUtilities.transformCoordinate( grid, crd, targetCRS ), interpolation );
          }

          if( Double.isNaN( value ) )
            continue;

          final Color color = symbolizer.getColor( value );
          if( color == null )
            continue;

          g.setColor( color );
          g.fillRect( x, y, 1, 1 );
        }

        ProgressUtilities.worked( monitor, 1 );
      }
    }
    else
    {
      progress.setWorkRemaining( clippedMaxCell.y + 2 - clippedMinCell.y );

      /* Iterate through the grid */
      // REMARK: always iterate through a bigger extent in order to compensate for rounding problems during
      // determination of the cell-box
      for( int j = clippedMinCell.y - 1; j < clippedMaxCell.y + 1; j += clusterSize )
      {
        for( int i = clippedMinCell.x - 1; i < clippedMaxCell.x + 1; i += clusterSize )
        {
          final double value = grid.getValueChecked( i, j );
          if( !Double.isNaN( value ) )
          {
            final Color color = symbolizer.getColor( value );
            if( color == null )
              continue;

            /* Get the surface of the cell (in the target coordinate system). */
            final GM_Surface< ? > cell = grid.getCell( i, j, targetCRS );
            final GM_Envelope cellEnvelope = cell.getEnvelope();
            final Envelope jtsCellEnvelope = JTSAdapter.export( cellEnvelope );
            paintEnvelope( g, projection, jtsCellEnvelope, color );
          }
        }

        ProgressUtilities.worked( monitor, 1 );
      }
    }

    /* DEBUG: This can be used to paint the grid cells and its center point. */
    // paintCells( g, grid, projection, targetCRS, true, true );
  }

  // REMARK: below is an essay of using geotools to render the coverages, but it
  // does not work due to dependency problems.
  // Please do not delete (for the moment).
// private void paintCoverage( final Graphics2D g, final RectifiedGridCoverage coverage, final GeoTransform projection,
// final ImageSymbolizer symbolizer, final IProgressMonitor monitor ) throws MalformedURLException,
// NoSuchAuthorityCodeException, FactoryException, TransformException, NoninvertibleTransformException
// {
// final SubMonitor progress = SubMonitor.convert( monitor, "Painting coverage", 100 );
//
// final GM_Envelope coverageEnvelope = coverage.getEnvelope();
// final Envelope jtsCoverageEnvelope = JTSAdapter.export( coverageEnvelope );
//
// paintEnvelope( g, projection, jtsCoverageEnvelope, new Color( 128, 128, 128, 20 ) );
//
// final RangeSetType rangeSet = coverage.getRangeSet();
// final FileType file = rangeSet.getFile();
// if( file == null )
// {
// // we definitely only support referenced images here
// }
// else
// {
// final String fileName = file.getFileName();
// final URL context = getFeature().getWorkspace().getContext();
// final URL fileURL = UrlResolverSingleton.getDefault().resolveURL( context, fileName );
//
// final RenderedOp image = JAI.create( "url", fileURL );
//
// final CoordinateReferenceSystem destinationCrs = CRS.decode( "ESPG:31467" );
//
// final ReferencedEnvelope envelope = new ReferencedEnvelope( jtsCoverageEnvelope, destinationCrs );
//
// final GM_Envelope screenEnvelope = projection.getDestRect();
//
// final GridCoverage2D geotoolsCoverage = new GridCoverageFactory().create( fileName, image, envelope );
//
// final Rectangle screenSize = new Rectangle( (int) screenEnvelope.getMin().getX(), (int)
// screenEnvelope.getMin().getY(), (int) screenEnvelope.getWidth(), (int) screenEnvelope.getHeight() );
// final RenderingHints java2dHints = null;
// final GridCoverageRenderer gcr = new GridCoverageRenderer( destinationCrs, envelope, screenSize, java2dHints );
//
// // RasterSymbolizer geotoolsSymbolizer = new RasterSymbolizer();
// gcr.paint( g, geotoolsCoverage, null );
//
// image.dispose();
// }
// }

  /**
   * Paints one cell in form of an envelope.<br>
   * This is used instead of reusing PolygoneDisplayElement or similar, as for the grid it is crucial, that the border
   * of two cells is painted without intersection or gap.
   */
  private void paintEnvelope( final Graphics2D g, final GeoTransform projection, final Envelope currentCellEnv, final Color color )
  {
    // We assume the envelope is normalized here, so we can safely switch minY anc maxY
    final double paintMinX = projection.getDestX( currentCellEnv.getMinX() );
    final double paintMinY = projection.getDestY( currentCellEnv.getMinY() );
    final double paintMaxX = projection.getDestX( currentCellEnv.getMaxX() );
    final double paintMaxY = projection.getDestY( currentCellEnv.getMaxY() );

    final int x1 = (int) Math.ceil( paintMinX );
    final int y1 = (int) Math.ceil( paintMaxY );

    final int x2 = (int) Math.ceil( paintMaxX );
    final int y2 = (int) Math.ceil( paintMinY );

    final int width = x2 - x1;
    final int height = y2 - y1;

    g.setColor( color );
    g.fillRect( x1, y1, width, height );
  }

  //
  // THE GRID CACHE
  //

  private static final Map<GridCacheKey, WeakReference<IGeoGrid>> WEAK_CACHE = new HashMap<GridCacheKey, WeakReference<IGeoGrid>>();

  private static synchronized IGeoGrid getCachedGrid( final Feature feature ) throws Exception
  {
    final GridCacheKey gridCacheKey = new GridCacheKey( feature.getId(), feature.getWorkspace().getContext() );

// final WeakReference<IGeoGrid> ref = WEAK_CACHE.get( gridCacheKey );
// if( ref != null )
// {
// final IGeoGrid cachedGrid = ref.get();
// if( cachedGrid != null )
// return cachedGrid;
// }

    // Grid not available, create it
    final IGeoGrid newGrid = new RectifiedGridCoverageGeoGrid( feature );

    final WeakReference<IGeoGrid> newRef = new WeakReference<IGeoGrid>( newGrid );
    WEAK_CACHE.put( gridCacheKey, newRef );

    return newGrid;
  }

  /**
   * Key class used for caching the grids.
   */
  private static class GridCacheKey
  {
    // REMARK: all fields appear to be unsused, but they are: via reflection in equals() and hashCode()

    @SuppressWarnings("unused")
    private final String m_featureId;

    @SuppressWarnings("unused")
    private final URL m_url;

    public GridCacheKey( final String featureId, final URL url )
    {
      m_featureId = featureId;
      m_url = url;
    }

    /**
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals( final Object obj )
    {
      return EqualsBuilder.reflectionEquals( this, obj );
    }

    /**
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode( )
    {
      return HashCodeBuilder.reflectionHashCode( this );
    }
  }

  /**
   * This function paints the cells and/or the cells center points.
   */
  @SuppressWarnings("unused")
  // REMARK: not used, normally; but is used for debugging the raster stuff. Maybe add tracing option to switch this
  // on/off
  private void paintCells( final Graphics2D g, final IGeoGrid grid, final GeoTransform projection, final String targetCRS, final boolean cells, final boolean centerPoints )
  {
    try
    {
      for( int x = 0; x < grid.getSizeX(); x++ )
      {
        for( int y = 0; y < grid.getSizeY(); y++ )
        {
          if( cells )
          {
            /* Define how the cell will be drawn. */
            final PolygonSymbolizer cellSymbolizer = new PolygonSymbolizer_Impl();
            cellSymbolizer.getFill().setOpacity( 0 );
            cellSymbolizer.getFill().setFill( Color.RED );
            cellSymbolizer.getStroke().setStroke( Color.BLACK );
            cellSymbolizer.getStroke().setWidth( 1 );

            /* Create the cells geometry. */
            final GM_Surface< ? > surface = GeoGridUtilities.createCell( grid, x, y, targetCRS );

            /* Paint the cell at this position. */
            final PolygonDisplayElement cellDisplayElement = DisplayElementFactory.buildPolygonDisplayElement( null, surface, cellSymbolizer );
            cellDisplayElement.paint( g, projection, new NullProgressMonitor() );
          }

          if( centerPoints )
          {
            /* Define how the center point will be drawn. */
            final PointSymbolizer centerPointSymbolizer = new PointSymbolizer_Impl();

            /* Create the mark. */
            final Mark mark = StyleFactory.createMark( "square", Color.BLACK, Color.BLACK, 2 );
            final Graphic graphic = StyleFactory.createGraphic( null, mark, 1, 2, 0 );
            centerPointSymbolizer.setGraphic( graphic );

            /* Get the center point. */
            final Coordinate coordinate = GeoGridUtilities.toCoordinate( grid, x, y, null );

            /* This is the center point in the coordinate system of the grid. */
            final GM_Point centerPoint = GeometryFactory.createGM_Point( coordinate.x, coordinate.y, grid.getSourceCRS() );

            /* Transform it to the target coordinate system. */
            final GeoTransformer geo = new GeoTransformer( targetCRS );
            final GM_Object transformedCenterPoint = geo.transform( centerPoint );

            /* Draw the center point. */
            final PointDisplayElement centerPointDisplayElement = DisplayElementFactory.buildPointDisplayElement( null, transformedCenterPoint, centerPointSymbolizer );
            centerPointDisplayElement.paint( g, projection, new NullProgressMonitor() );
          }
        }
      }
    }
    catch( final Exception ex )
    {
      ex.printStackTrace();
    }
  }
}