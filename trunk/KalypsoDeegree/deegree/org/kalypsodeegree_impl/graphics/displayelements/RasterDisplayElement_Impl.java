/*--------------- Kalypso-Deegree-Header ------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de


 history:

 Files in this package are originally taken from deegree and modified here
 to fit in kalypso. As goals of kalypso differ from that one in deegree
 interface-compatibility to deegree is wanted but not retained always.

 If you intend to use this software in other ways than in kalypso
 (e.g. OGC-web services), you should consider the latest version of deegree,
 see http://www.deegree.org .

 all modifications are licensed as deegree,
 original copyright:

 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon GmbH
 http://www.lat-lon.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypsodeegree_impl.graphics.displayelements;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.grid.GeoGridException;
import org.kalypso.grid.GeoGridCell;
import org.kalypso.grid.GeoGridUtilities;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.grid.RectifiedGridCoverageGeoGrid;
import org.kalypsodeegree.graphics.displayelements.RasterDisplayElement;
import org.kalypsodeegree.graphics.sld.RasterSymbolizer;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Object;
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
      m_grid = new RectifiedGridCoverageGeoGrid( feature );
    }

    return m_grid;
  }

  private void releaseGrid( )
  {
    if( m_grid != null )
    {
      m_grid.dispose();
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
        final RasterSymbolizer symbolizer = (RasterSymbolizer) getSymbolizer();
        paintGrid( (Graphics2D) g, grid, projection, symbolizer, monitor );
      }
    }
    catch( final Exception e )
    {
      throw new CoreException( StatusUtilities.statusFromThrowable( e ) );
    }
    finally
    {
      releaseGrid();
    }
  }

  private void paintGrid( final Graphics2D g, final IGeoGrid grid, final GeoTransform projection, final RasterSymbolizer symbolizer, final IProgressMonitor monitor ) throws GeoGridException, CoreException
  {
    final SubMonitor progress = SubMonitor.convert( monitor, "Painting grid", 100 );

    final Envelope gridEnvelope = grid.getBoundingBox();
// paintEnvelope( g, projection, gridEnvelope, new Color( 128, 128, 128, 20 ) );

    /* Calculate cluster size */
    final double gridPixelWidth = projection.getDestX( gridEnvelope.getMaxX() ) - projection.getDestX( gridEnvelope.getMinX() );
    final double cellPixelWidth = gridPixelWidth / grid.getSizeX();
    final int clusterSize = (int) Math.ceil( CLUSTER_CELL_COUNT / cellPixelWidth );

    // TODO: refactor and put the whole iteration into a walk( grid, bbox, clusterSize ) method of the grid, allowing
    // for
    // optimization according to
    // underlying grid

    final Envelope paintEnvelope = JTSAdapter.export( projection.getSourceRect() );

    final Envelope env = gridEnvelope.intersection( paintEnvelope );

    final Coordinate min = new Coordinate( env.getMinX(), env.getMinY() );
    final Coordinate max = new Coordinate( env.getMaxX(), env.getMaxY() );

    final GeoGridCell minCell = GeoGridUtilities.cellFromPosition( grid, min );
    final GeoGridCell maxCell = GeoGridUtilities.cellFromPosition( grid, max );

    final GeoGridCell normalizedMinCell = new GeoGridCell( Math.min( minCell.x, maxCell.x ), Math.min( minCell.y, maxCell.y ) );
    final GeoGridCell normaliedMaxCell = new GeoGridCell( Math.max( minCell.x, maxCell.x ), Math.max( minCell.y, maxCell.y ) );

    final GeoGridCell originCell = GeoGridUtilities.originAsCell( grid );
    final GeoGridCell maxGridCell = GeoGridUtilities.maxCell( grid );

    final GeoGridCell clippedMinCell = normalizedMinCell.max( originCell );
    final GeoGridCell clippedMaxCell = normaliedMaxCell.min( maxGridCell );

    progress.setWorkRemaining( clippedMaxCell.y + 1 - clippedMinCell.y );

    for( int j = clippedMinCell.y; j < clippedMaxCell.y + 1; j += clusterSize )
    {
      for( int i = clippedMinCell.x; i < clippedMaxCell.x + 1; i += clusterSize )
      {
        final double value = grid.getValue( i, j );
        if( !Double.isNaN( value ) )
        {
          final Color color = symbolizer.getColor( value );
          if( color == null )
            continue;

          final Coordinate currentCellCrd = GeoGridUtilities.toCoordinate( grid, i, j, null );
          final Coordinate nextCellCrd = GeoGridUtilities.toCoordinate( grid, i + clusterSize, j + clusterSize, null );
          final Envelope clusterEnvelope = new Envelope( currentCellCrd, nextCellCrd ); // necessairy to normalize rect
          paintEnvelope( g, projection, clusterEnvelope, color );
        }
      }

      ProgressUtilities.worked( monitor, 1 );
    }
  }

  private void paintEnvelope( final Graphics2D g, final GeoTransform projection, final Envelope currentCellEnv, final Color color )
  {
    // We assume the envelope is normalized here, so we can safely switch minY anc maxY
    final double paintMinX = projection.getDestX( currentCellEnv.getMinX() );
    final double paintMinY = projection.getDestY( currentCellEnv.getMinY() );
    final double paintMaxX = projection.getDestX( currentCellEnv.getMaxX() );
    final double paintMaxY = projection.getDestY( currentCellEnv.getMaxY() );

    final int width = (int) Math.ceil( paintMaxX - paintMinX );
    final int height = (int) Math.ceil( paintMinY - paintMaxY );

    g.setColor( color );
    g.fillRect( (int) paintMinX, (int) paintMaxY, width, height );
  }

}