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
package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.elevation.IElevationModel;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_AbstractSurfacePatch;
import org.kalypsodeegree.model.geometry.GM_Triangle;
import org.kalypsodeegree.model.geometry.ISurfacePatchVisitable;
import org.kalypsodeegree.model.geometry.ISurfacePatchVisitor;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * An elevation provider base on ASC file
 *
 * @author Madanagopal
 * @author Patrice Congo
 *
 */
public class ASCTerrainElevationModel implements IElevationModel, ISurfacePatchVisitable<GM_AbstractSurfacePatch>
{
  private static final int PROPORTIONAL_FACTOR = 160;

  private static final int MIN_FINE = 4;

  /**
   * The envelope of the region of interest
   */
  private double cellSize;

  private double elevations[][];

  private int N_COLS;

  private int N_ROWS;

  private double xllcorner;

  private double yllcorner;

  private double minElevation;

  private double maxElevation;

  /**
   * The maximal envelop for the elevation
   */
  private GM_Envelope maxEnvelope;

  // FIXME: this is nonsense, we should use the crs configured at our containing NativeTerrainModelWrapper and transform
  // our data into that crs
  private final String crs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

  /**
   * Create an elevation provider based on the given asc file, in the specified region of interest. if the
   * regionInterest is null the file elevation information is computed and therefore available
   *
   * @param ascFile
   *          the asc file containing the native terrain model
   * @param regionOfInterest
   *          the {@link GM_Envelope} of region of interest
   * @throws IllegalArgumentException
   *           if asc file is null or is a directory or does not exist or is not accesible (cannot be read)
   *
   */
  public ASCTerrainElevationModel( final URL ascFileURL ) throws IllegalArgumentException, IOException
  {
    parse( ascFileURL.openStream() );
  }

  @Override
  public void dispose( )
  {
  }

  private void parse( final InputStream inputStream )
  {
    BufferedReader br = null;
    try
    {
      br = new BufferedReader( new InputStreamReader( inputStream ) );
      final String[] data = new String[6];
      String line;
      // reading header data
      for( int i = 0; i < 6; i++ )
      {
        line = br.readLine();
        final int index = line.indexOf( " " ); //$NON-NLS-1$
        final String subString = line.substring( index );
        data[i] = subString.trim();
      }
      N_COLS = Integer.parseInt( data[0] );
      N_ROWS = Integer.parseInt( data[1] );
      xllcorner = NumberUtils.parseDouble( data[2] );
      yllcorner = NumberUtils.parseDouble( data[3] );
      cellSize = NumberUtils.parseDouble( data[4] );
      final double noDataValue = NumberUtils.parseDouble( data[5] );
      double currentValue;

      elevations = new double[N_ROWS][N_COLS];
      minElevation = Double.MAX_VALUE;
      maxElevation = -Double.MAX_VALUE;

      String[] strRow;
      for( int y = N_ROWS - 1; y >= 0; y-- )
      {
        strRow = br.readLine().trim().split( " " ); //$NON-NLS-1$
        for( int x = 0; x < N_COLS; x++ )
        {
          currentValue = NumberUtils.parseDouble( strRow[x] );
          if( currentValue != noDataValue )
          {
            elevations[y][x] = currentValue;

            if( minElevation > currentValue )
            {
              minElevation = currentValue;
            }

            if( maxElevation < currentValue )
            {
              maxElevation = currentValue;
            }
          }
          else
          {
            elevations[y][x] = Double.NaN;
          }

        }
      }
      maxEnvelope = makeMaxEnvelope();
    }
    catch( final NumberFormatException e )
    {
      e.printStackTrace();
    }
    catch( final IOException e )
    {
      e.printStackTrace();
    }
    finally
    {
      IOUtils.closeQuietly( br );
    }
  }

  private final GM_Envelope makeMaxEnvelope( )
  {
    final GM_Position posMin = GeometryFactory.createGM_Position( xllcorner, yllcorner );
    final GM_Position posMax = GeometryFactory.createGM_Position( xllcorner + cellSize * N_COLS, yllcorner + cellSize * N_ROWS );
    return GeometryFactory.createGM_Envelope( posMin, posMax, crs );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getElevation(org.kalypsodeegree.model.geometry.GM_Point)
   */
  @Override
  public double getElevation( final GM_Point location )
  {
    final int col = (int) Math.floor( (location.getX() - xllcorner) / cellSize );
    final int row = (int) Math.floor( (location.getY() - yllcorner) / cellSize );
    if( col < N_COLS && row < N_ROWS && col >= 0 && row >= 0 )
      return elevations[row][col];

    return Double.NaN;
  }

  /**
   * @see org.kalypsodeegree.model.geometry.ISurfacePatchVisitable#acceptSurfacePatches(org.kalypsodeegree.model.geometry.GM_Envelope,
   *      org.kalypsodeegree.model.geometry.ISurfacePatchVisitor, org.eclipse.core.runtime.IProgressMonitor)
   *
   * Extended for better performance. according to the zoom factor only each (n) cell of the grid will be visited,
   * after getting close enough will refine the representation of model.
   */
  @Override
  public void acceptSurfacePatches( final GM_Envelope envToVisit, final ISurfacePatchVisitor<GM_AbstractSurfacePatch> surfacePatchVisitor, final IProgressMonitor monitor )
  {
    // FIXME: painting a grid with the triangle stuff is NOT appropriate! -> makes never sense, because a grid cell has
    // only onse single value; so splitting up
    // into triangle parts according to color classes is slow and makes absolutely no sense!

    final int iProportional = (int) (Math.min( envToVisit.getHeight(), envToVisit.getWidth()) /( cellSize * PROPORTIONAL_FACTOR ));
    final int iShift = ( Math.max( MIN_FINE, iProportional ) );
    double dShiftMonitor = iShift;
    boolean lBoolDoFine = false;
    if( iProportional < MIN_FINE / 2 ){
      lBoolDoFine = true;
      dShiftMonitor = ( double )MIN_FINE / ( MIN_FINE + 1 ) ;
    }
    final GM_Envelope env = GMRectanglesClip.getIntersectionEnv( maxEnvelope, envToVisit );
    final double xmin = env.getMin().getX();
    final int col = (int) Math.floor( (xmin - xllcorner) / cellSize );
    final double ymin = env.getMin().getY();
    int row = (int) Math.floor( (ymin - yllcorner) / cellSize );
    if( row < 0 )
      row = 0;

    final int totalWork = ( int )( Math.floor( env.getWidth() / cellSize ) / dShiftMonitor );
    if( col < N_COLS && row < N_ROWS && col >= 0 && row >= 0 )
    {
      monitor.beginTask( "", totalWork ); //$NON-NLS-1$

      final int N_COL_ENV = (int) Math.floor( env.getWidth() / cellSize );
      final int N_ROW_ENV = (int) Math.floor( env.getHeight() / cellSize );
      // iteration for rough representation with adaptive walking through the grid. iShift variable defines the step size of walking
      for( int i = 0; i < N_ROW_ENV; i += iShift )
      {
        for( int j = 0; j < N_COL_ENV; j += iShift )
        {
          final double x = xmin + j * cellSize;
          final double xPlusCellSize = x + cellSize * iShift;
          final double y = ymin + i * cellSize;
          final double yPlusCellSize = y + cellSize * iShift;
          final double z = elevations[row + i][col + j];

          final GM_Position pos0 = GeometryFactory.createGM_Position( x, y, z );
          final GM_Position pos1 = GeometryFactory.createGM_Position( xPlusCellSize, y, z );
          final GM_Position pos2 = GeometryFactory.createGM_Position( xPlusCellSize, yPlusCellSize, z );
          final GM_Position pos3 = GeometryFactory.createGM_Position( x, yPlusCellSize, z );

          try
          {
            final GM_AbstractSurfacePatch patch = GeometryFactory.createGM_PolygonPatch( new GM_Position[] { pos0, pos1, pos2, pos3, pos0 }, null, crs );
            surfacePatchVisitor.visit( patch );
          }
          catch( final Throwable e )
          {
            e.printStackTrace();
          }
        }
        if( monitor.isCanceled() ){
          monitor.worked( totalWork );
          monitor.done();
          return;
        }
        ProgressUtilities.worked( monitor, 1 );
      }
      // the refinement iteration, visits all the points of the given grid in actual envelope.
      if( lBoolDoFine )
      {
        for( int i = 0; i < N_ROW_ENV; i++ )
        {
          for( int j = 0; j < N_COL_ENV; j++ )
          {
            final double x = xmin + j * cellSize;
            final double xPlusCellSize = x + cellSize;
            final double y = ymin + i * cellSize;
            final double yPlusCellSize = y + cellSize;
            final double z = elevations[row + i][col + j];

            final GM_Position pos0 = GeometryFactory.createGM_Position( x, y, z );
            final GM_Position pos1 = GeometryFactory.createGM_Position( xPlusCellSize, y, z );
            final GM_Position pos2 = GeometryFactory.createGM_Position( xPlusCellSize, yPlusCellSize, z );
            final GM_Position pos3 = GeometryFactory.createGM_Position( x, yPlusCellSize, z );

            final GM_Triangle patch1 = GeometryFactory.createGM_Triangle( new GM_Position[] { pos0, pos1, pos3 }, crs );
            final GM_Triangle patch2 = GeometryFactory.createGM_Triangle( new GM_Position[] { pos1, pos2, pos3 }, crs );

            try
            {
              surfacePatchVisitor.visit( patch1 );
              surfacePatchVisitor.visit( patch2 );
            }
            catch( final Throwable e )
            {
              e.printStackTrace();
            }
          }
          if( monitor.isCanceled() ){
            monitor.worked( totalWork );
            monitor.done();
            return;
          }
          ProgressUtilities.worked( monitor, 1 );
        }
      }
    }
    return;
  }

  @Override
  public GM_Envelope getBoundingBox( )
  {
    return maxEnvelope;
  }

  public double getCellSize( )
  {
    return cellSize;
  }

  @Override
  public double getMaxElevation( )
  {
    return (maxElevation == -Double.MAX_VALUE) ? Double.NaN : maxElevation;
  }

  @Override
  public double getMinElevation( )
  {
    return (minElevation == Double.MAX_VALUE) ? Double.NaN : minElevation;
  }
}
