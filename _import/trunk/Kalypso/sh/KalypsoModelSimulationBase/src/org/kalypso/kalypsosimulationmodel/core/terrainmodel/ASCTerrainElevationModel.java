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
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.apache.commons.io.IOUtils;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
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
public class ASCTerrainElevationModel implements IElevationProvider, ISurfacePatchVisitable<GM_SurfacePatch>
{
  private static final List<GM_Position> NULL_LIST = Collections.<GM_Position> emptyList();

  // /**
  // * the asc data source File or URL containing the elevation info
  // */
  // private Object ascSource;

  // TODO check using polygons
  /**
   * The envelop if the reagion of interest
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

  private final String crs = KalypsoCorePlugin.getDefault().getCoordinatesSystem();

  /**
   * Create an elevation provider based on the given asc file, in the specified region of interest. if the
   * regionInterest is null the file elevation information is computed and therefore available
   * 
   * @param ascFile
   *            the asc file containing the native terrain model
   * @param regionOfInterest
   *            the {@link GM_Envelope} of region of interest
   * @throws IllegalArgumentException
   *             if asc file is null or is a directory or does not exist or is not accesible (cannot be read)
   * 
   */
  // public ASCTerrainElevationModel(
  // File ascFile,
  // GM_Envelope regionOfInterest)
  // throws IllegalArgumentException, FileNotFoundException
  // {
  // Assert.throwIAEOnNulOrIsDirOrNotExistsOrNotReadable( ascFile );
  // // Assert.throwIAEOnNullParam( regionOfInterest, "regionOfInterest" );
  // this.regionOfInterest=regionOfInterest;
  // this.ascSource=ascFile;
  // parse( ascFile );
  // }
  public ASCTerrainElevationModel( final URL ascFileURL ) throws IllegalArgumentException, IOException
  {
    parse( ascFileURL.openStream() );
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
      xllcorner = Double.parseDouble( data[2] );
      yllcorner = Double.parseDouble( data[3] );
      cellSize = Double.parseDouble( data[4] );
      final double noDataValue = Double.parseDouble( data[5] );
      double currentValue;

      elevations = new double[N_ROWS][N_COLS];
      minElevation = Double.MAX_VALUE;
      maxElevation = Double.MIN_VALUE;

      String[] strRow;
      for( int y = N_ROWS - 1; y >= 0; y-- )
      {
        strRow = br.readLine().trim().split( " " ); //$NON-NLS-1$
        for( int x = 0; x < N_COLS; x++ )
        {
          currentValue = Double.parseDouble( strRow[x] );
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
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    catch( final IOException e )
    {
      // TODO Auto-generated catch block
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
    final GM_Envelope envelope = GeometryFactory.createGM_Envelope( posMin, posMax, getCoordinateSystem() );
    return envelope;

  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getElevation(org.kalypsodeegree.model.geometry.GM_Point)
   */
  public double getElevation( final GM_Point location )
  {
    final int col = (int) Math.floor( (location.getX() - xllcorner) / cellSize );
    final int row = (int) Math.floor( (location.getY() - yllcorner) / cellSize );
    if( col < N_COLS && row < N_ROWS && col >= 0 && row >= 0 )
    {
      return elevations[row][col];
    }
    else
    {
      return Double.NaN;
    }
  }

  public List<GM_Position> getCellLLCornerIterator( final GM_Envelope env )
  {

    final GM_Envelope envToShow = GMRectanglesClip.getIntersectionEnv( maxEnvelope, env );

    if( envToShow == null )
    {
      return NULL_LIST;
    }
    else
    {
      return makeCellsLLCornerIterator( env );
    }
  }

  private List<GM_Position> makeCellsLLCornerIterator( final GM_Envelope env )
  {
    final double xmin = env.getMin().getX();
    final int col = (int) Math.floor( (xmin - xllcorner) / cellSize );
    final double ymin = env.getMin().getY();
    int row = (int) Math.floor( (ymin - yllcorner) / cellSize );
    if( row < 0 )
    {
      row = 0;
    }
    final ArrayList<GM_Position> poses = new ArrayList<GM_Position>();

    if( col < N_COLS && row < N_ROWS && col >= 0 && row >= 0 )
    {
      final int N_COL_ENV = (int) Math.floor( env.getWidth() / cellSize );
      final int N_ROW_ENV = (int) Math.floor( env.getHeight() / cellSize );
      for( int i = 0; i < N_ROW_ENV; i++ )
      {
        for( int j = 0; j < N_COL_ENV; j++ )
        {
          final double x = xmin + j * cellSize;
          final double y = ymin + i * cellSize;
          final double z = elevations[row + i][col + j];

          final GM_Position position = GeometryFactory.createGM_Position( x, y, z );
          poses.add( position );
        }
      }
      //    
      return poses;
    }
    else
    {
      return NULL_LIST;
    }
  }

  public void acceptSurfacePatches( final GM_Envelope envToVisit, final ISurfacePatchVisitor<GM_SurfacePatch> surfacePatchVisitor ) throws GM_Exception
  {
    final GM_Envelope env = GMRectanglesClip.getIntersectionEnv( maxEnvelope, envToVisit );
    final double xmin = env.getMin().getX();
    final int col = (int) Math.floor( (xmin - xllcorner) / cellSize );
    final double ymin = env.getMin().getY();
    int row = (int) Math.floor( (ymin - yllcorner) / cellSize );
    if( row < 0 )
    {
      row = 0;
    }

    if( col < N_COLS && row < N_ROWS && col >= 0 && row >= 0 )
    {
      final int N_COL_ENV = (int) Math.floor( env.getWidth() / cellSize );
      final int N_ROW_ENV = (int) Math.floor( env.getHeight() / cellSize );
      final double[][] interior = new double[][] {};
      for( int i = 0; i < N_ROW_ENV; i++ )
      {
        for( int j = 0; j < N_COL_ENV; j++ )
        {
          final double x = xmin + j * cellSize;
          final double xPlusCellSize = x + cellSize;
          final double y = ymin + i * cellSize;
          final double yPlusCellSize = y + cellSize;
          final double z = elevations[row + i][col + j];

          final double[] exterior = new double[] { x, y, z,// lowerleft corner
              xPlusCellSize, y, z,// lower right side
              xPlusCellSize, yPlusCellSize, z,// upper right corner
              x, yPlusCellSize, z, // upper left corner
              x, y, z };
          final GM_SurfacePatch patch = GeometryFactory.createGM_SurfacePatch( exterior, interior, 3, crs );
          try
          {
            if( !surfacePatchVisitor.visit( patch, z ) )
            {
              return;
            }
          }
          catch( final Exception e )
          {
            throw new GM_Exception( e.getLocalizedMessage(), e );
          }
        }
      }
    }
    else
    {

    }
    return;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getBoundingBox()
   */
  public GM_Envelope getBoundingBox( )
  {
    return maxEnvelope;
  }

  public double getCellSize( )
  {
    return cellSize;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getCoordinateSystem()
   */
  public String getCoordinateSystem( )
  {
    // TODO Patrice introduce the it in the schema
    return this.crs;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getMaxElevation()
   * @returns a valid Maximum Elevation value or Double.NaN and not the default Double.MIN_VALUE
   */
  public double getMaxElevation( )
  {
    return (maxElevation == Double.MIN_VALUE) ? Double.NaN : maxElevation;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getMaxElevation()
   * @returns a valid Minimum Elevation value or Double.NaN and not the default Double.MAX_VALUE
   */
  public double getMinElevation( )
  {
    return (minElevation == Double.MAX_VALUE) ? Double.NaN : minElevation;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#setCoordinateSystem(java.lang.String)
   */
  public void setCoordinateSystem( final String coordinateSystem )
  {

  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.SurfacePatchVisitable#getDiscretisationInterval()
   */
  public double getDiscretisationInterval( )
  {
    // TODO Auto-generated method stub
    return 0;
  }

}
