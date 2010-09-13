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
package org.kalypso.kalypsosimulationmodel.core.wind;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.Date;
import java.util.StringTokenizer;

import org.apache.commons.io.IOUtils;
import org.deegree.framework.util.Pair;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.GMRectanglesClip;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree.model.geometry.GM_Triangle;
import org.kalypsodeegree.model.geometry.ISurfacePatchVisitable;
import org.kalypsodeegree.model.geometry.ISurfacePatchVisitor;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * wind data provider base on ASC file
 * 
 * @author ig
 * 
 */
public class ASCWindDataModel implements IWindDataProvider, ISurfacePatchVisitable<GM_SurfacePatch>
{
  /**
   * The envelop if the region of interest
   */
  private double cellSize;

  private Pair< Double, Double > m_arrayWinds[][];

  private int N_COLS;

  private int N_ROWS;

  private double xllcorner;

  private double yllcorner;

  private GM_Envelope maxEnvelope;

  private String crs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

  private Date m_date;

  /**
   * Create an wind data provider based on the given asc file, in the specified region of interest. 
   * 
   * @param ascFile
   *            the asc file containing the native wind model
   * @param regionOfInterest
   *            the {@link GM_Envelope} of region of interest
   * @throws IllegalArgumentException
   *             if asc file is null or is a directory or does not exist or is not accesible (cannot be read)
   * 
   */
  public ASCWindDataModel( final URL ascFileURL, RectifiedGridDomain gridDescriptor ) throws IllegalArgumentException, IOException
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
      xllcorner = NumberUtils.parseDouble( data[2] );
      yllcorner = NumberUtils.parseDouble( data[3] );
      cellSize = NumberUtils.parseDouble( data[4] );
      final double noDataValue = NumberUtils.parseDouble( data[5] );
      double currentValueX;
      double currentValueY;

      m_arrayWinds = new Pair[N_ROWS][N_COLS];

      String[] strRow;
      for( int y = N_ROWS - 1; y >= 0; y-- )
      {
        StringTokenizer lStrTokenizer = new StringTokenizer( br.readLine().trim(), " " ); 
        for( int x = 0; x < N_COLS; x++ )
        {
          String lStrPairOfValues = lStrTokenizer.nextToken();
          int lIntSeparatorPos = lStrPairOfValues.indexOf( ';' );
          currentValueX = NumberUtils.parseDouble( lStrPairOfValues.substring( 0, lIntSeparatorPos ) );
          currentValueY = NumberUtils.parseDouble( lStrPairOfValues.substring( lIntSeparatorPos + 1 ) );
          if( currentValueX != noDataValue && currentValueY != noDataValue )
          {
            m_arrayWinds[y][x] = new Pair< Double, Double >( currentValueX, currentValueY );
          }
          else
          {
            m_arrayWinds[y][x] = new Pair< Double, Double >( Double.NaN, Double.NaN );
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
    return GeometryFactory.createGM_Envelope( posMin, posMax, getCoordinateSystem() );
  }

  /**
   * @see org.kalypsodeegree.model.geometry.ISurfacePatchVisitable#acceptSurfacePatches(org.kalypsodeegree.model.geometry.GM_Envelope,
   *      org.kalypsodeegree.model.geometry.ISurfacePatchVisitor, org.eclipse.core.runtime.IProgressMonitor)
   */
  public void acceptSurfacePatches( final GM_Envelope envToVisit, final ISurfacePatchVisitor<GM_SurfacePatch> surfacePatchVisitor, final IProgressMonitor monitor ) throws CoreException, GM_Exception
  {
    final GM_Envelope env = GMRectanglesClip.getIntersectionEnv( maxEnvelope, envToVisit );
    final double xmin = env.getMin().getX();
    final int col = (int) Math.floor( (xmin - xllcorner) / cellSize );
    final double ymin = env.getMin().getY();
    int row = (int) Math.floor( (ymin - yllcorner) / cellSize );
    if( row < 0 )
      row = 0;

    if( col < N_COLS && row < N_ROWS && col >= 0 && row >= 0 )
    {
      monitor.beginTask( "", row ); //$NON-NLS-1$

      final int N_COL_ENV = (int) Math.floor( env.getWidth() / cellSize );
      final int N_ROW_ENV = (int) Math.floor( env.getHeight() / cellSize );
      for( int i = 0; i < N_ROW_ENV; i++ )
      {
        for( int j = 0; j < N_COL_ENV; j++ )
        {
          final double x = xmin + j * cellSize;
          final double xPlusCellSize = x + cellSize;
          final double y = ymin + i * cellSize;
          final double yPlusCellSize = y + cellSize;
          final Pair< Double, Double > z = m_arrayWinds[row + i][col + j];

          final GM_Position pos00 = GeometryFactory.createGM_Position( x, y, z.first );
          final GM_Position pos01 = GeometryFactory.createGM_Position( xPlusCellSize, y, z.first );
          final GM_Position pos02 = GeometryFactory.createGM_Position( xPlusCellSize, yPlusCellSize, z.first );
          final GM_Position pos03 = GeometryFactory.createGM_Position( x, yPlusCellSize, z.first );
          
          final GM_Position pos10 = GeometryFactory.createGM_Position( x, y, z.second );
          final GM_Position pos11 = GeometryFactory.createGM_Position( xPlusCellSize, y, z.second );
          final GM_Position pos12 = GeometryFactory.createGM_Position( xPlusCellSize, yPlusCellSize, z.second );
          final GM_Position pos13 = GeometryFactory.createGM_Position( x, yPlusCellSize, z.second );
 
          final GM_Triangle patch1 = GeometryFactory.createGM_Triangle( new GM_Position[] { pos00, pos01, pos03 }, crs );
          final GM_Triangle patch2 = GeometryFactory.createGM_Triangle( new GM_Position[] { pos11, pos12, pos13 }, crs );
          surfacePatchVisitor.visit( patch1, z.first );
          surfacePatchVisitor.visit( patch2, z.first );
        }

        ProgressUtilities.worked( monitor, 1 );
      }
    }
    else
    {

    }
    return;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IWindDataProvider#getBoundingBox()
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
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IWindDataProvider#getCoordinateSystem()
   */
  public String getCoordinateSystem( )
  {
    return this.crs;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IWindDataProvider#setCoordinateSystem(java.lang.String)
   */
  public void setCoordinateSystem( final String coordinateSystem )
  {
    crs = coordinateSystem;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.flowrel.IWindDataProvider#getWindAsSpeedAndDirection(org.kalypsodeegree.model.geometry.GM_Point)
   */
  @Override
  public Pair< Double, Double > getWindAsSpeedAndDirection( GM_Point location )
  {
    final int col = (int) Math.floor( (location.getX() - xllcorner) / cellSize );
    final int row = (int) Math.floor( (location.getY() - yllcorner) / cellSize );
    if( col < N_COLS && row < N_ROWS && col >= 0 && row >= 0 )
      return m_arrayWinds[row][col];

    return new Pair< Double, Double >( Double.NaN, Double.NaN );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.flowrel.IWindDataProvider#getWindAsVector(org.kalypsodeegree.model.geometry.GM_Point)
   */
  @Override
  public Pair< Double, Double > getWindAsVector( GM_Point location )
  {
    final int col = (int) Math.floor( (location.getX() - xllcorner) / cellSize );
    final int row = (int) Math.floor( (location.getY() - yllcorner) / cellSize );
    if( col < N_COLS && row < N_ROWS && col >= 0 && row >= 0 )
      return m_arrayWinds[row][col];

    return new Pair< Double, Double >( Double.NaN, Double.NaN );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataProvider#getDataAsGrid()
   */
  @Override
  public IGeoGrid getDataAsGrid( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataProvider#getGridDescriptor()
   */
  @Override
  public RectifiedGridDomain getGridDescriptor( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataProvider#isRegularGrid()
   */
  @Override
  public boolean isRegularGrid( )
  {
    // TODO Auto-generated method stub
    return false;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataProvider#getDataFile()
   */
  @Override
  public URL getDataFileURL( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataProvider#getDateStep()
   */
  @Override
  public Date getDateStep( )
  {
    return m_date;
  }
}
