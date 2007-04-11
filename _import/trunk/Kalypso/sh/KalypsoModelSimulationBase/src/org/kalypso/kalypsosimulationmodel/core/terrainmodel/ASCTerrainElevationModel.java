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
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * An elevation provider base on ASC file
 * 
 * @author Madanagopal
 * @author Patrice Congo 
 *
 */
public class ASCTerrainElevationModel implements IElevationProvider, SurfacePatchVisitable
{
  
  
  private static final List<GM_Position> NULL_LIST = Collections.<GM_Position>emptyList();

//  /**
//   * the asc data source File or URL containing the elevation info 
//   */
//  private Object ascSource;
  
  //TODO check using polygons
  /**
   * The envelop if the reagion of interest
   */
  private GM_Envelope regionOfInterest;
  
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
  private  GM_Envelope maxEnvelope;

  private CS_CoordinateSystem crs = CRS_GAUSS_KRUEGER;
  
  /**
   * Create an elevation provider based on the given asc file, in the specified 
   * region of interest. if the regionInterest is null the file elevation 
   * information is computed and therefore available
   * @param ascFile the asc file containing the native terrain model
   * @param regionOfInterest the {@link GM_Envelope} of region of interest
   * @throws IllegalArgumentException if asc file is null or is a directory or does not exist
   *        or is not accesible (cannot be read)
   * 
   */
//  public ASCTerrainElevationModel(
//              File ascFile,
//              GM_Envelope regionOfInterest)
//              throws IllegalArgumentException, FileNotFoundException
//  {
//    Assert.throwIAEOnNulOrIsDirOrNotExistsOrNotReadable( ascFile );
////    Assert.throwIAEOnNullParam( regionOfInterest, "regionOfInterest" );
//    this.regionOfInterest=regionOfInterest;
//    this.ascSource=ascFile;
//    parse( ascFile );
//  }
  
  public ASCTerrainElevationModel(
      URL ascFileURL,
      GM_Envelope regionOfInterest)
      throws IllegalArgumentException, IOException
  {
    this.regionOfInterest=regionOfInterest;
    parse( ascFileURL.openStream() );
  }
  

  
  private void parse(InputStream inputStream)
  {
    BufferedReader br = null;
    try
    {
      br = new BufferedReader( new InputStreamReader(inputStream) );
      final String[] data = new String[6];
      String line;
      //reading header data
      for( int i = 0; i < 6; i++ )
      {
        line = br.readLine();
        final int index = line.indexOf( " " ); //$NON-NLS-1$
        final String subString = line.substring( index );
        data[i] = subString.trim();
      }
      N_COLS = Integer.parseInt( data[0] );
      N_ROWS = Integer.parseInt( data[1] );
      xllcorner= Double.parseDouble( data[2] );
      yllcorner= Double.parseDouble( data[3] );
      cellSize=Integer.parseInt( data[4] );
      double noDataValue = Double.parseDouble( data[5] );
      double currentValue;
  
      elevations = new double[N_ROWS][N_COLS];
      minElevation=Double.MAX_VALUE;
      maxElevation=Double.MIN_VALUE;       
      
      String[] strRow;
      for(int y=N_ROWS-1; y>=0; y--)
      {
        strRow = br.readLine().trim().split( " " );
        for(int x=0; x<N_COLS; x++)
        {
          currentValue = Double.parseDouble( strRow[x] );
          if(currentValue!=noDataValue)
          {
            elevations[y][x] = currentValue;
            
            if(minElevation>currentValue)
            {
              minElevation = currentValue;
            }
            
            if(maxElevation<currentValue)
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
      maxEnvelope=makeMaxEnvelope();
    }
    catch( NumberFormatException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    catch( IOException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    finally {
      IOUtils.closeQuietly( br );
    }
  }
  
  private final GM_Envelope makeMaxEnvelope()
  {
    
    GM_Position posMin = 
        GeometryFactory.createGM_Position( xllcorner, yllcorner );
    GM_Position posMax = 
        GeometryFactory.createGM_Position( 
                            xllcorner+cellSize*N_COLS, 
                            yllcorner+cellSize*N_ROWS );
    GM_Envelope envelope = 
        GeometryFactory.createGM_Envelope( posMin, posMax );    
    return envelope;
    
  }
  
  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getElevation(org.kalypsodeegree.model.geometry.GM_Point)
   */
  public double getElevation( GM_Point location )
  {
    int col = (int)Math.floor( (location.getX()-xllcorner)/cellSize );
    int row = (int)Math.floor( (location.getY()-yllcorner)/cellSize );
    if(col<N_COLS && row<N_ROWS && col>=0 && row>=0)
    {
      return elevations[row][col];
    }
    else
    {
      return Double.NaN;
    }
  }
  
  public List<GM_Position> getCellLLCornerIterator(GM_Envelope env)
  {
    
    GM_Envelope envToShow = 
        GMRectanglesClip.getIntersectionEnv(  
                        maxEnvelope, env );
    
    if(envToShow==null)
    {
      return NULL_LIST; 
    }
    else
    {
      return makeCellsLLCornerIterator(env);
    }
  }



  private List<GM_Position> makeCellsLLCornerIterator( 
                                              GM_Envelope env )
  {
    double xmin = env.getMin().getX();
    int col = (int)Math.floor( (xmin-xllcorner)/cellSize );
    double ymin = env.getMin().getY();
    int row = (int)Math.floor( (ymin-yllcorner)/cellSize );
    if(row<0)
    {
      row=0;
    }
    ArrayList<GM_Position> poses = new ArrayList<GM_Position>();
    
    if(col<N_COLS && row<N_ROWS && col>=0 && row>=0)
    {
      int N_COL_ENV = (int)Math.floor( env.getWidth()/cellSize);
      int N_ROW_ENV = (int)Math.floor( env.getHeight()/cellSize);
      for(int i=0;i<N_ROW_ENV;i++)
      {
        for(int j=0;j<N_COL_ENV;j++)
        {
          double x=xmin+j*cellSize;
          double y=ymin+i*cellSize;
          double z=elevations[row+i][col+j];
          
          GM_Position position=
              GeometryFactory.createGM_Position(x,y,z );
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
  
  public void aceptSurfacePatches(GM_Envelope envToVisit, SurfacePatchVisitor surfacePatchVisitor ) throws GM_Exception
  {
    GM_Envelope env = 
      GMRectanglesClip.getIntersectionEnv(  
                      maxEnvelope, envToVisit );
    double xmin = env.getMin().getX();
    int col = (int)Math.floor( (xmin-xllcorner)/cellSize );
    double ymin = env.getMin().getY();
    int row = (int)Math.floor( (ymin-yllcorner)/cellSize );
    if(row<0)
    {
     row=0;
    }
    
    if(col<N_COLS && row<N_ROWS && col>=0 && row>=0)
    {
      int N_COL_ENV = (int)Math.floor( env.getWidth()/cellSize);
      int N_ROW_ENV = (int)Math.floor( env.getHeight()/cellSize);
      GM_Surface surfacePatch=null;
      for(int i=0;i<N_ROW_ENV;i++)
      {
        for(int j=0;j<N_COL_ENV;j++)
        {
          double x = xmin+j*cellSize;
          double xPlusCellSize=x+cellSize;
          double y = ymin+i*cellSize;
          double yPlusCellSize=y+cellSize;
          double z = elevations[row+i][col+j];
//          GM_Position position=
//            GeometryFactory.createGM_Position(x,y,z );
          double[] exterior = 
            new double[]{
              x,y,z,//lowerleft corner
              xPlusCellSize,y,z,//lower right side
              xPlusCellSize,yPlusCellSize,z,//upper right corner
              x,yPlusCellSize,z, //upper left corner
              x,y,z
            };
          double[][] interior=new double[][]{};
          surfacePatch = 
              GeometryFactory.createGM_Surface( 
                      exterior, interior, 3, getCoordinateSystem() );
          if(!surfacePatchVisitor.visit( surfacePatch, z ))
          {
            return;
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
  public CS_CoordinateSystem getCoordinateSystem( )
  {
    //TODO Patrice introduce the it in the schema
    return this.crs ;
  }  

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getMaxElevation()
   * @returns a valid Maximum Elevation value or Double.NaN and not the default Double.MIN_VALUE
   */
  public double getMaxElevation( )
  {
    return (maxElevation==Double.MIN_VALUE)?Double.NaN:maxElevation;
  }
  
  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getMaxElevation()
   * @returns a valid Minimum Elevation value or Double.NaN and not the default Double.MAX_VALUE
   */
  public double getMinElevation( )
  {
    return (minElevation==Double.MAX_VALUE)?Double.NaN:minElevation;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#setCoordinateSystem(java.lang.String)
   */
  public void setCoordinateSystem( String coordinateSystem )
  {    
       
  }

}
