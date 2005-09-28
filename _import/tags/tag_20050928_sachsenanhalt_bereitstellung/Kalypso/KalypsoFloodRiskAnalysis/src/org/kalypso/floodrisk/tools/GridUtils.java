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
package org.kalypso.floodrisk.tools;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.math.BigDecimal;
import java.net.URL;
import java.util.Vector;

import org.kalypso.floodrisk.data.RasterDataModel;
import org.kalypsodeegree.model.coverage.GridRange;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.cv.GridRange_Impl;
import org.kalypsodeegree_impl.model.cv.RangeSet;
import org.kalypsodeegree_impl.model.cv.RectifiedGridCoverage;
import org.kalypsodeegree_impl.model.cv.RectifiedGridDomain;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * 
 * Class for reading and writing ascii-Grid files
 * 
 * @author N. Peiler
 * 
 *  
 */
public abstract class GridUtils
{
  static RasterDataModel rasterDataModel = new RasterDataModel();

  /**
   * exports a RectifiedGridCoverage to an Ascii-Grid
   * 
   * @param out
   *          ascii output file
   * @param grid
   *          RectifiedGridCoverage to export
   * @throws Exception
   */
  public static void exportGridArc( File out, RectifiedGridCoverage grid ) throws Exception
  {
    BufferedWriter bw = new BufferedWriter( new FileWriter( out ) );
    RectifiedGridDomain gridDomain = grid.getGridDomain();
    RangeSet rangeSet = grid.getRangeSet();
    int nCols = gridDomain.getNumColumns();
    int nRows = gridDomain.getNumRows();
    GM_Point origin = gridDomain.getOrigin( null );
    double originX = origin.getX();
    double originY = origin.getY();
    double offsetX = gridDomain.getOffsetX( origin.getCoordinateSystem() );
    int nodata = -9999;
    Vector rangeSetData = rangeSet.getRangeSetData();
    try
    {
      bw.write( "ncols " + nCols + "\nnrows " + nRows + "\nxllcorner " + originX + "\nyllcorner " + originY
          + "\ncellsize " + offsetX + "\nnodata_value " + nodata );
      bw.newLine();

      for( int i = 0; i < rangeSetData.size(); i++ )
      {
        Vector rowData = (Vector)rangeSetData.get( i );
        for( int j = 0; j < rowData.size(); j++ )
        {
          if( rowData.get( j ) != null )
          {
            double value = ( (Double)rowData.get( j ) ).doubleValue();
            double roundValue = Number.round( value, 6, BigDecimal.ROUND_HALF_EVEN );
            bw.write( roundValue + " " );
          }
          else
          {
            bw.write( nodata + " " );
          }
          /*
           * System.out.println(i + "," + j + ": " + rowData.get(j) + " ");
           */
        }//for j
        bw.newLine();
      }//for i
      bw.close();
    }// try
    catch( IOException e )
    {
      bw.write( "Error while writing ascii-arc file: " + e.getMessage() );
      throw e;
    }// catch
    finally
    {
      bw.close();
      if( out.length() == 0 )
        out.delete();
    }// finally
  }//exportGridArc

  /**
   * imports an ascii-grid file
   * 
   * @param in
   *          input file (*.asc)
   * @param cs
   *          the coordinate system for the geometric data of the ascii-grid
   * @return RectifiedGridCoverage
   */
  public static RectifiedGridCoverage importGridArc( File in, CS_CoordinateSystem cs )
  {
    int nCols = 0;
    int nRows = 0;
    GM_Point origin = null;
    double[] offset = new double[2];
    String nodata = null;
    Vector rangeSetData = new Vector();
    //StringBuffer rangeData = new StringBuffer();
    Vector rangeData = new Vector();
    try
    {
      BufferedReader br = new BufferedReader( new FileReader( in ) );
      String[] data = new String[6];
      String line;
      for( int i = 0; i < data.length; i++ )
      {
        line = br.readLine();
        int index = line.indexOf( " " );
        String subString = line.substring( index );
        data[i] = subString.trim();
        System.out.println( data[i] );
      }
      nCols = new Integer( data[0] ).intValue();
      nRows = new Integer( data[1] ).intValue();
      double originX = new Double( data[2] ).doubleValue();
      double originY = new Double( data[3] ).doubleValue();
      //double originZ = 0;
      origin = GeometryFactory.createGM_Point( originX, originY, cs );
      offset[0] = ( new Double( data[4] ) ).doubleValue();
      offset[1] = ( new Double( data[4] ) ).doubleValue();
      //offset[2] = 0;
      nodata = data[5];
      while( ( line = br.readLine() ) != null )
      {
        //rangeData.append(line);
        String[] dataAsString = line.split( " " );
        //System.out.println("...");
        for( int i = 0; i < dataAsString.length; i++ )
        {
          rangeData.addElement( dataAsString[i] );
        }
      }
    }
    catch( Exception e )
    {
      System.out.println( e );
    }
    double[] low =
    {
        0.0,
        0.0 };
    double[] high =
    {
        nCols,
        nRows };
    GridRange gridRange = new GridRange_Impl( low, high );
    RectifiedGridDomain gridDomain = new RectifiedGridDomain( origin, offset, gridRange );
    //String[] dataAsString = rangeData.toString().split(" ");
    for( int i = 0; i < nRows; i++ )
    {
      Vector rowData = new Vector();
      for( int n = 0; n < nCols; n++ )
      {
        if( rangeData.get( n + ( i * nCols ) ).equals( nodata ) )
        {
          rowData.addElement( null );
        }
        else
        {
          double actualValue = Double.parseDouble( (String)rangeData.get( n + ( i * nCols ) ) );
          rowData.addElement( new Double( actualValue ) );
        }
      }
      //System.out.println(rowData);
      //System.out.println(i+" of "+nRows+"calculated");
      rangeSetData.addElement( rowData );
    }
    RangeSet rangeSet = new RangeSet( rangeSetData, null );
    RectifiedGridCoverage grid = new RectifiedGridCoverage( gridDomain, rangeSet );
    return grid;
  }

  /**
   * @see org.kalypso.floodrisk.data.RasterDataModel#getRectifiedGridCoverage(URL)
   * 
   * @param rasterDataModelGML
   * @return RectifiedGridCoverage
   * @throws Exception
   *  
   */
  public static RectifiedGridCoverage readRasterData( File rasterDataModelGML ) throws Exception
  {
    return rasterDataModel.getRectifiedGridCoverage( rasterDataModelGML.toURL() );
  }

  /**
   * @see org.kalypso.floodrisk.data.RasterDataModel#toFile(File, RectifiedGridCoverage)
   * @param rasterDataModelGML
   * @param grid
   * @throws Exception
   *  
   */
  public static void writeRasterData( File rasterDataModelGML, RectifiedGridCoverage grid ) throws Exception
  {
    rasterDataModel.toFile( rasterDataModelGML, grid );
  }

}