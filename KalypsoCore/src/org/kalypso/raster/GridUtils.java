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
package org.kalypso.raster;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;

import ogc31.www.opengis.net.gml.FileType;
import ogc31.www.opengis.net.gml.RangeSetType;

import org.apache.commons.io.IOUtils;
import org.kalypso.contribs.ogc31.KalypsoOGC31JAXBcontext;
import org.kalypsodeegree.model.coverage.GridRange;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.cv.GridRange_Impl;
import org.kalypsodeegree_impl.model.cv.RectifiedGridCoverage2;
import org.kalypsodeegree_impl.model.cv.RectifiedGridDomain;
import org.kalypsodeegree_impl.model.cv.RectifiedGridDomain.OffsetVector;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * Class for reading and writing ascii-Grid files
 * 
 * @author N. Peiler
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
  public static void exportGridArc( final File out, final RectifiedGridCoverage2 grid ) throws Exception
  {
    // FIXME: Dejan: use RectifiedGridCoverage2 (File)
    // instead of RectifiedGridCoverage (Vector<Vector>Double>>)

    /*
     * BufferedWriter bw = new BufferedWriter( new FileWriter( out ) ); RectifiedGridDomain gridDomain =
     * grid.getGridDomain(); RangeSet rangeSet = grid.getRangeSet(); int nCols = gridDomain.getNumColumns(); int nRows =
     * gridDomain.getNumRows(); GM_Point origin = gridDomain.getOrigin( null ); double originX = origin.getX(); double
     * originY = origin.getY(); double offsetX = gridDomain.getOffsetX( origin.getCoordinateSystem() ); int nodata =
     * -9999; Vector rangeSetData = rangeSet.getRangeSetData(); try { bw.write( "ncols " + nCols + "\nnrows " + nRows +
     * "\nxllcorner " + originX + "\nyllcorner " + originY //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ +
     * "\ncellsize " + offsetX + "\nnodata_value " + nodata ); //$NON-NLS-1$ //$NON-NLS-2$ bw.newLine(); for( int i = 0;
     * i < rangeSetData.size(); i++ ) { Vector rowData = (Vector) rangeSetData.get( i ); for( int j = 0; j <
     * rowData.size(); j++ ) { if( rowData.get( j ) != null ) { double value = ((Double) rowData.get( j
     * )).doubleValue(); double roundValue = Number.round( value, 6, BigDecimal.ROUND_HALF_EVEN ); bw.write( roundValue + " " );
     * //$NON-NLS-1$ } else { bw.write( nodata + " " ); //$NON-NLS-1$ } System.out.println(i + "," + j + ": " +
     * rowData.get(j) + " "); }// for j bw.newLine(); }// for i bw.close(); }// try catch( IOException e ) { bw.write(
     * Messages.getString( "tools.GridUtils.ErrorWhileWritingAsciiArcFile" ) + ": " + e.getMessage() ); //$NON-NLS-1$
     * //$NON-NLS-2$ throw e; }// catch finally { bw.close(); if( out.length() == 0 ) out.delete(); }
     */
  }

  // /**
  // * imports an ascii-grid file
  // *
  // * @param in
  // * input file (*.asc)
  // * @param cs
  // * the coordinate system for the geometric data of the ascii-grid
  // * @return RectifiedGridCoverage
  // */
  // public static RectifiedGridCoverage importGridArc( File in, CS_CoordinateSystem cs ) throws Exception
  // {
  // BufferedReader br = new BufferedReader( new FileReader( in ) );
  // String[] data = new String[6];
  // String line;
  // // reading header data
  // for( int i = 0; i < 6; i++ )
  // {
  // line = br.readLine();
  // int index = line.indexOf( " " ); //$NON-NLS-1$
  // String subString = line.substring( index );
  // data[i] = subString.trim();
  // }
  // int nCols = new Integer( data[0] ).intValue();
  // int nRows = new Integer( data[1] ).intValue();
  // double originX = new Double( data[2] ).doubleValue();
  // double originY = new Double( data[3] ).doubleValue();
  // //double originZ = 0;
  // GM_Point origin = GeometryFactory.createGM_Point( originX, originY, cs );
  // double[] offset = new double[2];
  // offset[0] = ( new Double( data[4] ) ).doubleValue();
  // offset[1] = ( new Double( data[4] ) ).doubleValue();
  // //offset[2] = 0;
  // Vector<Vector<Double>> rangeDomain = new Vector<Vector<Double>>(nRows);
  // Vector<Double> rangeRow = new Vector<Double>(nCols);
  // //double[][] rangeData = new double[nRows][nCols];
  // Double singleValue;
  // Double nodataValue = new Double( data[5] );
  // //double nodataDoublevalue = Double.parseDouble(data[5]);
  // while( ( line = br.readLine() ) != null )
  // {
  // String[] dataAsString = line.split( " " ); //$NON-NLS-1$
  // for( int i = 0; i < dataAsString.length; i++ )
  // {
  // singleValue = new Double(dataAsString[i]);
  // //rangeData[lineCnt][i] = (singleValue != nodataDoublevalue) ? singleValue : RangeSet.NaN;
  // rangeRow.add( nodataValue.equals(singleValue)?null:singleValue );
  // }
  // //lineCnt++;
  // rangeDomain.add(rangeRow);
  // }
  // double[] low = { 0.0, 0.0 };
  // double[] high = { nCols, nRows };
  // GridRange gridRange = new GridRange_Impl( low, high );
  // RectifiedGridDomain gridDomain = new RectifiedGridDomain( origin, offset, gridRange );
  // RangeSet rangeSet = new RangeSet( rangeDomain, null );
  // //RangeSet rangeSet = new RangeSet( rangeData, null);
  // RectifiedGridCoverage grid = new RectifiedGridCoverage( gridDomain, rangeSet );
  // return grid;
  // }

  /**
   * imports an ascii-grid file
   * 
   * @param in
   *          input file (*.asc)
   * @param cs
   *          the coordinate system for the geometric data of the ascii-grid
   * @return RectifiedGridCoverage
   */
  public static RectifiedGridCoverage2 importGridArc( final File in, final CS_CoordinateSystem cs ) throws Exception
  {
    BufferedReader br = null;
    try
    {
      br = new BufferedReader( new FileReader( in ) );
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
      br.close();

      final int nCols = new Integer( data[0] ).intValue();
      final int nRows = new Integer( data[1] ).intValue();

      final OffsetVector offsetX = new OffsetVector( new Double( data[2] ), new Double( data[4] ) );
      final OffsetVector offsetY = new OffsetVector( new Double( data[4] ), new Double( data[3] ) );

      final GM_Point origin = GeometryFactory.createGM_Point( offsetX.getGeoX(), offsetY.getGeoY(), cs );

      final double[] low = { 0.0, 0.0 };
      final double[] high = { nCols, nRows };
      final GridRange gridRange = new GridRange_Impl( low, high );
      final RectifiedGridDomain gridDomain = new RectifiedGridDomain( origin, offsetX, offsetY, gridRange );

      /* RangeSet */
      final FileType rangeSetFile = KalypsoOGC31JAXBcontext.GML3_FAC.createFileType();
      // TODO: consider import the file into the eclipse workspace
      rangeSetFile.setFileName( in.toURL().toExternalForm() );
      // TODO: define a good mime type
      rangeSetFile.setMimeType( "text/asc" );
      // rangeSetFile.setFileStructure( FileValueModelType.RECORD_INTERLEAVED );

      final RangeSetType rangeSet = KalypsoOGC31JAXBcontext.GML3_FAC.createRangeSetType();
      rangeSet.setFile( rangeSetFile );
      // final RangeSet rangeSet = new RangeSet();
      // rangeSet.setRangeSetDataFile( rangeSetFile.getFileName() );

      return RectifiedGridCoverage2.createRectifiedGridCoverage( gridDomain, rangeSet );
    }
    finally
    {
      IOUtils.closeQuietly( br );
    }
  }

  /**
   * @see org.kalypso.floodrisk.data.RasterDataModel#getRectifiedGridCoverage(URL)
   * @param rasterDataModelGML
   * @return RectifiedGridCoverage
   * @throws Exception
   */
  public static RectifiedGridCoverage2 readRasterData( final File rasterDataModelGML ) throws Exception
  {
    return GridUtils.rasterDataModel.getRectifiedGridCoverage( rasterDataModelGML.toURL() );
  }

  /**
   * @param rasterDataModelGML
   * @param grid
   * @throws Exception
   */
  public static void writeRasterData( final File rasterDataModelGML, final RectifiedGridCoverage2 grid ) throws Exception
  {
    GridUtils.rasterDataModel.exportToGML( rasterDataModelGML, grid );
  }
}