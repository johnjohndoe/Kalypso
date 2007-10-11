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

package org.kalypsodeegree_impl.io.shpapi;

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;

import org.kalypsodeegree.model.geometry.ByteUtils;

/**
 * Class representing an ESRI Shape File.
 * <p>
 * Uses class ByteUtils modified from the original package com.bbn.openmap.layer.shape <br>
 * Copyright (C) 1998 BBN Corporation 10 Moulton St. Cambridge, MA 02138 <br>
 * <P>
 * <B>Last changes <B>: <BR>
 * 14.12.1999 ap: import clauses added <BR>
 * 14.12.1999 ap: private variable declarations <BR>
 * 14.12.1999 ap: all public static references removed <BR>
 * 14.12.1999 ap: constructor implemented <BR>
 * 14.12.1999 ap: method: openFiles (String url) implemented <BR>
 * 14.12.1999 ap: method: getFileMBR() implemented <BR>
 * 14.12.1999 ap: method: getRecordNum() implemented <BR>
 * 14.12.1999 ap: method: getRecordMBR(int RecNo) implemented <BR>
 * 14.12.1999 ap: method: getByRecNo(int RecNo) implemented <BR>
 * 14.12.1999 ap: method: getShapeTypeByRecNo(int RecNo) implemented <BR>
 * 21.12.1999 ap: method: openfiles(String url) removed <BR>
 * 21.12.1999 ap: all static final declarations replaced <BR>
 * 07.01.2000 ap: return types of the methods changed from WKBxxxx and Rectangle <BR>
 * to SHPxxxx <BR>
 * 07.01.2000 ap: method getRecordMBR modified - SHAPE_TYPE_MULTIPOINT added <BR>
 * 13.01.2000 ap: method getByRecNo re-implemented <BR>
 * 21.03.2000 ap: method getByRecNo completed; multipoint is now supported <BR>
 * 16.08.2000 ap: method write(..) added <BR>
 * 16.08.2000 ap: method writeHeader(..) added <BR>
 * <!---------------------------------------------------------------------------->
 * 
 * @version 16.08.2000
 * @author Andreas Poth
 */

public class MainFile
{

  /*
   * A buffer for current record's header.
   */
  protected byte[] recHdr = new byte[ShapeConst.SHAPE_FILE_RECORD_HEADER_LENGTH];

  /*
   * instance variables
   */
  private final FileHeader sfh;

  private final IndexFile shx;

  /*
   * file suffixes for shp
   */
  private final String _shp = ".shp";

  /*
   * references to the main file
   */
  private final RandomAccessFile rafShp;

  /**
   * Construct a MainFile from a file name.
   */
  public MainFile( String url ) throws IOException
  {

    /*
     * creates rafShp
     */
    rafShp = new RandomAccessFile( url + _shp, "r" );

    sfh = new FileHeader( rafShp );

    shx = new IndexFile( url );

  }

  /**
   * Construct a MainFile from a file name.
   */
  public MainFile( String url, String rwflag ) throws IOException
  {

    // delet file if it exists
    File file = new File( url + _shp );

    if( rwflag.indexOf( 'w' ) > -1 && file.exists() )
      file.delete();
    file = null;

    /*
     * creates rafShp
     */
    rafShp = new RandomAccessFile( url + _shp, rwflag );

    sfh = new FileHeader( rafShp );

    shx = new IndexFile( url, rwflag );

  }

  public void close( ) throws IOException
  {
    rafShp.close();
    shx.close();
  }

  /**
   * method: getFileMBR() <BR>
   * returns the minimum bounding rectangle of geometries <BR>
   * within the shape-file
   */
  public SHPEnvelope getFileMBR( )
  {

    return sfh.getFileMBR();

  }

  /**
   * method: getRecordNum() <BR>
   * returns the number of record with in a shape-file <BR>
   */
  public int getRecordNum( )
  {

    return shx.getRecordNum();

  }

  /**
   * method: getRecordMBR(int RecNo) <BR>
   * returns the minimum bound rectangle of RecNo's Geometrie of the shape-file <BR>
   */
  public SHPEnvelope getRecordMBR( int RecNo ) throws IOException
  {

    SHPEnvelope recordMBR = null;
    byte[] recBuf = null;

    // index in IndexArray (see IndexFile)
    int iaIndex = RecNo - 1;

    int off = shx.getRecordOffset( iaIndex );

    // calculate length from 16-bit words (= 2 bytes) to lenght in bytes
    int len = shx.getRecordLength( iaIndex ) * 2;

    // off holds the offset of the shape-record in 16-bit words (= 2 byte)
    // multiply with 2 gets number of bytes to seek
    long rafPos = off * 2;

    // fetch shape record
    rafShp.seek( rafPos + ShapeConst.SHAPE_FILE_RECORD_HEADER_LENGTH );

    recBuf = null;
    recBuf = new byte[len];

    if( rafShp.read( recBuf, 0, len ) != -1 )
    {

      int shpType = ByteUtils.readLEInt( recBuf, 0 );

      /*
       * only for PolyLines, Polygons and MultiPoints minimum bounding rectangles are defined
       */
      if( (shpType == ShapeConst.SHAPE_TYPE_POLYLINE) || (shpType == ShapeConst.SHAPE_TYPE_POLYGON) || (shpType == ShapeConst.SHAPE_TYPE_MULTIPOINT) || (shpType == ShapeConst.SHAPE_TYPE_POLYLINEZ)
          || (shpType == ShapeConst.SHAPE_TYPE_POLYGONZ) || (shpType == ShapeConst.SHAPE_TYPE_MULTIPOINTZ) )
      {

        recordMBR = new SHPEnvelope( recBuf );

      } // end if shpType

    } // end if result

    return recordMBR;
  }

  /**
   * method: getByRecNo (int RecNo) <BR>
   * retruns a ShapeRecord-Geometry by RecorcNumber <BR>
   */
  public ISHPGeometry getByRecNo( int RecNo ) throws IOException
  {

    ISHPGeometry shpGeom = null;
    byte[] recBuf = null;

    // index in IndexArray (see IndexFile)
    int iaIndex = RecNo - 1;

    int off = shx.getRecordOffset( iaIndex );

    // calculate length from 16-bit words (= 2 bytes) to lenght in bytes
    int len = shx.getRecordLength( iaIndex ) * 2;

    // off holds the offset of the shape-record in 16-bit words (= 2 byte)
    // multiply with 2 gets number of bytes to seek
    long rafPos = off * 2;

    // fetch record header
    rafShp.seek( rafPos );

    recBuf = null;
    recBuf = new byte[ShapeConst.SHAPE_FILE_RECORD_HEADER_LENGTH];

    // fetch shape record
    rafShp.seek( rafPos + ShapeConst.SHAPE_FILE_RECORD_HEADER_LENGTH );

    recBuf = null;
    recBuf = new byte[len];

    if( rafShp.read( recBuf, 0, len ) != -1 )
    {

      int shpType = ByteUtils.readLEInt( recBuf, 0 );

      // create a geometry out of record buffer with shapetype
      if( shpType == ShapeConst.SHAPE_TYPE_POINT )
      {

        SHPPoint shppoint = new SHPPoint( recBuf, 4 );
        shpGeom = shppoint;

      }
      else if( shpType == ShapeConst.SHAPE_TYPE_MULTIPOINT )
      {

        SHPMultiPoint shpmultipoint = new SHPMultiPoint( recBuf );
        shpGeom = shpmultipoint;

      }
      else if( shpType == ShapeConst.SHAPE_TYPE_POLYLINE )
      {
        SHPPolyLine shppolyline = new SHPPolyLine( recBuf );
        shpGeom = shppolyline;
      }
      else if( shpType == ShapeConst.SHAPE_TYPE_POLYGON )
      {
        SHPPolygon shppolygon = new SHPPolygon( recBuf );
        shpGeom = shppolygon;
      }
      if( shpType == ShapeConst.SHAPE_TYPE_POINTZ )
      {
        SHPPointz shppointz = new SHPPointz( recBuf, 4, ShapeConst.SHAPE_TYPE_POINTZ );
        shpGeom = shppointz;
      }
      else if( shpType == ShapeConst.SHAPE_TYPE_POLYLINEZ )
      {
        SHPPolyLinez shppolyline = new SHPPolyLinez( recBuf );
        shpGeom = shppolyline;
      }
      else if( shpType == ShapeConst.SHAPE_TYPE_POLYGONZ )
      {
        SHPPolygonz shppolygonz = new SHPPolygonz( recBuf );
        shpGeom = shppolygonz;
      }
      else if( shpType == ShapeConst.SHAPE_TYPE_MULTIPOINTZ )
      {
        SHPMultiPointz shpmultipointZ = new SHPMultiPointz( recBuf );
        shpGeom = shpmultipointZ;
      }

    } // end if result

    return shpGeom;

  }

  public int getFileShapeType( )
  {
    return sfh.getFileShapeType();
  }

  /**
   * returns the type of the RecNo'th Geometrie <BR>
   * per definition a shape file contains onlay one shape type <BR>
   * but null shapes are possible too! <BR>
   */
  public int getShapeTypeByRecNo( int RecNo ) throws IOException
  {

    byte[] recBuf = null;
    int shpType = -1;

    // index in IndexArray (see IndexFile)
    int iaIndex = RecNo - 1;

    int off = shx.getRecordOffset( iaIndex );
    if( off == -1 )
      return -1;

    // calculate length from 16-bit words (= 2 bytes) to lenght in bytes
    int len = shx.getRecordLength( iaIndex ) * 2;

    // off holds the offset of the shape-record in 16-bit words (= 2 byte)
    // multiply with 2 gets number of bytes to seek
    long rafPos = off * 2;

    // fetch shape record
    rafShp.seek( rafPos + ShapeConst.SHAPE_FILE_RECORD_HEADER_LENGTH );

    recBuf = null;
    recBuf = new byte[len];

    if( rafShp.read( recBuf, 0, len ) != -1 )// ck: && recBuf.length != 0)
    {

      shpType = ByteUtils.readLEInt( recBuf, 0 );

    } // end if result

    return shpType;
  }

  /**
   * method: public void write(byte[] bytearray) <BR>
   * appends a bytearray to the shape file <BR>
   */
  public void write( byte[] bytearray, IndexRecord record, SHPEnvelope mbr ) throws IOException
  {
    rafShp.seek( record.offset * 2 );
    rafShp.write( bytearray );
    shx.appendRecord( record, mbr );
  }

  /**
   * method: public void writeHeader(int filelength, byte shptype, SHPEnvelope mbr) <BR>
   * writes a header to the shape and index file <BR>
   */
  public void writeHeader( int filelength, byte shptype, SHPEnvelope mbr ) throws IOException
  {
    sfh.writeHeader( filelength, shptype, mbr );
    shx.writeHeader( shptype, mbr );
  }

}