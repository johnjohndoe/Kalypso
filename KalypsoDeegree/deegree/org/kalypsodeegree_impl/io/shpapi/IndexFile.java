/*----------------    FILE HEADER  ------------------------------------------

 This file is part of deegree.
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon Fitzke/Fretter/Poth GbR
 http://www.lat-lon.de

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

 Andreas Poth
 lat/lon Fitzke/Fretter/Poth GbR
 Meckenheimer Allee 176
 53115 Bonn
 Germany
 E-Mail: poth@lat-lon.de

 Jens Fitzke
 Department of Geography
 University of Bonn
 Meckenheimer Allee 166
 53115 Bonn
 Germany
 E-Mail: jens.fitzke@uni-bonn.de

 
 ---------------------------------------------------------------------------*/

package org.deegree_impl.io.shpapi;

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.ArrayList;

import org.deegree.model.geometry.ByteUtils;

/**
 * Class representing an ESRI Shape File.
 * <p>
 * Uses class ShapeUtils modified from the original package
 * com.bbn.openmap.layer.shape <br>
 * Copyright (C) 1998 BBN Corporation 10 Moulton St. Cambridge, MA 02138 <br>
 * 
 * <P>
 * <B>Last changes <B>: <BR>
 * 17.12.1999 ap: import clauses added <BR>
 * 31.07.2000 ap: method writeIndexFileHeader(SHPEnvelope mbr) added <BR>
 * 31.07.2000 ap: method appendRecord(IndexRecord record, SHPEnvelope mbr) added
 * <BR>
 * 
 * <p>
 * -------------------------------------------------------------------------
 * </p>
 * 
 * @version 31.07.2000
 * @author Andreas Poth
 *         <p>
 */

public class IndexFile
{

  private final String _shx = ".shx";

  private RandomAccessFile rafShx;

  /**
   * The length of an index record. (8 byte)
   */
  private static final int INDEX_RECORD_LENGTH = 8;

  /**
   * array which holds the content of .shx-file:
   */
  private IndexRecord[] indexArray = null;

  /**
   * IndexFileHeader is equal to ShapeFileHeader
   */
  private FileHeader ifh;

  /**
   * minimum bounding rectangle of the shape-file
   */
  private SHPEnvelope fileMBR;

  /**
   * number of Records in .shp, .shx., .dbf has to be identical
   */
  private int RecordNum;

  /**
   * file position offset
   */
  private long offset;

  /**
   * length of the indexfile
   */
  private int filelength = 0;

  /**
   * Construct a IndexFile from a file name.
   */
  public IndexFile( String url ) throws IOException
  {

    /*
     * creates rafShx
     */
    rafShx = new RandomAccessFile( url + _shx, "r" );

    /*
     * construct Header as ShapeFileHeader
     */
    ifh = new FileHeader( rafShx );

    fileMBR = ifh.getFileMBR();

    /*
     * construct indexArray
     */
    setIndexArray();

  }

  /**
   * Construct a IndexFile from a file name.
   */
  public IndexFile( String url, String rwflag ) throws IOException
  {

    // delet file if it exists
    File file = new File( url + _shx );
    if( file.exists() )
      file.delete();
    file = null;

    //creates rafShx
    rafShx = new RandomAccessFile( url + _shx, rwflag );

    // if the file doesn't exists an empty header will be
    // written by FileHeader
    ifh = new FileHeader( rafShx );

    fileMBR = ifh.getFileMBR();

    offset = rafShx.length();

    if( offset < 100 )
      offset = ShapeConst.SHAPE_FILE_HEADER_LENGTH;

  }

  public void close()
  {
    try
    {
      rafShx.close();
    }
    catch( Exception ex )
    {
      ex.printStackTrace();
    }

  }

  /**
   * method: writeHeader(int filelength, byte shptype,SHPEnvelope mbr) <BR>
   * Writes a header into the index file. <BR>
   */
  public void writeHeader( int shptype, SHPEnvelope mbr ) throws IOException
  {

    byte[] header = new byte[ShapeConst.SHAPE_FILE_HEADER_LENGTH];

    ByteUtils.writeBEInt( header, 0, ShapeConst.SHAPE_FILE_CODE );
    ByteUtils.writeBEInt( header, 24, filelength );
    ByteUtils.writeLEInt( header, 28, ShapeConst.SHAPE_FILE_VERSION );
    ByteUtils.writeLEInt( header, 32, shptype );
    ShapeUtils.writeBox( header, 36, mbr );

    rafShx.seek( 0 );
    rafShx.write( header, 0, ShapeConst.SHAPE_FILE_HEADER_LENGTH );
  }

  /**
   * method: getFileMBR() <BR>
   * returns the minimum bounding rectangle of the shape-file <BR>
   */
  public SHPEnvelope getFileMBR()
  {

    return fileMBR;

  }

  /**
   * method: setIndexArray() <BR>
   * local constructor for local field indexArray <BR>
   */
  private void setIndexArray() throws IOException
  {

    byte[] recBuf = new byte[INDEX_RECORD_LENGTH];
    long rafPos = ShapeConst.SHAPE_FILE_HEADER_LENGTH;
    int iaIndex = 0;
    ArrayList indexArrayVector = new ArrayList( 10000 );

    rafShx.seek( rafPos );

    // loop over index records, until EOF
    while( rafShx.read( recBuf, 0, INDEX_RECORD_LENGTH ) != -1 )
    {

      IndexRecord ir = new IndexRecord( recBuf );

      // set ArrayVector item as index record
      indexArrayVector.add( ir );

      // array index adjustment
      ++iaIndex;

      // filepos adjustment
      rafPos = rafPos + INDEX_RECORD_LENGTH;
      rafShx.seek( rafPos );

    } // end of while

    // iaIndex holds Record Number
    RecordNum = iaIndex;

    // copy vector into indexArray
    indexArray = (IndexRecord[])indexArrayVector.toArray( new IndexRecord[RecordNum] );

  }

  /**
   * method: getIndexArray() <BR>
   * clones local field indexArray <BR>
   */
  public IndexRecord[] getIndexArray()
  {

    IndexRecord[] ia = null;
    ia = indexArray;

    return ia;

  }

  /**
   * method: getRecordNum() <BR>
   * function to get number of Records <BR>
   */
  public int getRecordNum()
  {

    return RecordNum;

  }

  /**
   * methode: getRecordOffset (int RecNo) <BR>
   * function to get Record offset by Record number <BR>
   */
  public int getRecordOffset( int RecNo )
  {

    if( RecNo >= 0 && RecNo < indexArray.length )
    {
      return indexArray[RecNo].offset;
    }
    else
    {
      return -1;
    }

  }

  /**
   * method: getRecordLength (int RecNo) <BR>
   * function to get Record Length by Record number <BR>
   */
  public int getRecordLength( int RecNo )
  {

    if( RecNo >= 0 && RecNo < indexArray.length )
    {
      return indexArray[RecNo].length;
    }
    else
    {
      return -1;
    }

  }

  /**
   * method: getIndexRecord (int RecNo) <BR>
   * function to get Index Record by Record number <BR>
   */
  public IndexRecord getIndexRecord( int RecNo )
  {

    IndexRecord ir = new IndexRecord();

    if( RecNo >= 0 )
    {

      return ir = indexArray[RecNo];

    }
    else
    {

      return ir;

    }

  }

  /**
   * appends an index record to the indexfile
   */
  public void appendRecord( IndexRecord record, SHPEnvelope mbr ) throws IOException
  {

    offset = rafShx.length();
    rafShx.seek( offset );
    rafShx.write( record.writeIndexRecord() );
    offset = offset + INDEX_RECORD_LENGTH;
    //actualize mbr
    if( fileMBR.west > mbr.west )
    {
      fileMBR.west = mbr.west;
    }
    if( fileMBR.east < mbr.east )
    {
      fileMBR.east = mbr.east;
    }
    if( fileMBR.south > mbr.south )
    {
      fileMBR.south = mbr.south;
    }
    if( fileMBR.north < mbr.north )
    {
      fileMBR.north = mbr.north;
    }
    rafShx.seek( 36 );
    rafShx.write( fileMBR.writeLESHPEnvelope() );

    //actualize file length
    filelength = (int)offset / 2;

  }

}