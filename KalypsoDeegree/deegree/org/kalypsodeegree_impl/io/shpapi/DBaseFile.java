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

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.Hashtable;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.model.geometry.ByteUtils;
import org.deegree_impl.model.feature.FeatureFactory;
import org.deegree_impl.tools.TimeTools;

/**
 * 
 * the datatypes of the dBase file and their representation as java types:
 * 
 * dBase-type dBase-type-ID java-type
 * 
 * character "C" String float "F" Float number "N" Double logical "L" String
 * memo "M" String date "D" Date binary "B" ByteArrayOutputStream
 * 
 * 
 * <!---------------------------------------------------------------------------->
 * 
 * @version 12.12.2000
 * @author Andreas Poth
 * @author Markus Müller, email: mm@giub.uni-bonn.de
 *  
 */
public class DBaseFile
{
  private ArrayList colHeader = new ArrayList();

  // representing the datasection of the dBase file
  // only needed for writing a dBase file
  private DBFDataSection dataSection = null;

  // feature type of the dbase table + a GM_Object as last field
  private FeatureType featureType = null;

  // Hashtable to contain info abouts in the table
  private Hashtable column_info = new Hashtable();

  // references to the dbase file
  private RandomAccessFile rafDbf;

  // file suffixes for dbf
  private final String _dbf = ".dbf";

  // represents the dBase file header
  // only needed for writing the dBase file
  private DBFHeader header = null;

  // representing the name of the dBase file
  // only needed for writing the dBase file
  private String fname = null;

  private String ftName = null;

  // end of file flag
  private boolean eof = false;

  // number of records in the table
  private double file_numrecs;

  // data start position, and length of the data
  private int file_datalength;

  // data start position, and length of the data
  private int file_datap;

  // flag wich indicates if a dBase file should be
  // read or writed.
  // filemode = 0 : read only
  // filemode = 1 : write only
  private int filemode = 0;

  // number of columns
  private int num_fields;

  // current record
  private long record_number = 0;

  // the file type (dbase, fox, etc) and the file update date values
  private short file_type;

  // the file type (dbase, fox, etc) and the file update date values
  private short file_update_day;

  // the file type (dbase, fox, etc) and the file update date values
  private short file_update_month;

  // the file type (dbase, fox, etc) and the file update date values
  private short file_update_year;

  // size of the cache used for reading data from the dbase table
  private long cacheSize = 1000000;

  // array containing the data of the cache
  private byte[] dataArray = null;

  // file position the caches starts
  private long startIndex = 0;
final int m_defaultFileShapeType;
  /**
   * constructor <BR>
   * only for reading a dBase file <BR>
   */
  public DBaseFile( String url,int defaultFileShapeType ) throws IOException
  {
    fname = url;
    m_defaultFileShapeType=defaultFileShapeType;
    //creates rafDbf
    rafDbf = new RandomAccessFile( url + _dbf, "r" );

    //dataArray = new byte[(int)rafDbf.length()];
    if( cacheSize > rafDbf.length() )
    {
      cacheSize = rafDbf.length();
    }

    dataArray = new byte[(int)cacheSize];
    rafDbf.read( dataArray );
    rafDbf.seek( 0 );

    //initialize dbase file
    initDBaseFile();

    filemode = 0;
  }

  /**
   * constructor <BR>
   * only for writing a dBase file <BR>
   */
  public DBaseFile( String url, FieldDescriptor[] fieldDesc ) throws DBaseException
  {
    m_defaultFileShapeType=-1;
    fname = url;

    // create header
    header = new DBFHeader( fieldDesc );

    // create data section
    dataSection = new DBFDataSection( fieldDesc );

    filemode = 1;
  }

  /**
   *  
   */
  public void close()
  {
    try
    {
      rafDbf.close();
    }
    catch( Exception ex )
    {}
  }

  /**
   * method: initDBaseFile(); inits a DBF file. This is based on Pratap
   * Pereira's Xbase.pm perl module
   *  
   */
  private void initDBaseFile() throws IOException
  {
    // position the record pointer at 0
    rafDbf.seek( 0 );

    // read the file type
    file_type = fixByte( rafDbf.readByte() );

    // get the last update date
    file_update_year = fixByte( rafDbf.readByte() );
    file_update_month = fixByte( rafDbf.readByte() );
    file_update_day = fixByte( rafDbf.readByte() );

    // a byte array to hold little-endian long data
    byte[] b = new byte[4];

    // read that baby in...
    rafDbf.readFully( b );

    // convert the byte array into a long (really a double)
    file_numrecs = ByteUtils.readLEInt( b, 0 );

    b = null;

    // a byte array to hold little-endian short data
    b = new byte[2];

    // get the data position (where it starts in the file)
    rafDbf.readFully( b );
    file_datap = ByteUtils.readLEShort( b, 0 );

    // find out the length of the data portion
    rafDbf.readFully( b );
    file_datalength = ByteUtils.readLEShort( b, 0 );

    // calculate the number of fields
    num_fields = (int)( file_datap - 33 ) / 32;

    // read in the column data
    int locn = 0; // offset of the current column

    // process each field
    for( int i = 1; i <= num_fields; i++ )
    {
      // seek the position of the field definition data.
      // This information appears after the first 32 byte
      // table information, and lives in 32 byte chunks.
      rafDbf.seek( ( ( i - 1 ) * 32 ) + 32 );

      b = null;

      // get the column name into a byte array
      b = new byte[11];
      rafDbf.readFully( b );

      // convert the byte array to a String
      String col_name = new String( b ).trim().toUpperCase();

      // read in the column type
      char[] c = new char[1];
      c[0] = (char)rafDbf.readByte();

      String ftyp = new String( c );

      // skip four bytes
      rafDbf.skipBytes( 4 );

      // get field length and precision
      short flen = fixByte( rafDbf.readByte() );
      short fdec = fixByte( rafDbf.readByte() );
      //System.out.println(col_name + " len: " + flen + " dec: " + fdec);
      // set the field position to the current
      // value of locn
      int fpos = locn;

      // increment locn by the length of this field.
      locn += flen;

      // create a new dbfCol object and assign it the
      // attributes of the current field
      dbfCol column = new dbfCol( col_name );
      column.type = new String( c );
      column.size = flen;
      column.position = fpos + 1;
      column.prec = fdec;

      // to be done: get the name of dbf-table via method in ShapeFile
      column.table = "NOT";

      column_info.put( col_name, column );
      colHeader.add( col_name );
    } // end for

    featureType = createFeatureType();
  } // end of initDBaseFile

  /**
   * 
   * 
   * @return
   */
  private FeatureType createFeatureType()
  {
    dbfCol column = null;

    FeatureTypeProperty[] ftp = new FeatureTypeProperty[colHeader.size() + 1];

    for( int i = 0; i < colHeader.size(); i++ )
    {
      // retrieve the dbfCol object which corresponds
      // to this column.
      column = (dbfCol)column_info.get( colHeader.get( i ) );

      if( column.type.equalsIgnoreCase( "C" ) )
      {
        ftp[i] = FeatureFactory.createFeatureTypeProperty( column.name, "java.lang.String", true );
      }
      else if( column.type.equalsIgnoreCase( "F" ) || column.type.equalsIgnoreCase( "N" ) )
      {
        if( column.prec == 0 )
        {
          if( column.size < 10 )
          {
            ftp[i] = FeatureFactory.createFeatureTypeProperty( column.name, "java.lang.Integer",
                true );
          }
          else
          {
            ftp[i] = FeatureFactory.createFeatureTypeProperty( column.name, "java.lang.Long", true );
          }
        }
        else
        {
          if( column.size < 8 )
          {
            ftp[i] = FeatureFactory
                .createFeatureTypeProperty( column.name, "java.lang.Float", true );
          }
          else
          {
            ftp[i] = FeatureFactory.createFeatureTypeProperty( column.name, "java.lang.Double",
                true );
          }
        }
      }
      else if( column.type.equalsIgnoreCase( "M" ) )
      {
        ftp[i] = FeatureFactory.createFeatureTypeProperty( column.name, "java.lang.String", true );
      }
      else if( column.type.equalsIgnoreCase( "L" ) )
      {
        ftp[i] = FeatureFactory.createFeatureTypeProperty( column.name, "java.lang.String", true );
      }
      else if( column.type.equalsIgnoreCase( "D" ) )
      {
        ftp[i] = FeatureFactory.createFeatureTypeProperty( column.name, "java.util.Date", true );
      }
      else if( column.type.equalsIgnoreCase( "B" ) )
      {
        ftp[i] = FeatureFactory.createFeatureTypeProperty( column.name,
            "java.io.ByteArrayOutputStream", true );
      }

    }

    int index = fname.lastIndexOf( "/" );
    ftName = fname;

    if( index >= 0 )
    {
      ftName = fname.substring( index + 1 );
    }

    ftp[ftp.length - 1] = FeatureFactory
    .createFeatureTypeProperty( "GEOM",getGeometryType(), true );
    return FeatureFactory.createFeatureType( null, null, ftName, ftp );
  }

  private String getGeometryType()
  {
    switch( m_defaultFileShapeType )
    {
    case ShapeConst.SHAPE_TYPE_POINT:
      return "org.deegree.model.geometry.GM_Point";
    case ShapeConst.SHAPE_TYPE_MULTIPOINT:
      return "org.deegree.model.geometry.GM_MultiPoint";
    case ShapeConst.SHAPE_TYPE_POLYLINE:
      return "org.deegree.model.geometry.GM_LineString";
    case ShapeConst.SHAPE_TYPE_POLYGON:
      return "org.deegree.model.geometry.GM_Polygon";
    default:
      return "org.deegree.model.geometry.GM_Object";
    }
  }

  /**
   * method: getRecordNum() <BR>
   * Get the number of records in the table
   */
  public int getRecordNum() throws DBaseException
  {
    if( filemode == 1 )
    {
      throw new DBaseException( "class is initialized in write-only mode" );
    }

    return (int)file_numrecs;
  }

  /**
   * method: goTop() <BR>
   * Position the record pointer at the top of the table.
   */
  public void goTop() throws DBaseException
  {
    if( filemode == 1 )
    {
      throw new DBaseException( "class is initialized in write-only mode" );
    }

    record_number = 0;
    eof = false;
  }

  /**
   * method: nextRecord() <BR>
   * Advance the record pointer to the next record.
   */
  public boolean nextRecord() throws DBaseException
  {
    if( filemode == 1 )
    {
      throw new DBaseException( "class is initialized in write-only mode" );
    }

    if( record_number < file_numrecs )
    {
      record_number++;
      eof = false;
      return true;
    }
    else
    {
      eof = true;
      return false;
    }
  }

  /**
   * method: getColumn(String col_name) <BR>
   * Retrieve a column's string value from the current row.
   */
  public String getColumn( String col_name ) throws DBaseException
  {
    if( filemode == 1 )
    {
      throw new DBaseException( "class is initialized in write-only mode" );
    }

    try
    {
      // retrieve the dbfCol object which corresponds
      // to this column.
      dbfCol column = (dbfCol)column_info.get( col_name );

      // seek the starting offset of the current record,
      // as indicated by record_number
      long pos = file_datap + ( ( record_number - 1 ) * file_datalength );

      // read data from cache if the requested part of the dbase file is
      // within it
      if( ( pos >= startIndex )
          && ( ( pos + column.position + column.size ) < ( startIndex + cacheSize ) ) )
      {
        pos = pos - startIndex;
      }
      else
      {
        // actualize cache starting at the current cursor position
        // if neccesary correct cursor position
        rafDbf.seek( pos );
        rafDbf.read( dataArray );
        startIndex = pos;
        pos = 0;
      }
      StringBuffer sb = new StringBuffer( column.size );
      int i = 0;
      while( i < column.size )
      {
        int kk = (int)pos + column.position + i;
        /*
         * if ( dataArray[kk] == -127 ) { sb.append( 'ü' ); } else if (
         * dataArray[kk] == -108 ) { sb.append( 'ö' ); } else if ( dataArray[kk] ==
         * -124 ) { sb.append( 'ä' ); } else
         */if( dataArray[kk] != 32 )
        {
          sb.append( (char)dataArray[kk] );
        }
        i++;
      }

      // if it's the pseudo column _DELETED, return
      // the first character in it
      //            if (col_name.equals("_DELETED")) {
      //                return result.substring(0, 1);
      //            }
      return sb.toString();
    }
    catch( Exception e )
    {
      e.printStackTrace();
      return e.toString();
    }
  }

  /**
   * method: public String[] getProperties() <BR>
   * returns the properties (column headers) of the dBase-file <BR>
   */
  public String[] getProperties() throws DBaseException
  {
    if( filemode == 1 )
    {
      throw new DBaseException( "class is initialized in write-only mode" );
    }

    return (String[])colHeader.toArray( new String[colHeader.size()] );
  }

  /**
   * method: public String[] getDataTypes() <BR>
   * returns the datatype of each column of the database <BR>
   */
  public String[] getDataTypes() throws DBaseException
  {
    if( filemode == 1 )
    {
      throw new DBaseException( "class is initialized in write-only mode" );
    }

    String[] datatypes = new String[colHeader.size()];
    dbfCol column;

    for( int i = 0; i < colHeader.size(); i++ )
    {
      // retrieve the dbfCol object which corresponds
      // to this column.
      column = (dbfCol)column_info.get( colHeader.get( i ) );

      datatypes[i] = column.type.trim();
    }

    return datatypes;
  }

  /**
   * method: private boolean contains(String[] container, String element) <BR>
   * retruns true if the container sting array contains element <BR>
   */
  private boolean contains( String[] container, String element )
  {
    for( int i = 0; i < container.length; i++ )

      if( container[i].equals( element ) )
      {
        return true;
      }

    return false;
  }

  /**
   * returns the size of a column
   */
  public int getDataLength( String field ) throws DBaseException
  {
    dbfCol col = (dbfCol)column_info.get( field );
    if( col == null )
      throw new DBaseException( "Field " + field + " not found" );

    return col.size;
  }

  /**
   * method: public String[] getDataTypes(String[] fields) <BR>
   * returns the datatype of each column of the database specified by fields
   * <BR>
   */
  public String[] getDataTypes( String[] fields ) throws DBaseException
  {
    if( filemode == 1 )
    {
      throw new DBaseException( "class is initialized in write-only mode" );
    }

    String[] datatypes = null;
    ArrayList vec = new ArrayList();
    dbfCol column;

    for( int i = 0; i < colHeader.size(); i++ )
    {
      // check if the current (i'th) column (string) is
      // within the array of specified columns
      if( contains( fields, (String)colHeader.get( i ) ) )
      {
        // retrieve the dbfCol object which corresponds
        // to this column.
        column = (dbfCol)column_info.get( colHeader.get( i ) );

        vec.add( (Object)column.type.trim() );
      }
    }

    return (String[])vec.toArray( new String[vec.size()] );
  }

  /**
   * returns a row of the dBase-file as Feature containing a place holder (field
   * name = "GEOM") for a geometry.
   */
  public Feature getFRow( int rowNo ) throws DBaseException
  {

    goTop();

    record_number += rowNo;

    Object[] fp = new Object[colHeader.size() + 1];

    for( int i = 0; i < colHeader.size(); i++ )
    {
      // retrieve the dbfCol object which corresponds
      // to this column.
      dbfCol column = (dbfCol)column_info.get( colHeader.get( i ) );

      String value = getColumn( column.name.trim() );

      if( value != null )
      {
        // cast the value of the i'th column to corresponding datatype
        if( column.type.equalsIgnoreCase( "C" ) )
        {
          fp[i] = value;
        }
        else if( column.type.equalsIgnoreCase( "F" ) || column.type.equalsIgnoreCase( "N" ) )
        {
          try
          {
            if( column.prec == 0 )
            {
              if( column.size < 10 )
              {
                fp[i] = new Integer( value );
              }
              else
              {
                fp[i] = new Long( value );
              }
            }
            else
            {
              if( column.size < 8 )
              {
                fp[i] = new Float( value );
              }
              else
              {
                fp[i] = new Double( value );
              }
            }
          }
          catch( Exception ex )
          {
            fp[i] = new Double( "0" );
          }
        }
        else if( column.type.equalsIgnoreCase( "M" ) )
        {
          fp[i] = value;
        }
        else if( column.type.equalsIgnoreCase( "L" ) )
        {
          fp[i] = value;
        }
        else if( column.type.equalsIgnoreCase( "D" ) )
        {
          if( value.equals( "" ) )
            fp[i] = null;
          else
            fp[i] = TimeTools.createCalendar(
                value.substring( 0, 4 ) + "-" + value.substring( 4, 6 ) + "-"
                    + value.substring( 6, 8 ) ).getTime();
        }
        else if( column.type.equalsIgnoreCase( "B" ) )
        {
          ByteArrayOutputStream os = new ByteArrayOutputStream( 10000 );
          try
          {
            os.write( value.getBytes() );
          }
          catch( IOException e )
          {
            e.printStackTrace();
          }

          fp[i] = os;
        }
      }
      else
      {
        fp[i] = "";
      }
    }

    return FeatureFactory.createFeature( ftName + rowNo, featureType, fp );
  }

  /**
   * method: public ArrayList getRow(int row) <BR>
   * returns a row of the dBase-file <BR>
   */
  public Object[] getRow( int rowNo ) throws DBaseException
  {
    if( filemode == 1 )
    {
      throw new DBaseException( "class is initialized in write-only mode" );
    }

    String value = "";
    Object[] row = new Object[colHeader.size()];
    dbfCol column;

    goTop();

    record_number += rowNo;

    for( int i = 0; i < colHeader.size(); i++ )
    {
      // retrieve the dbfCol object which corresponds
      // to this column.
      column = (dbfCol)column_info.get( colHeader.get( i ) );

      value = getColumn( column.name.trim() );

      // cast the value of the i'th column to corresponding datatype
      if( column.type.equalsIgnoreCase( "C" ) )
      {
        row[i] = value;
      }
      else if( column.type.equalsIgnoreCase( "F" ) || column.type.equalsIgnoreCase( "N" ) )
      {
        try
        {
          if( column.prec == 0 )
          {
            if( column.size < 10 )
            {
              row[i] = new Integer( value );
            }
            else
            {
              row[i] = new Long( value );
            }
          }
          else
          {
            if( column.size < 8 )
            {
              row[i] = new Float( value );
            }
            else
            {
              row[i] = new Double( value );
            }
          }
        }
        catch( Exception ex )
        {
          row[i] = new Double( "0" );
        }
      }
      else if( column.type.equalsIgnoreCase( "M" ) )
      {
        row[i] = value;
      }
      else if( column.type.equalsIgnoreCase( "L" ) )
      {
        row[i] = value;
      }
      else if( column.type.equalsIgnoreCase( "D" ) )
      {
        if( value.equals( "" ) )
          row[i] = "";
        else
          row[i] = TimeTools.createCalendar(
              value.substring( 0, 4 ) + "-" + value.substring( 4, 6 ) + "-"
                  + value.substring( 6, 8 ) ).getTime();
      }
      else if( column.type.equalsIgnoreCase( "B" ) )
      {
        ByteArrayOutputStream os = new ByteArrayOutputStream( 10000 );

        try
        {
          os.write( value.getBytes() );
        }
        catch( IOException e )
        {
          System.out.println( e.toString() );
        }

        row[i] = os;
      }
    }

    return row;
  }

  /**
   * method: private fixByte (byte b)<BR>
   * bytes are signed; let's fix them...
   */
  private static short fixByte( byte b )
  {
    if( b < 0 )
    {
      return (short)( b + 256 );
    }

    return b;
  }

  /**
   * method: public void writeAllToFile() creates the dbase file and writes all
   * data to it if the file specified by fname (s.o.) exists it will be deleted!
   */
  public void writeAllToFile() throws IOException, DBaseException
  {
    if( filemode == 0 )
    {
      throw new DBaseException( "class is initialized in read-only mode" );
    }

    // if a file with the retrieved filename exists, delete it!
    File file = new File( fname + ".dbf" );

    if( file.exists() )
    {
      file.delete();
    }

    file = null;

    // create a new file
    RandomAccessFile rdbf = new RandomAccessFile( fname + ".dbf", "rw" );

    byte[] b = header.getHeader();

    int nRecords = dataSection.getNoOfRecords();

    //write number of records
    ByteUtils.writeLEInt( b, 4, nRecords );

    // write header to the file
    rdbf.write( b );

    b = null;

    b = dataSection.getDataSection();

    // write datasection to the file
    rdbf.write( b );

    rdbf.close();
  }

  /**
   * method: public setRecord(ArrayList recData) writes a data record to byte
   * array representing the data section of the dBase file. The method gets the
   * data type of each field in recData from fieldDesc wich has been set at the
   * constructor.
   */
  public void setRecord( ArrayList recData ) throws DBaseException
  {
    if( filemode == 0 )
    {
      throw new DBaseException( "class is initialized in read-only mode" );
    }

    dataSection.setRecord( recData );
  }

  /**
   * method: public setRecord(int index, ArrayList recData) writes a data record
   * to byte array representing the data section of the dBase file. The method
   * gets the data type of each field in recData from fieldDesc wich has been
   * set at the constructor. index specifies the location of the retrieved
   * record in the datasection. if an invalid index is used an exception will be
   * thrown
   */
  public void setRecord( int index, ArrayList recData ) throws DBaseException
  {
    if( filemode == 0 )
    {
      throw new DBaseException( "class is initialized in read-only mode" );
    }

    dataSection.setRecord( index, recData );
  }
} // end of class DBaseFile

/**
 * 
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */

class tsColumn
{
  public String name = null; // the column's name

  public String table = null; // the table which "owns" the column

  public String type = null; // the column's type

  public int prec = 0; // the column's precision

  public int size = 0; // the column's size

  /**
   * 
   * Constructs a tsColumn object.
   * 
   * @param s
   *          the column name
   */
  tsColumn( String s )
  {
    name = s;
  }
} // end of class tsColumn

/**
 * 
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */

class dbfCol extends tsColumn
{
  int position = 0;

  /**
   * Creates a new dbfCol object.
   * 
   * @param c
   */
  public dbfCol( String c )
  {
    super( c );
  }
} // end of class dbfCol
