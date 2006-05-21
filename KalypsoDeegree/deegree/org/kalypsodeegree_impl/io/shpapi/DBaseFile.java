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

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.Hashtable;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;
import org.kalypso.contribs.java.io.CharsetUtilities;
import org.kalypso.gmlschema.GMLSchemaFactory;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.ITypeHandler;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.ByteUtils;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;
import org.kalypsodeegree_impl.tools.TimeTools;

/**
 * the datatypes of the dBase file and their representation as java types: dBase-type dBase-type-ID java-type character
 * "C" String float "F" Float number "N" Double logical "L" String memo "M" String date "D" Date binary "B"
 * ByteArrayOutputStream <!---------------------------------------------------------------------------->
 * 
 * @version 12.12.2000
 * @author Andreas Poth
 * @author Markus M?ller, email: mm@giub.uni-bonn.de
 */
public class DBaseFile
{

  public IFeatureType getFeatureType( )
  {
    return m_featureType;
  }

  private static String NS_SHAPEFILE = "namespace";

  private ArrayList<String> colHeader = new ArrayList<String>();

  // representing the datasection of the dBase file
  // only needed for writing a dBase file
  private DBFDataSection dataSection = null;

  // feature type of the dbase table + a GM_Object as last field
  private IFeatureType m_featureType = null;

  // Hashtable to contain info abouts in the table
  private Hashtable<String, dbfCol> column_info = new Hashtable<String, dbfCol>();

  // references to the dbase file
  private RandomAccessFile rafDbf;

  // file suffixes for dbf
  private final String _dbf = ".dbf";

  // represents the dBase file header
  // only needed for writing the dBase file
  private DBFHeader header = null;

  // representing the name of the dBase file
  // only needed for writing the dBase file
  private final String fname;

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

  // size of the cache used for reading data from the dbase table
  private long cacheSize = 1000000;

  // array containing the data of the cache
  private byte[] dataArray = null;

  // file position the caches starts
  private long startIndex = 0;

  final int m_defaultFileShapeType;

  private String m_prefix;

  /**
   * constructor <BR>
   * only for reading a dBase file <BR>
   */
  public DBaseFile( String url, int defaultFileShapeType ) throws IOException
  {
    fname = url;
    m_prefix = fname.replaceAll( ".+[/|\\\\]", "" );
    m_defaultFileShapeType = defaultFileShapeType;
    // creates rafDbf
    rafDbf = new RandomAccessFile( url + _dbf, "r" );

    // dataArray = new byte[(int)rafDbf.length()];
    if( cacheSize > rafDbf.length() )
    {
      cacheSize = rafDbf.length();
    }

    dataArray = new byte[(int) cacheSize];
    rafDbf.read( dataArray );
    rafDbf.seek( 0 );

    // initialize dbase file
    initDBaseFile();

    filemode = 0;
  }

  /**
   * constructor <BR>
   * only for writing a dBase file <BR>
   */
  public DBaseFile( String url, FieldDescriptor[] fieldDesc )
  {
    m_defaultFileShapeType = -1;
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
  public void close( )
  {
    try
    {
      rafDbf.close();
    }
    catch( Exception ex )
    {
      // shouldnt we do something here?
    }
  }

  /**
   * method: initDBaseFile(); inits a DBF file. This is based on Pratap Pereira's Xbase.pm perl module
   */
  private void initDBaseFile( ) throws IOException
  {
    // position the record pointer at 0
    rafDbf.seek( 0 );

    // read the file type
    fixByte( rafDbf.readByte() );

    // get the last update date
    fixByte( rafDbf.readByte() );
    fixByte( rafDbf.readByte() );
    fixByte( rafDbf.readByte() );

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
    num_fields = (file_datap - 33) / 32;

    // read in the column data
    int locn = 0; // offset of the current column

    // process each field
    for( int i = 1; i <= num_fields; i++ )
    {
      // seek the position of the field definition data.
      // This information appears after the first 32 byte
      // table information, and lives in 32 byte chunks.
      rafDbf.seek( ((i - 1) * 32) + 32 );

      b = null;

      // get the column name into a byte array
      b = new byte[11];
      rafDbf.readFully( b );

      // bugfix: 'b' may contain 0-bytes, so convert only up to first 0 byte
      int length = 11;
      for( int bIndex = 0; bIndex < 11; bIndex++ )
      {
        if( b[bIndex] == 0 )
        {
          length = bIndex;
          break;
        }
      }

      // convert the byte array to a String
      String col_name = new String( b, 0, length ).trim().toUpperCase();

      // read in the column type
      char[] c = new char[1];
      c[0] = (char) rafDbf.readByte();

      // skip four bytes
      rafDbf.skipBytes( 4 );

      // get field length and precision
      short flen = fixByte( rafDbf.readByte() );
      short fdec = fixByte( rafDbf.readByte() );
      // System.out.println(col_name + " len: " + flen + " dec: " + fdec);
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

    m_featureType = createFeatureType();
  } // end of initDBaseFile

  /**
   *  
   */
  private IFeatureType createFeatureType( )
  {
    dbfCol column = null;

    final IPropertyType[] ftp = new IPropertyType[colHeader.size() + 1];

    final ITypeRegistry<IMarshallingTypeHandler> registry = MarshallingTypeRegistrySingleton.getTypeRegistry();

    final ITypeHandler stringTH = registry.getTypeHandlerForTypeName( new QName( NS.XSD_SCHEMA, "string" ) );
    final ITypeHandler integerTH = registry.getTypeHandlerForTypeName( new QName( NS.XSD_SCHEMA, "int" ) );
    final ITypeHandler longTH = registry.getTypeHandlerForTypeName( new QName( NS.XSD_SCHEMA, "long" ) );
    final ITypeHandler doubleTH = registry.getTypeHandlerForTypeName( new QName( NS.XSD_SCHEMA, "double" ) );
    final ITypeHandler floatTH = registry.getTypeHandlerForTypeName( new QName( NS.XSD_SCHEMA, "float" ) );
    // final ITypeHandler booleanTH = registry.getTypeHandlerForTypeName( new QName( NS.XSD_SCHEMA, "boolean" ) );
    final ITypeHandler dateTH = registry.getTypeHandlerForTypeName( new QName( NS.XSD_SCHEMA, "date" ) );

    final ITypeHandler byteArrayOutputStreamTH = registry.getTypeHandlerForClassName( ByteArrayOutputStream.class );

    for( int i = 0; i < colHeader.size(); i++ )
    {
      // retrieve the dbfCol object which corresponds
      // to this column.
      column = column_info.get( colHeader.get( i ) );
      final ITypeHandler th;
      if( column.type.equalsIgnoreCase( "C" ) )
        th = stringTH;
      else if( column.type.equalsIgnoreCase( "F" ) || column.type.equalsIgnoreCase( "N" ) )
      {
        if( column.prec == 0 )
        {
          if( column.size < 10 )
            th = integerTH;
          else
            th = longTH;
        }
        else
        {
          if( column.size < 8 )
            th = floatTH;
          else
            th = doubleTH;
        }
      }
      else if( column.type.equalsIgnoreCase( "M" ) )
        th = stringTH;
      else if( column.type.equalsIgnoreCase( "L" ) )
        th = stringTH;
      else if( column.type.equalsIgnoreCase( "D" ) )
        th = dateTH;
      else if( column.type.equalsIgnoreCase( "B" ) )
        th = byteArrayOutputStreamTH;
      else
        th = null;
      ftp[i] = GMLSchemaFactory.createValuePropertyType( new QName( NS_SHAPEFILE, column.name ), th.getTypeName(), th, 1, 1 );
    }

    // remove everything before "\" or "/"
    final QName qNameFT = new QName( NS_SHAPEFILE, fname.replaceAll( ".+(/,\\\\)", "" ) );
    final Class geoClass = getGeometryType();
    final IMarshallingTypeHandler geoTH = registry.getTypeHandlerForClassName( geoClass );
    ftp[ftp.length - 1] = GMLSchemaFactory.createValuePropertyType( new QName( NS_SHAPEFILE, "GEOM" ), geoTH.getTypeName(), geoTH, 1, 1 );
    return GMLSchemaFactory.createFeatureType( qNameFT, ftp );
  }

  private Class getGeometryType( )
  {
    switch( m_defaultFileShapeType )
    {
      // remeber: the geometry classes must be the same
      // as the one used by the marshalling type handlers
      case ShapeConst.SHAPE_TYPE_POINT:
        return GeometryUtilities.getPointClass();
      case ShapeConst.SHAPE_TYPE_MULTIPOINT:
        return GeometryUtilities.getMultiPointClass();
      case ShapeConst.SHAPE_TYPE_POLYLINE:
        return GeometryUtilities.getLineStringClass();
      case ShapeConst.SHAPE_TYPE_POLYGON:
        return GeometryUtilities.getPolygonClass();
      default:
        return GM_Object.class;
    }
  }

  /**
   * method: getRecordNum() <BR>
   * Get the number of records in the table
   */
  public int getRecordNum( ) throws DBaseException
  {
    if( filemode == 1 )
    {
      throw new DBaseException( "class is initialized in write-only mode" );
    }

    return (int) file_numrecs;
  }

  /**
   * method: goTop() <BR>
   * Position the record pointer at the top of the table.
   */
  public void goTop( ) throws DBaseException
  {
    if( filemode == 1 )
    {
      throw new DBaseException( "class is initialized in write-only mode" );
    }

    record_number = 0;
  }

  /**
   * method: nextRecord() <BR>
   * Advance the record pointer to the next record.
   */
  public boolean nextRecord( ) throws DBaseException
  {
    if( filemode == 1 )
    {
      throw new DBaseException( "class is initialized in write-only mode" );
    }

    if( record_number < file_numrecs )
    {
      record_number++;
      return true;
    }
    return false;
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
      dbfCol column = column_info.get( col_name );

      // seek the starting offset of the current record,
      // as indicated by record_number
      long pos = file_datap + ((record_number - 1) * file_datalength);

      // read data from cache if the requested part of the dbase file is
      // within it
      if( (pos >= startIndex) && ((pos + column.position + column.size) < (startIndex + cacheSize)) )
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

      // Changed by Belger
      // The Old version did not respect Charset Conversion
      // REMARK:
      // the old version also filtered every whitespace 'char(32)'
      // but i think what to be done is just to trim() the returned string
      final byte[] bytes = new byte[column.size];
      for( int i = 0; i < bytes.length; i++ )
      {
        final int kk = (int) pos + column.position + i;
        bytes[i] = dataArray[kk];
      }

      final String charsetname = CharsetUtilities.getDefaultCharset();
      return new String( bytes, charsetname ).trim();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return e.toString();
    }
  }

  /**
   * method: public String[] getProperties() <BR>
   * returns the properties (column headers) of the dBase-file <BR>
   */
  public String[] getProperties( ) throws DBaseException
  {
    if( filemode == 1 )
      throw new DBaseException( "class is initialized in write-only mode" );

    return colHeader.toArray( new String[colHeader.size()] );
  }

  /**
   * method: public String[] getDataTypes() <BR>
   * returns the datatype of each column of the database <BR>
   */
  public String[] getDataTypes( ) throws DBaseException
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
      column = column_info.get( colHeader.get( i ) );

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
    dbfCol col = column_info.get( field );
    if( col == null )
      throw new DBaseException( "Field " + field + " not found" );

    return col.size;
  }

  /**
   * method: public String[] getDataTypes(String[] fields) <BR>
   * returns the datatype of each column of the database specified by fields <BR>
   */
  public String[] getDataTypes( String[] fields ) throws DBaseException
  {
    if( filemode == 1 )
    {
      throw new DBaseException( "class is initialized in write-only mode" );
    }

    ArrayList<String> vec = new ArrayList<String>();
    dbfCol column;

    for( int i = 0; i < colHeader.size(); i++ )
    {
      // check if the current (i'th) column (string) is
      // within the array of specified columns
      if( contains( fields, colHeader.get( i ) ) )
      {
        // retrieve the dbfCol object which corresponds
        // to this column.
        column = column_info.get( colHeader.get( i ) );

        vec.add( column.type.trim() );
      }
    }

    return vec.toArray( new String[vec.size()] );
  }

  /**
   * returns a row of the dBase-file as Feature containing a place holder (field name = "GEOM") for a geometry.
   * 
   * @param allowNull
   *          if true, everything wich cannot read or parsed gets 'null' instead of ""
   */
  public Feature getFRow( Feature parent, int rowNo, boolean allowNull ) throws DBaseException
  {
    goTop();

    record_number += rowNo;

    Object[] fp = new Object[colHeader.size() + 1];

    for( int i = 0; i < colHeader.size(); i++ )
    {
      // retrieve the dbfCol object which corresponds
      // to this column.
      dbfCol column = column_info.get( colHeader.get( i ) );

      String value = getColumn( column.name.trim() );
      // System.out.print(value);
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
            fp[i] = allowNull ? null : new Double( "0" );
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
            fp[i] = TimeTools.createCalendar( value.substring( 0, 4 ) + "-" + value.substring( 4, 6 ) + "-" + value.substring( 6, 8 ) ).getTime();
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
        fp[i] = allowNull ? null : "";
      }
    }
    // final String prefix = featureType.getQName().getLocalPart();

    return FeatureFactory.createFeature( parent, m_prefix + rowNo, m_featureType, fp );
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
      column = column_info.get( colHeader.get( i ) );

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
          row[i] = TimeTools.createCalendar( value.substring( 0, 4 ) + "-" + value.substring( 4, 6 ) + "-" + value.substring( 6, 8 ) ).getTime();
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
          // System.out.println( e.toString() );
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
      return (short) (b + 256);
    }

    return b;
  }

  /**
   * method: public void writeAllToFile() creates the dbase file and writes all data to it if the file specified by
   * fname (s.o.) exists it will be deleted!
   */
  public void writeAllToFile( ) throws IOException, DBaseException
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

    // write number of records
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
   * method: public setRecord(ArrayList recData) writes a data record to byte array representing the data section of the
   * dBase file. The method gets the data type of each field in recData from fieldDesc wich has been set at the
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
   * method: public setRecord(int index, ArrayList recData) writes a data record to byte array representing the data
   * section of the dBase file. The method gets the data type of each field in recData from fieldDesc wich has been set
   * at the constructor. index specifies the location of the retrieved record in the datasection. if an invalid index is
   * used an exception will be thrown
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
