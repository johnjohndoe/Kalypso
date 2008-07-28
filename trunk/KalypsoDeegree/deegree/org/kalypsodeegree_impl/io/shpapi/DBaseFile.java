/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always. 
 * 
 * If you intend to use this software in other ways than in kalypso 
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree, 
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree_impl.io.shpapi;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.RandomAccessFile;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.regex.Pattern;

import javax.xml.namespace.QName;

import org.apache.commons.io.IOUtils;
import org.kalypso.commons.xml.NS;
import org.kalypso.contribs.eclipse.core.runtime.TempFileUtilities;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.gmlschema.GMLSchemaFactory;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.ByteUtils;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;
import org.kalypsodeegree_impl.tools.TimeTools;

/**
 * the datatypes of the dBase file and their representation as java types: dBase-type dBase-type-ID java-type character
 * "C" String float "F" Float number "N" Double logical "L" String memo "M" String date "D" Date binary "B"
 * ByteArrayOutputStream
 * 
 * @version 12.12.2000
 * @author Andreas Poth
 * @author Markus Müller, email: mm@giub.uni-bonn.de
 * @version 18.12.2007
 * @author Stefan Kurzbach
 */
public class DBaseFile
{
  public static final String SHP_NAMESPACE_URI = "org.kalypso.shape";

  private final String m_customNamespaceURI;

  private final QName m_propertyCustomFeatureMember;

  private final ArrayList<String> colHeader = new ArrayList<String>();

  // representing the datasection of the dBase file
  // only needed for writing a dBase file
  private DBFDataSection dataSection = null;

  // feature type of the dbase table + a GM_Object as last field
  private IFeatureType m_featureType = null;

  // Hashtable to contain info abouts in the table
  private final Hashtable<String, dbfCol> column_info = new Hashtable<String, dbfCol>();

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

  private String m_suffix;

  /**
   * constructor <BR>
   * only for reading a dBase file <BR>
   */
  public DBaseFile( final String url, final int defaultFileShapeType ) throws IOException
  {
    fname = url;
    // m_prefix = fname.replaceAll( ".+[/|\\\\]", "" );
    m_suffix = "" + fname.hashCode();
    m_customNamespaceURI = "org.kalypso.shape.custom_" + m_suffix;
    // TODO: name is wrong: feature normally have capitals as first char
    m_propertyCustomFeatureMember = new QName( m_customNamespaceURI, "featureMember" );

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
  public DBaseFile( final String url, final FieldDescriptor[] fieldDesc )
  {
    this(url, fieldDesc, null);
  }

  /**
   * constructor <BR>
   * only for writing a dBase file <BR>
   * name of the charset string value will be encoded with. If null, the default charset will be used.
   */
  public DBaseFile( final String url, final FieldDescriptor[] fieldDesc, final String charset )
  {
    m_defaultFileShapeType = -1;
    fname = url;

    m_customNamespaceURI = "org.kalypso.shape.custom#" + fname.hashCode();
    m_propertyCustomFeatureMember = new QName( m_customNamespaceURI, "featureMember" );

    // create header
    header = new DBFHeader( fieldDesc );

    // create data section
    dataSection = new DBFDataSection( fieldDesc, charset );

    filemode = 1;
  }

  public void close( ) throws IOException
  {
    // rafDbf can be null if dbf is written not read
    if( rafDbf != null )
      rafDbf.close();
  }

  /**
   * method: initDBaseFile(); inits a DBF file. This is based on Pratap Pereira's Xbase.pm perl module
   */
  private void initDBaseFile( ) throws IOException
  {
    // position the record pointer at 0
    rafDbf.seek( 0 );

    // read the file type
    DBaseFile.fixByte( rafDbf.readByte() );

    // get the last update date
    DBaseFile.fixByte( rafDbf.readByte() );
    DBaseFile.fixByte( rafDbf.readByte() );
    DBaseFile.fixByte( rafDbf.readByte() );

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
      final String col_name = new String( b, 0, length ).trim().toUpperCase();

      // read in the column type
      final char[] c = new char[1];
      c[0] = (char) rafDbf.readByte();

      // skip four bytes
      rafDbf.skipBytes( 4 );

      // get field length and precision
      final short flen = DBaseFile.fixByte( rafDbf.readByte() );
      final short fdec = DBaseFile.fixByte( rafDbf.readByte() );
      // System.out.println(col_name + " len: " + flen + " dec: " + fdec);
      // set the field position to the current
      // value of locn
      final int fpos = locn;

      // increment locn by the length of this field.
      locn += flen;

      // create a new dbfCol object and assign it the
      // attributes of the current field
      final dbfCol column = new dbfCol( col_name );
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
    String elementsString = "";

    final IPropertyType[] ftp = new IPropertyType[colHeader.size() + 1];

    final ITypeRegistry<IMarshallingTypeHandler> registry = MarshallingTypeRegistrySingleton.getTypeRegistry();

    final IMarshallingTypeHandler stringTH = registry.getTypeHandlerForTypeName( new QName( NS.XSD_SCHEMA, "string" ) );
    final IMarshallingTypeHandler integerTH = registry.getTypeHandlerForTypeName( new QName( NS.XSD_SCHEMA, "int" ) );
    final IMarshallingTypeHandler longTH = registry.getTypeHandlerForTypeName( new QName( NS.XSD_SCHEMA, "long" ) );
    final IMarshallingTypeHandler doubleTH = registry.getTypeHandlerForTypeName( new QName( NS.XSD_SCHEMA, "double" ) );
    final IMarshallingTypeHandler floatTH = registry.getTypeHandlerForTypeName( new QName( NS.XSD_SCHEMA, "float" ) );
    // final IMarshallingTypeHandler booleanTH = registry.getTypeHandlerForTypeName( new QName( NS.XSD_SCHEMA, "boolean"
    // ) );
    final IMarshallingTypeHandler dateTH = registry.getTypeHandlerForTypeName( new QName( NS.XSD_SCHEMA, "date" ) );

    final IMarshallingTypeHandler byteArrayOutputStreamTH = registry.getTypeHandlerForClassName( ByteArrayOutputStream.class );

    for( int i = 0; i < colHeader.size(); i++ )
    {
      // retrieve the dbfCol object which corresponds
      // to this column.
      column = column_info.get( colHeader.get( i ) );
      final IMarshallingTypeHandler th;
      if( column.type.equalsIgnoreCase( "C" ) )
      {
        th = stringTH;
      }
      else if( column.type.equalsIgnoreCase( "F" ) || column.type.equalsIgnoreCase( "N" ) )
      {
        if( column.prec == 0 )
        {
          if( column.size < 10 )
          {
            th = integerTH;
          }
          else
          {
            th = longTH;
          }
        }
        else
        {
          if( column.size < 8 )
          {
            th = floatTH;
          }
          else
          {
            th = doubleTH;
          }
        }
      }
      else if( column.type.equalsIgnoreCase( "M" ) )
      {
        th = stringTH;
      }
      else if( column.type.equalsIgnoreCase( "L" ) )
      {
        // TODO: This is wrong: L should be parsed as boolean; is there already some code which depends on this
        // wrong implementation?
        th = stringTH;
      }
      else if( column.type.equalsIgnoreCase( "D" ) )
      {
        th = dateTH;
      }
      else if( column.type.equalsIgnoreCase( "B" ) )
      {
        th = byteArrayOutputStreamTH;
      }
      else
      {
        th = null;
      }
      ftp[i] = GMLSchemaFactory.createValuePropertyType( new QName( m_customNamespaceURI, column.name ), th, 1, 1, false );
      elementsString = elementsString + "<xs:element name=\"" + column.name + "\" type=\"xs:" + th.getTypeName().getLocalPart() + "\"/>\n";
    }

    // remove everything before "\" or "/"
    // final QName qNameFT = new QName( ShapeFile.CUSTOM_NAMESPACE_URI, fname.replaceAll( ".+(/,\\\\)", "" ) );
    // final QName qNameFT = new QName( DBaseFile.NS_SHAPEFILE, fname.replaceAll( ".+(/,\\\\)", "" ) );
    final Class< ? extends GM_Object> geoClass = getGeometryType();
    final IMarshallingTypeHandler geoTH = registry.getTypeHandlerForClassName( geoClass );
    ftp[ftp.length - 1] = GMLSchemaFactory.createValuePropertyType( new QName( m_customNamespaceURI, "GEOM" ), geoTH, 1, 1, false );

    final String geometryPropertyTypeString = "gml:"+ geoTH.getShortname();

    try
    {
      final InputStream schemaTemplateInput = getClass().getResource( "resources/shapeCustomTemplate.xsd" ).openStream();
      String schemaString = IOUtils.toString( schemaTemplateInput );
      schemaString = schemaString.replaceAll( Pattern.quote( "${CUSTOM_NAMESPACE_SUFFIX}" ), m_suffix );
      schemaString = schemaString.replaceAll( Pattern.quote( "${CUSTOM_FEATURE_GEOMETRY_PROPERTY_TYPE}" ), geometryPropertyTypeString );
      schemaString = schemaString.replaceAll( Pattern.quote( "${CUSTOM_FEATURE_PROPERTY_ELEMENTS}" ), elementsString );

      final File tempFile = TempFileUtilities.createTempFile( KalypsoDeegreePlugin.getDefault(), "temporaryCustomSchemas", "customSchema", ".xsd" );
      tempFile.deleteOnExit();
      final FileOutputStream fileOutputStream = new FileOutputStream( tempFile );
      fileOutputStream.write( schemaString.getBytes( "UTF8" ) );
      fileOutputStream.flush();
      fileOutputStream.close();
      final GMLSchema schema = GMLSchemaFactory.createGMLSchema( "3.1.1", tempFile.toURL() );
      // final IGMLSchema schema = GMLSchemaFactory.createGMLSchema( new StringInputStream( schemaString ), "3.1.1", new
      // File( fname ).getParentFile().toURL() );
      return GMLSchemaFactory.createFeatureType( m_propertyCustomFeatureMember, ftp, schema, new QName( SHP_NAMESPACE_URI, "_Shape" ) );
    }
    catch( final IOException e )
    {
      // should not happen
      throw new IllegalStateException( e );
    }
    catch( final GMLSchemaException e )
    {
      // should not happen
      throw new IllegalStateException( e );
    }
  }

  public IFeatureType getFeatureType( )
  {
    return m_featureType;
  }

  private Class< ? extends GM_Object> getGeometryType( )
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
        return GeometryUtilities.getMultiLineStringClass();
      case ShapeConst.SHAPE_TYPE_POLYGON:
        return GeometryUtilities.getMultiPolygonClass();
      case ShapeConst.SHAPE_TYPE_POINTZ:
        return GeometryUtilities.getPointClass();
      case ShapeConst.SHAPE_TYPE_POLYLINEZ:
        return GeometryUtilities.getMultiLineStringClass();
      case ShapeConst.SHAPE_TYPE_POLYGONZ:
        return GeometryUtilities.getMultiPolygonClass();
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
  public String getColumn( final String col_name ) throws DBaseException
  {
    if( filemode == 1 )
    {
      throw new DBaseException( "class is initialized in write-only mode" );
    }

    try
    {
      // retrieve the dbfCol object which corresponds
      // to this column.
      final dbfCol column = column_info.get( col_name );

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

      final String charsetname = Charset.defaultCharset().name();
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
    {
      throw new DBaseException( "class is initialized in write-only mode" );
    }

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

    final String[] datatypes = new String[colHeader.size()];
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
  private boolean contains( final String[] container, final String element )
  {
    for( final String element2 : container )
    {
      if( element2.equals( element ) )
      {
        return true;
      }
    }

    return false;
  }

  /**
   * returns the size of a column
   */
  public int getDataLength( final String field ) throws DBaseException
  {
    final dbfCol col = column_info.get( field );
    if( col == null )
    {
      throw new DBaseException( "Field " + field + " not found" );
    }

    return col.size;
  }

  /**
   * method: public String[] getDataTypes(String[] fields) <BR>
   * returns the datatype of each column of the database specified by fields <BR>
   */
  public String[] getDataTypes( final String[] fields ) throws DBaseException
  {
    if( filemode == 1 )
    {
      throw new DBaseException( "class is initialized in write-only mode" );
    }

    final ArrayList<String> vec = new ArrayList<String>();
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
   *            if true, everything wich cannot read or parsed gets 'null' instead of ""
   */
  public Feature getFRow( final Feature parent, final IRelationType parentRelation, final int rowNo, final boolean allowNull ) throws DBaseException
  {
    goTop();

    record_number += rowNo;

    final Object[] fp = new Object[colHeader.size() + 1];

    for( int i = 0; i < colHeader.size(); i++ )
    {
      // retrieve the dbfCol object which corresponds
      // to this column.
      final dbfCol column = column_info.get( colHeader.get( i ) );

      final String value = getColumn( column.name.trim() );
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
          catch( final Exception ex )
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
          {
            fp[i] = null;
          }
          else
          {
            fp[i] = TimeTools.createCalendar( value.substring( 0, 4 ) + "-" + value.substring( 4, 6 ) + "-" + value.substring( 6, 8 ) ).getTime();
          }
        }
        else if( column.type.equalsIgnoreCase( "B" ) )
        {
          final ByteArrayOutputStream os = new ByteArrayOutputStream( 10000 );
          try
          {
            os.write( value.getBytes() );
          }
          catch( final IOException e )
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

// return FeatureFactory.createFeature( parent, parentRelation, rowNo + "_" + m_suffix, m_featureType, fp );
    return FeatureFactory.createFeature( parent, parentRelation, "" + rowNo, m_featureType, fp );
  }

  /**
   * method: public ArrayList getRow(int row) <BR>
   * returns a row of the dBase-file <BR>
   */
  public Object[] getRow( final int rowNo ) throws DBaseException
  {
    if( filemode == 1 )
    {
      throw new DBaseException( "class is initialized in write-only mode" );
    }

    String value = "";
    final Object[] row = new Object[colHeader.size()];
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
        catch( final Exception ex )
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
        {
          row[i] = "";
        }
        else
        {
          row[i] = TimeTools.createCalendar( value.substring( 0, 4 ) + "-" + value.substring( 4, 6 ) + "-" + value.substring( 6, 8 ) ).getTime();
        }
      }
      else if( column.type.equalsIgnoreCase( "B" ) )
      {
        final ByteArrayOutputStream os = new ByteArrayOutputStream( 10000 );

        try
        {
          os.write( value.getBytes() );
        }
        catch( final IOException e )
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
  private static short fixByte( final byte b )
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
      throw new DBaseException( "class is initialized in read-only mode" );

    // if a file with the retrieved filename exists, delete it!
    File file = new File( fname + ".dbf" );

    if( file.exists() )
      file.delete();

    file = null;

    // create a new file
    final RandomAccessFile rdbf = new RandomAccessFile( fname + ".dbf", "rw" );

    byte[] b = header.getHeader();

    final int nRecords = dataSection.getNoOfRecords();

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
  public void setRecord( final ArrayList recData ) throws DBaseException
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
  public void setRecord( final int index, final ArrayList recData ) throws DBaseException
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
   *            the column name
   */
  tsColumn( final String s )
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
  public dbfCol( final String c )
  {
    super( c );
  }
} // end of class dbfCol
