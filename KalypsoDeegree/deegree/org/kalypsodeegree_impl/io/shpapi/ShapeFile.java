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

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Hashtable;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureCollection;
import org.deegree.model.feature.FeatureProperty;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.model.geometry.ByteUtils;
import org.deegree.model.geometry.GM_Curve;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Exception;
import org.deegree.model.geometry.GM_MultiCurve;
import org.deegree.model.geometry.GM_MultiPoint;
import org.deegree.model.geometry.GM_MultiSurface;
import org.deegree.model.geometry.GM_Object;
import org.deegree.model.geometry.GM_Point;
import org.deegree.model.geometry.GM_Position;
import org.deegree.model.geometry.GM_Surface;
import org.deegree_impl.io.rtree.HyperBoundingBox;
import org.deegree_impl.io.rtree.HyperPoint;
import org.deegree_impl.io.rtree.RTree;
import org.deegree_impl.io.rtree.RTreeException;
import org.deegree_impl.model.feature.FeatureFactory;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.deegree_impl.tools.Debug;

/**
 * Class representing an ESRI Shape File.
 * <p>
 * This is a modification of the <tt>ShapeFile</tt> class within the shpapi
 * package of sfcorba2java project performed by the EXSE-Working group of of the
 * geogr. institute of the university of Bonn
 * (http://www.giub.uni-bonn.de/exse/results/welcome.html).
 * 
 * 
 * <p>
 * ------------------------------------------------------------------------
 * </p>
 * 
 * @version 17.10.2001
 * @author Andreas Poth
 *         <p>
 */
public class ShapeFile
{
  private DBaseFile dbf = null;

  private SHP2WKS shpwks = new SHP2WKS();

  /*
   * contains the dBase indexes
   */
  private Hashtable dBaseIndexes = new Hashtable();

  /*
   * aggregated Instance-variables
   */
  private MainFile shp = null;

  private RTree rti = null;

  private String url = null;

  /*
   * indicates if a dBase-file is associated to the shape-file
   */
  private boolean hasDBaseFile = true;

  /*
   * indicates if an R-tree index is associated to the shape-file
   */
  private boolean hasRTreeIndex = true;

  /**
   * constructor: <BR>
   * Construct a ShapeFile from a file name. <BR>
   */
  public ShapeFile( String url ) throws IOException
  {
    this.url = url;

    /*
     * initialize the MainFile
     */
    shp = new MainFile( url );

    /*
     * initialize the DBaseFile
     */
    try
    {
      dbf = new DBaseFile( url, shp.getFileShapeType() );
    }
    catch( IOException e )
    {
      hasDBaseFile = false;
    }

    /*
     * initialize the RTreeIndex
     */
    try
    {
      rti = new RTree( url + ".rti" );
    }
    catch( RTreeException e )
    {
      hasRTreeIndex = false;
    }

    if( hasDBaseFile )
    {
      String[] s = null;

      try
      {
        s = getProperties();
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }

      for( int i = 0; i < s.length; i++ )
      {
        try
        {
          dBaseIndexes.put( s[i], new DBaseIndex( url + "$" + s[i] ) );
        }
        catch( IOException e )
        {}
      }
    }
  }

  /**
   * constructor: <BR>
   * Construct a ShapeFile from a file name. <BR>
   */
  public ShapeFile( String url, String rwflag ) throws IOException
  {
    this.url = url;

    shp = new MainFile( url, rwflag );

    //TODO: initialize dbf, rti
    hasDBaseFile = false;
    hasRTreeIndex = false;

  }

  /**
   *  
   */
  public void close()
  {

    shp.close();

    if( dbf != null )
    {
      dbf.close();
    }

    if( rti != null )
    {
      try
      {
        rti.close();
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
    }

    for( Enumeration e = dBaseIndexes.elements(); e.hasMoreElements(); )
    {
      DBaseIndex index = (DBaseIndex)e.nextElement();

      try
      {
        index.close();
      }
      catch( Exception ex )
      {}
    }

  }

  /**
   * returns true if a column is indexed
   */
  public boolean hasDBaseIndex( String column )
  {
    DBaseIndex index = (DBaseIndex)dBaseIndexes.get( column );
    return index != null;
  }

  /**
   * returns true if a dBase-file is associated to the shape-file <BR>
   */
  public boolean hasDBaseFile()
  {
    return this.hasDBaseFile;
  }

  /**
   * returns true if an R-tree index is associated to the shape-file <BR>
   */
  public boolean hasRTreeIndex()
  {
    return this.hasRTreeIndex;
  }

  /**
   * returns the number of records within a shape-file <BR>
   */
  public int getRecordNum()
  {
    return shp.getRecordNum();
  }

  /**
   * returns the minimum bounding rectangle of all geometries <BR>
   * within the shape-file
   */
  public GM_Envelope getFileMBR()
  {
    double xmin = shp.getFileMBR().west;
    double xmax = shp.getFileMBR().east;
    double ymin = shp.getFileMBR().south;
    double ymax = shp.getFileMBR().north;

    return GeometryFactory.createGM_Envelope( xmin, ymin, xmax, ymax );
  }

  /**
   * returns the minimum bound rectangle of RecNo'th Geometrie <BR>
   */
  public GM_Envelope getMBRByRecNo( int recNo ) throws IOException
  {
    SHPEnvelope shpenv = shp.getRecordMBR( recNo );
    double xmin = shpenv.west;
    double xmax = shpenv.east;
    double ymin = shpenv.south;
    double ymax = shpenv.north;

    return GeometryFactory.createGM_Envelope( xmin, ymin, xmax, ymax );
  }

  /** Same as {@link #getFeatureByRecNo(int, boolean) getFeatureByRecNo(int, true)} */
  public Feature getFeatureByRecNo( int RecNo ) throws IOException, GM_Exception,
  HasNoDBaseFileException, DBaseException
  {
    return getFeatureByRecNo( RecNo, false );
  }
  
  /**
   * returns the RecNo'th entry of the shape file as Feature. This contains the
   * geometry as well as the attributes stored into the dbase file.
   * @param allowNull if true, everything wich cannot parsed gets 'null' instaed of ""
   */
  public Feature getFeatureByRecNo( int RecNo, boolean allowNull ) throws IOException, GM_Exception,
      HasNoDBaseFileException, DBaseException
  {
    if( !hasDBaseFile )
    {
      throw new HasNoDBaseFileException( "Exception: there is no dBase-file "
          + "associated to this shape-file" );
    }

    Feature feature = dbf.getFRow( RecNo, allowNull );
    GM_Object geo = getGM_ObjectByRecNo( RecNo );

    FeatureProperty fp = FeatureFactory.createFeatureProperty( "GEOM", geo );

    feature.setProperty( fp );

    return feature;
  }

  /**
   * returns RecNo'th Geometrie <BR>
   */
  public GM_Object getGM_ObjectByRecNo( int RecNo ) throws IOException, GM_Exception
  {
    GM_Object geom = null;

    int shpType = getShapeTypeByRecNo( RecNo );

    if( shpType == ShapeConst.SHAPE_TYPE_POINT )
    {
      SHPPoint shppoint = (SHPPoint)shp.getByRecNo( RecNo );

      geom = shpwks.transformPoint( null, shppoint );
    }
    else if( shpType == ShapeConst.SHAPE_TYPE_MULTIPOINT )
    {
      SHPMultiPoint shpmultipoint = (SHPMultiPoint)shp.getByRecNo( RecNo );

      GM_Point[] points = shpwks.transformMultiPoint( null, shpmultipoint );

      if( points != null )
      {
        GM_MultiPoint mp = GeometryFactory.createGM_MultiPoint( points );
        geom = mp;
      }
      else
      {
        geom = null;
      }
    }
    else if( shpType == ShapeConst.SHAPE_TYPE_POLYLINE )
    {
      SHPPolyLine shppolyline = (SHPPolyLine)shp.getByRecNo( RecNo );

      GM_Curve[] curves = shpwks.transformPolyLine( null, shppolyline );

      if( ( curves != null ) && ( curves.length > 1 ) )
      {
        // create multi curve
        GM_MultiCurve mc = GeometryFactory.createGM_MultiCurve( curves );
        geom = mc;
      }
      else if( ( curves != null ) && ( curves.length == 1 ) )
      {
        // single curve
        geom = curves[0];
      }
      else
      {
        geom = null;
      }
    }
    else if( shpType == ShapeConst.SHAPE_TYPE_POLYGON )
    {
      SHPPolygon shppoly = (SHPPolygon)shp.getByRecNo( RecNo );

      GM_Surface[] polygons = shpwks.transformPolygon( null, shppoly );

      if( ( polygons != null ) && ( polygons.length > 1 ) )
      {
        // create multi surface
        GM_MultiSurface ms = GeometryFactory.createGM_MultiSurface( polygons );
        geom = ms;
      }
      else if( ( polygons != null ) && ( polygons.length == 1 ) )
      {
        geom = polygons[0];
      }
      else
      {
        geom = null;
      }
    }

    return geom;
  }

  /**
   * returns the type of the RecNo'th Geometrie <BR>
   * per definition a shape file contains onlay one shape type <BR>
   * but null shapes are possible too! <BR>
   */
  public int getShapeTypeByRecNo( int RecNo ) throws IOException
  {
    return shp.getShapeTypeByRecNo( RecNo );
  }

  /**
   * returns a int array that containts all the record numbers that matches the
   * search operation
   */
  public int[] getGeoNumbersByAttribute( String column, Comparable value ) throws IOException,
      DBaseIndexException
  {
    DBaseIndex index = (DBaseIndex)dBaseIndexes.get( column );

    if( index == null )
    {
      return null;
    }

    return index.search( value );
  }

  /**
   * returns a ArrayList that contains all geomeries of the shape file <BR>
   * which mbr's are completly or partly within the rectangle r <BR>
   * only Points, MultiPoints, PolyLines and Polygons are handled <BR>
   */
  public int[] getGeoNumbersByRect( GM_Envelope r ) throws IOException
  {
    SHPPoint geom = null;
    int[] num = null;
    int numRecs = getRecordNum();
    ArrayList numbers = new ArrayList();

    GM_Envelope mbr = getFileMBR();

    if( !mbr.intersects( r ) )
    {
      return null;
    }

    if( hasRTreeIndex )
    {
      try
      {
        // translate envelope (deegree) to bounding box (rtree)
        HyperBoundingBox box = new HyperBoundingBox( new HyperPoint( r.getMin().getAsArray() ),
            new HyperPoint( r.getMax().getAsArray() ) );
        Object[] iNumbers = rti.intersects( box );
        num = new int[iNumbers.length];

        for( int i = 0; i < iNumbers.length; i++ )
          num[i] = ( (Integer)iNumbers[i] ).intValue();

        return num;
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
    }

    // for every geometry (record) within the shape file
    // check if it's inside the search-rectangle r
    for( int i = 0; i < numRecs; i++ )
    {
      if( getShapeTypeByRecNo( i + 1 ) == ShapeConst.SHAPE_TYPE_NULL )
      {}
      else if( getShapeTypeByRecNo( i + 1 ) == ShapeConst.SHAPE_TYPE_POINT )
      {
        geom = (SHPPoint)shp.getByRecNo( i + 1 );

        // is the Point within the seach rectangle?
        GM_Position pos = GeometryFactory.createGM_Position( geom.x, geom.y );

        if( r.contains( pos ) == true )
        {
          numbers.add( new Integer( i + 1 ) );
        }
      }
      else
      {
        // get minimum bounding rectangle of the i'th record
        mbr = getMBRByRecNo( i + 1 );

        // is the i'th record a geometrie having a mbr
        // (only for PolyLines, Polygons and MultiPoints mbrs are defined)
        if( mbr != null )
        {
          // if the tested rectangles are not disjunct the number of the
          // actual record is added to the ArrayList
          if( mbr.intersects( r ) )
          {
            numbers.add( new Integer( i + 1 ) );
          }
        }
      }
    }

    if( numbers.size() > 0 )
    {
      num = new int[numbers.size()];

      // put all numbers within numbers to an array
      for( int i = 0; i < numbers.size(); i++ )
      {
        num[i] = ( (Integer)numbers.get( i ) ).intValue();
      }
    }

    return num;
  } // end of getGeoNumbersByRect

  /**
   * is a property unique?
   */
  public boolean isUnique( String property )
  {
    DBaseIndex index = (DBaseIndex)dBaseIndexes.get( property );

    if( index == null )
    {
      return false;
    }

    return index.isUnique();
  }

  /**
   * returns the properties (column headers) of the dBase-file <BR>
   * associated to the shape-file <BR>
   */
  public String[] getProperties() throws HasNoDBaseFileException, DBaseException
  {
    if( !hasDBaseFile )
    {
      throw new HasNoDBaseFileException( "Exception: there is no dBase-file "
          + "associated to this shape-file" );
    }

    return dbf.getProperties();
  }

  /**
   * returns the datatype of each column of the database file <BR>
   * associated to the shape-file <BR>
   */
  public String[] getDataTypes() throws HasNoDBaseFileException, DBaseException
  {
    if( !hasDBaseFile )
    {
      throw new HasNoDBaseFileException( "Exception: there is no dBase-file "
          + "associated to this shape-file" );
    }

    return dbf.getDataTypes();
  }

  /**
   * 
   * 
   * @return @throws
   *         HasNoDBaseFileException
   * @throws DBaseException
   */
  public int[] getDataLengths() throws HasNoDBaseFileException, DBaseException
  {
    String[] properties = getProperties();
    int[] retval = new int[properties.length];

    for( int i = 0; i < properties.length; i++ )
    {
      retval[i] = dbf.getDataLength( properties[i] );
    }

    return retval;
  }

  /**
   * returns the datatype of each column of the dBase associated <BR>
   * to the shape-file specified by fields <BR>
   */
  public String[] getDataTypes( String[] fields ) throws HasNoDBaseFileException, DBaseException
  {
    if( !hasDBaseFile )
    {
      throw new HasNoDBaseFileException( "Exception: there is no dBase-file "
          + "associated to this shape-file" );
    }

    return dbf.getDataTypes( fields );
  }

  /**
   * returns a row of the dBase-file <BR>
   * associated to the shape-file <BR>
   */
  public Object[] getRow( int rowNo ) throws HasNoDBaseFileException, DBaseException
  {
    if( !hasDBaseFile )
    {
      throw new HasNoDBaseFileException( "Exception: there is no dBase-file "
          + "associated to this shape-file" );
    }

    return dbf.getRow( rowNo );
  }

  /**
   * returns the type of the n'th feature in a featurecollection
   * 
   * @param fc :
   *          FeatureCollection
   * @param n :
   *          number of the feature which should be examined starts with 0
   */
  private int getGeometryType( final Feature feature )
  {
    GM_Object[] g = feature.getGeometryProperties();

    if( ( g == null ) || ( g.length == 0 ) )
    {
      return -1;
    }

    if( g[0] instanceof GM_Point )
    {
      return 0;
    }

    if( g[0] instanceof GM_Curve )
    {
      return 1;
    }

    if( g[0] instanceof GM_Surface )
    {
      return 2;
    }

    if( g[0] instanceof GM_MultiPoint )
    {
      return 3;
    }

    if( g[0] instanceof GM_MultiCurve )
    {
      return 4;
    }

    if( g[0] instanceof GM_MultiSurface )
    {
      return 5;
    }

    return -1;
  }

  /**
   * returns the n'th feature of a featurecollection as a GM_Object <BR>
   * 
   * @param fc :
   *          FeatureCollection <BR>
   * @param n :
   *          number of the feature which should be returned <BR>
   */
  private GM_Object getFeatureAsGeometry( final Feature feature )
  {
    return feature.getGeometryProperties()[0];
  }

  /**
   */
  private FeatureProperty[] getFeatureProperties( final Feature feature )
  {
    FeatureTypeProperty[] ftp = feature.getFeatureType().getProperties();
    FeatureProperty[] fp = new FeatureProperty[ftp.length];
    Object[] fp_ = feature.getProperties();

    for( int i = 0; i < ftp.length; i++ )
    {
      fp[i] = FeatureFactory.createFeatureProperty( ftp[i].getName(), fp_[i] );
    }

    return fp;
  }

  /**
   */
  private void initDBaseFile( final Feature[] features ) throws DBaseException
  {
    FieldDescriptor[] fieldDesc = null;

    // get feature properties
    FeatureProperty[] pairs = getFeatureProperties( features[0] );

    // count regular fields
    int cnt = 0;
    FeatureType featT = features[0].getFeatureType();
    FeatureTypeProperty[] ftp = featT.getProperties();
    for( int i = 0; i < pairs.length; i++ )
    {
      if( !( pairs[i].getValue() instanceof ByteArrayInputStream )
          && !( pairs[i].getValue() instanceof GM_Object ) )
        cnt++;
    }

    // allocate memory for fielddescriptors
    fieldDesc = new FieldDescriptor[cnt];

    // get properties names and types and create a FieldDescriptor
    // for each properties except the geometry-property
    cnt = 0;
    for( int i = 0; i < ftp.length; i++ )
    {
      int pos = ftp[i].getName().lastIndexOf( '.' );
      if( pos < 0 )
      {
        pos = -1;
      }
      String s = ftp[i].getName().substring( pos + 1 );
      if( ftp[i].getType().endsWith( "Integer" ) )
      {
        fieldDesc[cnt++] = new FieldDescriptor( s, "N", (byte)20, (byte)0 );
      }
      else if( ftp[i].getType().endsWith( "Byte" ) )
      {
        fieldDesc[cnt++] = new FieldDescriptor( s, "N", (byte)4, (byte)0 );
      }
      else if( ftp[i].getType().endsWith( "Character" ) )
      {
        fieldDesc[cnt++] = new FieldDescriptor( s, "C", (byte)1, (byte)0 );
      }
      else if( ftp[i].getType().endsWith( "Float" ) )
      {
        fieldDesc[cnt++] = new FieldDescriptor( s, "N", (byte)30, (byte)10 );
      }
      else if( ftp[i].getType().endsWith( "Double" ) || ftp[i].getType().endsWith( "Number" ) )
      {
        fieldDesc[cnt++] = new FieldDescriptor( s, "N", (byte)30, (byte)10 );
      }
      else if( ftp[i].getType().endsWith( "BigDecimal" ) )
      {
        fieldDesc[cnt++] = new FieldDescriptor( s, "N", (byte)30, (byte)10 );
      }
      else if( ftp[i].getType().endsWith( "String" ) )
      {
        fieldDesc[cnt++] = new FieldDescriptor( s, "C", (byte)127, (byte)0 );
      }
      else if( ftp[i].getType().endsWith( "Date" ) )
      {
        fieldDesc[cnt++] = new FieldDescriptor( s, "D", (byte)12, (byte)0 );
      }
    }

    //initialize/create DBaseFile
    try
    {
      dbf = new DBaseFile( url, fieldDesc );
    }
    catch( DBaseException e )
    {
      hasDBaseFile = false;
    }
  }

  public void writeShape( final Feature[] features ) throws Exception
  {
    Debug.debugMethodBegin( this, "writeShape" );

    // TODO: check length 0
    
    int nbyte = 0;
    int geotype = -1;
    byte shptype = -1;
    int typ_ = getGeometryType( features[0] );
    byte[] bytearray = null;
    IndexRecord record = null;
    SHPEnvelope mbr = null;
    // mbr of the whole shape file
    SHPEnvelope shpmbr = new SHPEnvelope();
    FeatureProperty[] pairs = null;

    // Set the Offset to the end of the fileHeader
    int offset = ShapeConst.SHAPE_FILE_HEADER_LENGTH;

    // initialize the dbasefile associated with the shapefile
    initDBaseFile( features );

    // loop throug the Geometries of the feature collection anf write them
    // to a bytearray
    for( int i = 0; i < features.length; i++ )
    {
      // get i'th features properties
      pairs = getFeatureProperties( features[i] );

      // write i'th features properties to a ArrayList
      ArrayList vec = new ArrayList();
      FeatureTypeProperty[] ftp = features[0].getFeatureType().getProperties();
      for( int j = 0; j < pairs.length; j++ )
      {
        if( ( ftp[j].getType().endsWith( "Integer" ) ) || ( ftp[j].getType().endsWith( "Byte" ) )
            || ( ftp[j].getType().endsWith( "Character" ) )
            || ( ftp[j].getType().endsWith( "Float" ) ) || ( ftp[j].getType().endsWith( "Double" ) )
            || ( ftp[j].getType().endsWith( "Number" ) )
            || ( ftp[j].getType().endsWith( "String" ) ) || ( ftp[j].getType().endsWith( "Date" ) ) )
        {
          vec.add( pairs[j].getValue() );
        }
        else if( ftp[j].getType().endsWith( "BigDecimal" ) )
        {
          vec.add( new Double( ( (java.math.BigDecimal)pairs[j].getValue() ).doubleValue() ) );
        }
      }

      // write the ArrayList (properties) to the dbase file
      try
      {
        dbf.setRecord( vec );
      }
      catch( DBaseException db )
      {
        db.printStackTrace();
        throw new Exception( db.toString() );
      }

      // Get Geometry Type of i'th feature
      geotype = getGeometryType( features[i] );

      if( geotype < 0 )
      {
        continue;
      }

      if( ( typ_ == 0 ) || ( typ_ == 3 ) )
      {
        if( ( geotype != 0 ) && ( geotype != 3 ) )
        {
          throw new Exception( "not a homogen featurecollectiom" );
        }
      }

      if( ( typ_ == 1 ) || ( typ_ == 4 ) )
      {
        if( ( geotype != 1 ) && ( geotype != 4 ) )
        {
          throw new Exception( "not a homogen featurecollectiom" );
        }
      }

      if( ( typ_ == 2 ) || ( typ_ == 5 ) )
      {
        if( ( geotype != 2 ) && ( geotype != 5 ) )
        {
          throw new Exception( "not a homogen featurecollectiom" );
        }
      }

      // get wks geometrie for feature (i) and write it to a file
      if( geotype == 0 )
      {
        // Geometrie Type = Point
        GM_Point wks = (GM_Point)getFeatureAsGeometry( features[i] );
        SHPPoint shppoint = new SHPPoint( wks.getPosition() );
        nbyte = shppoint.size();
        bytearray = new byte[nbyte + ShapeConst.SHAPE_FILE_RECORD_HEADER_LENGTH];
        shppoint.writeSHPPoint( bytearray, ShapeConst.SHAPE_FILE_RECORD_HEADER_LENGTH );
        mbr = new SHPEnvelope( shppoint, shppoint );

        if( i == 0 )
        {
          shpmbr = mbr;
        }

        shptype = 1;
      }
      else if( geotype == 1 )
      {
        // Geometrie Type = LineString
        GM_Curve[] wks = new GM_Curve[1];
        wks[0] = (GM_Curve)getFeatureAsGeometry( features[i] );

        SHPPolyLine shppolyline = new SHPPolyLine( wks );
        nbyte = shppolyline.size();
        bytearray = new byte[nbyte + ShapeConst.SHAPE_FILE_RECORD_HEADER_LENGTH];
        shppolyline.writeSHPPolyLine( bytearray, ShapeConst.SHAPE_FILE_RECORD_HEADER_LENGTH );
        mbr = shppolyline.getEnvelope();

        if( i == 0 )
        {
          shpmbr = mbr;
        }

        shptype = 3;
      }
      else if( geotype == 2 )
      {
        // Geometrie Type = Polygon
        GM_Surface[] wks = new GM_Surface[1];
        wks[0] = (GM_Surface)getFeatureAsGeometry( features[i] );

        SHPPolygon shppolygon = new SHPPolygon( wks );
        nbyte = shppolygon.size();
        bytearray = new byte[nbyte + ShapeConst.SHAPE_FILE_RECORD_HEADER_LENGTH];
        shppolygon.writeSHPPolygon( bytearray, ShapeConst.SHAPE_FILE_RECORD_HEADER_LENGTH );
        mbr = shppolygon.getEnvelope();

        if( i == 0 )
        {
          shpmbr = mbr;
        }

        shptype = 5;
      }
      else if( geotype == 3 )
      {
        // Geometrie Type = MultiPoint
        GM_MultiPoint wks = (GM_MultiPoint)getFeatureAsGeometry( features[i] );
        SHPMultiPoint shpmultipoint = new SHPMultiPoint( wks );
        nbyte = shpmultipoint.size();
        bytearray = new byte[nbyte + ShapeConst.SHAPE_FILE_RECORD_HEADER_LENGTH];
        shpmultipoint.writeSHPMultiPoint( bytearray, ShapeConst.SHAPE_FILE_RECORD_HEADER_LENGTH );
        mbr = shpmultipoint.getEnvelope();
        shptype = 8;
      }
      else if( geotype == 4 )
      {
        // Geometrie Type = MultiLineString
        GM_MultiCurve wks = (GM_MultiCurve)getFeatureAsGeometry( features[i] );
        SHPPolyLine shppolyline = new SHPPolyLine( wks.getAllCurves() );
        nbyte = shppolyline.size();
        bytearray = new byte[nbyte + ShapeConst.SHAPE_FILE_RECORD_HEADER_LENGTH];
        shppolyline.writeSHPPolyLine( bytearray, ShapeConst.SHAPE_FILE_RECORD_HEADER_LENGTH );
        mbr = shppolyline.getEnvelope();

        if( i == 0 )
        {
          shpmbr = mbr;
        }

        shptype = 3;
      }
      else if( geotype == 5 )
      {
        // Geometrie Type = MultiPolygon
        GM_MultiSurface wks = (GM_MultiSurface)getFeatureAsGeometry( features[i] );
        SHPPolygon shppolygon = new SHPPolygon( wks.getAllSurfaces() );
        nbyte = shppolygon.size();
        bytearray = new byte[nbyte + ShapeConst.SHAPE_FILE_RECORD_HEADER_LENGTH];
        shppolygon.writeSHPPolygon( bytearray, ShapeConst.SHAPE_FILE_RECORD_HEADER_LENGTH );
        mbr = shppolygon.getEnvelope();

        if( i == 0 )
        {
          shpmbr = mbr;
        }

        shptype = 5;
      }

      // write bytearray to the shape file
      record = new IndexRecord( offset / 2, nbyte / 2 );

      // write recordheader to the bytearray
      ByteUtils.writeBEInt( bytearray, 0, i );
      ByteUtils.writeBEInt( bytearray, 4, nbyte / 2 );

      //write record (bytearray) including recordheader to the shape file
      shp.write( bytearray, record, mbr );

      // actualise shape file minimum boundary rectangle
      if( mbr.west < shpmbr.west )
      {
        shpmbr.west = mbr.west;
      }

      if( mbr.east > shpmbr.east )
      {
        shpmbr.east = mbr.east;
      }

      if( mbr.south < shpmbr.south )
      {
        shpmbr.south = mbr.south;
      }

      if( mbr.north > shpmbr.north )
      {
        shpmbr.north = mbr.north;
      }

      // icrement offset for pointing at the end of the file
      offset += ( nbyte + ShapeConst.SHAPE_FILE_RECORD_HEADER_LENGTH );

      bytearray = null;
    }

    dbf.writeAllToFile();

    // Header schreiben
    shp.writeHeader( offset, shptype, shpmbr );

    Debug.debugMethodEnd();
    
  }
  
  /**
   * writes a OGC FeatureCollection to a ESRI shape file. <BR>
   * all features in the collection must have the same properties. <BR>
   */
  public void writeShape( FeatureCollection fc ) throws Exception
  {
    writeShape( fc.getAllFeatures() );
  }
}