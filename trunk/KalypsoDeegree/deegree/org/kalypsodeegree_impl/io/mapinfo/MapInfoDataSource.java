/*----------------    FILE HEADER  ------------------------------------------
 
 This class bases on the MapInfoDataSource class from the GeoTools project.
 Geotools - OpenSource mapping toolkit
 (C) 2002, Centre for Computational Geography

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
package org.deegree_impl.io.mapinfo;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.StringTokenizer;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureCollection;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.services.wfs.filterencoding.Filter;
import org.deegree.services.wfs.protocol.WFSQuery;
import org.deegree_impl.model.feature.FeatureFactory;
import org.deegree_impl.model.geometry.JTSAdapter;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.StringExtend;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.TopologyException;

/**
 * 
 * 
 * @version $Revision$
 * @author $author$
 */
public class MapInfoDataSource
{
  public static final String TYPE_NONE = "none";

  public static final String TYPE_POINT = "point";

  public static final String TYPE_LINE = "line";

  public static final String TYPE_PLINE = "pline";

  public static final String TYPE_REGION = "region";

  public static final String TYPE_ARC = "arc";

  public static final String TYPE_TEXT = "text";

  public static final String TYPE_RECT = "rectangle";

  public static final String TYPE_ROUNDRECT = "rounded rectangle";

  public static final String TYPE_ELLIPSE = "ellipse";

  public static final String CLAUSE_SYMBOL = "SYMBOL";

  public static final String CLAUSE_PEN = "PEN";

  public static final String CLAUSE_SMOOTH = "SMOOTH";

  public static final String CLAUSE_CENTER = "CENTER";

  public static final String CLAUSE_BRUSH = "BRUSH";

  public static final String CLAUSE_VERSION = "Version";

  public static final String CLAUSE_CHARSET = "Charset";

  public static final String CLAUSE_DELIMETER = "DELIMITER";

  public static final String CLAUSE_UNIQUE = "UNIQUE";

  public static final String CLAUSE_INDEX = "INDEX";

  public static final String CLAUSE_COLUMNS = "COLUMNS";

  private ArrayList hColumnsNames;

  private ArrayList hColumnsTypes;

  private ArrayList hIndex;

  private ArrayList hUnique;

  private FeatureType lineType = null;

  private FeatureType pointType = null;

  private FeatureType polygonType = null;

  // Factory to use to build Geometries
  private GeometryFactory geomFactory;

  private String filename;

  private String hCharset;

  private String hDelimeter = "\t";

  // Header information
  private String hVersion;

  // CoordsSys not supported
  // Transform not supported
  // Global variables (for the initial read)
  private String line; // The current Line of the MIF file.

  /**
   * Creates a new MapInfoDataSource object.
   * 
   * @param url
   * 
   * @throws java.net.MalformedURLException
   */
  public MapInfoDataSource( URL url ) throws java.net.MalformedURLException
  {
    try
    {
      filename = java.net.URLDecoder.decode( url.getFile(), "ISO-8859-1" );
    }
    catch( Exception e )
    {
      throw new java.net.MalformedURLException( e.toString() );
    }
    geomFactory = new GeometryFactory();
  }

  /**
   * Reads the MIF and MID files and returns a ArrayList of the Features they
   * contain
   * 
   * @return a ArrayList of features?
   * 
   * @throws Exception
   *           if file doesn't exist or is not readable etc
   */
  protected ArrayList readMifMid() throws Exception
  {
    if( filename == null )
    {
      throw new Exception( "Invalid filename passed to readMifMid" );
    }

    String mifFile = setExtension( filename, "MIF" );
    String midFile = setExtension( filename, "MID" );

    // Read files
    try
    {
      File mif = new File( mifFile );

      if( !mif.exists() )
      {
        mifFile = setExtension( filename, "mif" );
        mif = new File( mifFile );

        if( !mif.exists() )
        {
          mifFile = setExtension( filename.toLowerCase(), "mif" );
          mif = new File( mifFile );

          if( !mif.exists() )
          {
            mifFile = setExtension( filename.toUpperCase(), "MIF" );
            mif = new File( mifFile );
          } // and at that I'm out of guesses
        }
      }

      File mid = new File( midFile );

      if( !mid.exists() )
      {
        midFile = setExtension( filename, "mid" );
        mid = new File( midFile );

        if( !mid.exists() )
        {
          midFile = setExtension( filename.toLowerCase(), "mid" );
          mid = new File( midFile );

          if( !mid.exists() )
          {
            midFile = setExtension( filename.toUpperCase(), "MID" );
            mid = new File( midFile );
          } // and at that I'm out of guesses
        }
      }

      ArrayList features = readMifMid( new BufferedReader( new FileReader( mif ) ),
          new BufferedReader( new FileReader( mid ) ) );

      return features;
    }
    catch( FileNotFoundException fnfexp )
    {
      throw new Exception( "FileNotFoundException trying to read mif file : ", fnfexp );
    }
  }

  /**
   * 
   * 
   * @param filename
   * @param ext
   * 
   * @return
   */
  private String setExtension( String filename, String ext )
  {
    if( ext.indexOf( "." ) == -1 )
    {
      ext = "." + ext;
    }

    if( filename.lastIndexOf( "." ) == -1 )
    {
      return filename + ext;
    }

    return filename.substring( 0, filename.lastIndexOf( "." ) ) + ext;
  }

  /**
   * This private method constructs the factories used to create the Feature,
   * and Geometries as they are read It takes it's setup values from the value
   * of the COLUMNS clause in the MIF file
   * 
   * @throws Exception
   */
  private void setUpFactories() throws Exception
  {
    // Go through each column name, and set up an attribute for each one
    ArrayList colAttribs = new ArrayList( hColumnsNames.size() );

    // Add attributes for each column
    //Iterator it = hColumns.keySet().iterator();
    for( int i = 0; i < hColumnsNames.size(); i++ )
    {
      String type = ( (String)hColumnsTypes.get( i ) ).toLowerCase();
      Class typeClass = null;

      if( type.equals( "float" ) || type.startsWith( "decimal" ) )
      {
        typeClass = Double.class;
        hColumnsTypes.set( i, "Double" );
      }
      else if( type.startsWith( "char" ) )
      {
        typeClass = String.class;
        hColumnsTypes.set( i, "String" );
      }
      else if( type.equals( "integer" ) || type.equals( "smallint" ) )
      {
        typeClass = Integer.class;
        hColumnsTypes.set( i, "Integer" );
      }
      else
      {
        typeClass = String.class;
        hColumnsTypes.set( i, "String" );
      }

      FeatureTypeProperty ftp = FeatureFactory.createFeatureTypeProperty( (String)hColumnsNames
          .get( i ), typeClass.getName(), true );
      colAttribs.add( ftp );
    }

    FeatureTypeProperty ftp = FeatureFactory.createFeatureTypeProperty( "GEOM",
        "org.deegree.model.geometry.GM_Object", true );

    // Add default Geometry attribute type
    colAttribs.add( 0, ftp );

    // create point feature Type & factory
    try
    {
      FeatureTypeProperty[] ftps = (FeatureTypeProperty[])colAttribs
          .toArray( new FeatureTypeProperty[colAttribs.size()] );
      pointType = FeatureFactory.createFeatureType( null, null, filename.toString() + "_point",
          ftps );
    }
    catch( Exception schexp )
    {
      throw new Exception( "SchemaException setting up point factory : ", schexp );
    }

    // Set up Line factory
    // Add default attribute type
    ftp = FeatureFactory.createFeatureTypeProperty( "GEOM", "org.deegree.model.geometry.GM_Object",
        true );
    colAttribs.set( 0, ftp );

    // create line feature Type & factory
    try
    {
      FeatureTypeProperty[] ftps = (FeatureTypeProperty[])colAttribs
          .toArray( new FeatureTypeProperty[colAttribs.size()] );
      lineType = FeatureFactory.createFeatureType( null, null, filename.toString() + "_line", ftps );
    }
    catch( Exception schexp )
    {
      throw new Exception( "SchemaException setting up line factory : ", schexp );
    }

    // Set up Polygon factory
    // Add default attribute type
    ftp = FeatureFactory.createFeatureTypeProperty( "GEOM", "org.deegree.model.geometry.GM_Object",
        true );
    colAttribs.set( 0, ftp );

    // create polygon feature Type & factory
    try
    {
      FeatureTypeProperty[] ftps = (FeatureTypeProperty[])colAttribs
          .toArray( new FeatureTypeProperty[colAttribs.size()] );
      polygonType = FeatureFactory.createFeatureType( null, null, filename.toString() + "_poly",
          ftps );
    }
    catch( Exception schexp )
    {
      throw new Exception( "SchemaException setting up polygon factory : ", schexp );
    }
  }

  /**
   * Reads an entire MID/MIF file. (Two files, actually, separately opened)
   * 
   * @param mifReader
   *          An opened BufferedReader to the MIF file.
   * @param midReader
   *          An opened BufferedReader to the MID file.
   * 
   * @return @throws
   *         Exception
   */
  private ArrayList readMifMid( BufferedReader mifReader, BufferedReader midReader )
      throws Exception
  {
    // Read the MIF header
    readMifHeader( mifReader );

    // Set up factories
    setUpFactories();

    ArrayList features = new ArrayList();

    // Start by reading first line
    try
    {
      line = readMifLine( mifReader );
    }
    catch( IOException ioexp )
    {
      throw new Exception( "No data at start of file", ioexp );
    }

    Feature feature;

    // Read each object in the MIF file
    while( ( feature = readObject( mifReader, midReader ) ) != null )
    {
      // Figure out which type of feature it is
      // Add to relevent ArrayList
      features.add( feature );
    }

    return features;
  }

  /**
   * Reads the header from the given MIF file stream
   * 
   * @param mifReader
   * 
   * @throws Exception
   */
  private void readMifHeader( BufferedReader mifReader ) throws Exception
  {
    try
    {
      while( ( readMifLine( mifReader ) != null ) && !line.trim().equalsIgnoreCase( "DATA" ) )
      {
        if( clause( line ).equalsIgnoreCase( CLAUSE_VERSION ) )
        {
          // Read Version clause
          hVersion = line.trim().substring( line.trim().indexOf( ' ' ) ).trim();
          Debug.debugSimpleMessage( "version [" + hVersion + "]" );
        }

        if( clause( line ).equalsIgnoreCase( CLAUSE_CHARSET ) )
        {
          // Read Charset clause
          //hCharset = line.replace('\"','
          // ').trim().substring(line.trim().indexOf(' ')).trim();
          hCharset = remainder( line ).replace( '"', ' ' ).trim();
          Debug.debugSimpleMessage( "Charset [" + hCharset + "]" );
        }

        if( clause( line ).equalsIgnoreCase( CLAUSE_DELIMETER ) )
        {
          // Read Delimeter clause
          hDelimeter = line.replace( '\"', ' ' ).trim().substring( line.trim().indexOf( ' ' ) )
              .trim();
          Debug.debugSimpleMessage( "delimiter [" + hDelimeter + "]" );
        }

        if( clause( line ).equalsIgnoreCase( CLAUSE_UNIQUE ) )
        {
          // Read Unique clause
          StringTokenizer st = new StringTokenizer( line.trim().substring(
              line.trim().indexOf( ' ' ) ), "," );
          hUnique = new ArrayList();
          Debug.debugSimpleMessage( "Unique cols " );

          while( st.hasMoreTokens() )
          {
            String uniq = st.nextToken();
            Debug.debugSimpleMessage( "\t" + uniq );
            hUnique.add( uniq );
          }
        }

        if( clause( line ).equalsIgnoreCase( CLAUSE_INDEX ) )
        {
          // Read Index clause
          StringTokenizer st = new StringTokenizer( line.trim().substring(
              line.trim().indexOf( ' ' ) ), "," );
          hIndex = new ArrayList( st.countTokens() );
          Debug.debugSimpleMessage( "Indexes" );

          while( st.hasMoreTokens() )
          {
            String index = st.nextToken();
            Debug.debugSimpleMessage( "\t" + index );
            hIndex.add( index );
          }
        }

        if( clause( line ).equalsIgnoreCase( CLAUSE_COLUMNS ) )
        {
          // Read Columns clause
          int cols = 0;

          try
          {
            cols = Integer.parseInt( remainder( line ) );
          }
          catch( NumberFormatException nfexp )
          {
            Debug.debugException( nfexp, "bad number of colums " );
          }

          // Read each of the columns
          hColumnsNames = new ArrayList( cols );
          hColumnsTypes = new ArrayList( cols );

          for( int i = 0; i < cols; i++ )
          {
            line = readMifLine( mifReader );

            //StringTokenizer st = new
            // StringTokenizer(line.trim().substring(line.trim().indexOf('
            // ')), " ");
            String name = clause( line );
            String value = remainder( line );

            Debug.debugSimpleMessage( "column name " + name + " value " + value );

            hColumnsNames.add( name );
            hColumnsTypes.add( value );
          }
        }
      }
    }
    catch( IOException ioexp )
    {
      throw new Exception( "IOException reading MIF header : " + ioexp.getMessage() );
    }
  }

  /**
   * A 'Clause' is stored as a single string at the start of a line. This rips
   * the clause name out of the given line.
   * 
   * @param line
   * 
   * @return
   */
  private String clause( String line )
  {
    return clause( line, ' ' );
  }

  /**
   * 
   * 
   * @param line
   * @param delimiter
   * 
   * @return
   */
  private String clause( String line, char delimiter )
  {
    line = line.trim();

    int index = line.indexOf( delimiter );

    if( index == -1 )
    {
      return line;
    }
    else
    {
      return line.substring( 0, index ).trim();
    }
  }

  /**
   * returns the last word of the string
   * 
   * @param line
   * 
   * @return
   */
  private String remainder( String line )
  {
    return remainder( line, ' ' );
  }

  /**
   * 
   * 
   * @param line
   * @param delimiter
   * 
   * @return
   */
  private String remainder( String line, char delimiter )
  {
    line = line.trim();

    int index = line.lastIndexOf( delimiter );

    if( index == -1 )
    {
      return "";
    }
    else
    {
      return line.substring( index ).trim();
    }
  }

  /**
   * Reads the next line in the reader, ignoring lines which are nothing but
   * whitespace. Sets the global 'line' variable to the currently read line
   * 
   * @param reader
   * 
   * @return @throws
   *         IOException
   * @throws Exception
   */
  private String readMifLine( BufferedReader reader ) throws IOException, Exception
  {
    do
    {
      line = reader.readLine();

      if( line == null )
      {
        return null;
      }

      if( isShadingClause( line ) )
      {
        Debug.debugSimpleMessage( "going to process shading" );

        //processShading(line);
        line = " ";
      }
    }
    while( line.trim().length() == 0 );

    line = line.trim();

    //Debug.debugSimpleMessage("returning line " + line);
    return line;
  }

  /**
   * Reads a single MIF Object (Point, Line, Region, etc.) as a Feature
   * 
   * @param mifReader
   * @param midReader
   * 
   * @return @throws
   *         Exception
   */
  private Feature readObject( BufferedReader mifReader, BufferedReader midReader ) throws Exception
  {
    Feature feature = null;

    //Debug.debugSimpleMessage("line = " + line);
    // examine The current line
    if( line == null )
    {
      return null;
    }

    int index = line.indexOf( ' ' );

    if( index == -1 )
    {
      index = line.length();
    }

    if( line.substring( 0, index ).equalsIgnoreCase( TYPE_POINT ) )
    {
      // Read point data
      feature = readPointObject( mifReader, midReader );
    }
    else if( line.substring( 0, index ).equalsIgnoreCase( TYPE_LINE ) )
    {
      // Read line data
      feature = readLineObject( mifReader, midReader );
    }
    else if( line.substring( 0, index ).equalsIgnoreCase( TYPE_PLINE ) )
    {
      // Read pline data
      feature = readPLineObject( mifReader, midReader );
    }
    else if( line.substring( 0, index ).equalsIgnoreCase( TYPE_REGION ) )
    {
      // Read region data
      feature = readRegionObject( mifReader, midReader );
    }
    else
    {
      Debug.debugSimpleMessage( line + " unknown object in mif reader" );
    }

    return feature;
  }

  /**
   * Reads Point information from the MIF stream
   * 
   * @param mifReader
   * @param midReader
   * 
   * @return @throws
   *         Exception
   */
  private Feature readPointObject( BufferedReader mifReader, BufferedReader midReader )
      throws Exception
  {
    Feature feature = null;

    StringTokenizer st = new StringTokenizer( line.substring( line.indexOf( " " ) ), "," );

    try
    {
      double x = Double.parseDouble( st.nextToken() );
      double y = Double.parseDouble( st.nextToken() );

      // Construct Geomtry
      Geometry pointGeom = geomFactory.createPoint( new Coordinate( x, y ) );

      // Read next line
      readMifLine( mifReader );

      //Hashtable shading = readShading(mifReader);
      // Shading is not included, as null feature attributes are not
      // supported yet
      ArrayList midValues = readMid( midReader );

      //			midValues.putAll(shading);
      // Create Feature
      feature = buildFeature( pointType, pointGeom, midValues );

      Debug.debugSimpleMessage( "Built point feature : " + x + " " + y );
    }
    catch( NumberFormatException nfexp )
    {
      throw new Exception( "Exception reading Point data from MIF file : ", nfexp );
    }
    catch( IOException ioexp )
    {
      throw new Exception( "IOException reading point data : ", ioexp );
    }

    return feature;
  }

  /**
   * Reads Line information from the MIF stream
   * 
   * @param mifReader
   * @param midReader
   * 
   * @return @throws
   *         Exception
   */
  private Feature readLineObject( BufferedReader mifReader, BufferedReader midReader )
      throws Exception
  {
    Feature feature = null;

    StringTokenizer st = new StringTokenizer( line.substring( line.indexOf( " " ) ), "," );

    try
    {
      double x1 = Double.parseDouble( st.nextToken() );
      double y1 = Double.parseDouble( st.nextToken() );
      double x2 = Double.parseDouble( st.nextToken() );
      double y2 = Double.parseDouble( st.nextToken() );

      // Construct Geomtry
      Coordinate[] cPoints = new Coordinate[2];
      cPoints[0] = new Coordinate( x1, y1 );
      cPoints[1] = new Coordinate( x2, y2 );

      Geometry lineGeom = geomFactory.createLineString( cPoints );

      // Read next line
      readMifLine( mifReader );

      //Hashtable shading = readShading(mifReader);
      // Shading is not included, as null feature attributes are not
      // supported yet
      ArrayList midValues = readMid( midReader );

      //			midValues.putAll(shading);
      // Create Feature
      feature = buildFeature( lineType, lineGeom, midValues );

      Debug.debugSimpleMessage( "Built line feature : " + x1 + " " + y1 + " - " + x2 + " " + y2 );
    }
    catch( NumberFormatException nfexp )
    {
      throw new Exception( "Exception reading Point data from MIF file : " + nfexp.getMessage() );
    }
    catch( IOException ioexp )
    {
      throw new Exception( "IOException reading point data : " + ioexp.getMessage() );
    }

    return feature;
  }

  /**
   * Reads Multi-Line (PLine) information from the MIF stream
   * 
   * @param mifReader
   * @param midReader
   * 
   * @return @throws
   *         Exception
   */
  private Feature readPLineObject( BufferedReader mifReader, BufferedReader midReader )
      throws Exception
  {
    Feature feature = null;

    StringTokenizer st = new StringTokenizer( line.substring( line.indexOf( " " ) ) );

    try
    {
      int numsections = 1;

      if( st.hasMoreTokens() && st.nextToken().trim().equalsIgnoreCase( "MULTIPLE" ) )
      {
        numsections = Integer.parseInt( st.nextToken() );
      }

      // A ArrayList of coordinates
      ArrayList coords = new ArrayList( numsections );

      // Read each polygon
      for( int i = 0; i < numsections; i++ )
      {
        // Read line (number of points
        int numpoints = Integer.parseInt( readMifLine( mifReader ) );

        // Read each point
        for( int p = 0; p < numpoints; p++ )
        {
          StringTokenizer pst = new StringTokenizer( readMifLine( mifReader ) );
          double x = Double.parseDouble( pst.nextToken() );
          double y = Double.parseDouble( pst.nextToken() );
          coords.add( new Coordinate( x, y ) );
        }
      }

      Geometry plineGeom = geomFactory.createLineString( (Coordinate[])coords
          .toArray( new Coordinate[coords.size()] ) );

      // Read next line
      readMifLine( mifReader );

      //Hashtable shading = readShading(mifReader);
      // Shading is not included, as null feature attributes are not
      // supported yet
      ArrayList midValues = readMid( midReader );

      //			midValues.putAll(shading);
      // Create Feature
      feature = buildFeature( lineType, plineGeom, midValues );

      Debug.debugSimpleMessage( "Read polyline (" + coords.size() + ")" );
    }
    catch( NumberFormatException nfexp )
    {
      throw new Exception( "Exception reading Point data from MIF file : " + nfexp.getMessage() );
    }
    catch( IOException ioexp )
    {
      throw new Exception( "IOException reading point data : " + ioexp.getMessage() );
    }

    return feature;
  }

  /**
   * Reads Region (Polygon) information from the MIF stream
   * 
   * @param mifReader
   * @param midReader
   * 
   * @return @throws
   *         Exception
   */
  private Feature readRegionObject( BufferedReader mifReader, BufferedReader midReader )
      throws Exception
  {
    Feature feature = null;

    StringTokenizer st = new StringTokenizer( line.substring( line.indexOf( " " ) ) );

    try
    {
      int numpolygons = Integer.parseInt( st.nextToken() );

      // A ArrayList of polygons
      ArrayList polys = new ArrayList( numpolygons );

      // Read each polygon
      for( int i = 0; i < numpolygons; i++ )
      {
        // Read number of points
        int numpoints = Integer.parseInt( readMifLine( mifReader ) );
        ArrayList coords = new ArrayList( numpoints );

        // Read each point
        for( int p = 0; p < numpoints; p++ )
        {
          StringTokenizer pst = new StringTokenizer( readMifLine( mifReader ) );
          double x = Double.parseDouble( pst.nextToken() );
          double y = Double.parseDouble( pst.nextToken() );
          coords.add( new Coordinate( x, y ) );
        }

        // Create polygon from points
        coords.add( new Coordinate( ( (Coordinate)coords.get( 0 ) ).x,
            ( (Coordinate)coords.get( 0 ) ).y ) );

        try
        {
          Polygon pol = geomFactory.createPolygon( geomFactory
              .createLinearRing( (Coordinate[])coords.toArray( new Coordinate[coords.size()] ) ),
              null );

          // Add to ArrayList
          polys.add( pol );
        }
        catch( TopologyException topexp )
        {
          throw new Exception( "TopologyException reading Region polygon : ", topexp );
        }
      }

      Geometry polyGeom = geomFactory.createMultiPolygon( (Polygon[])polys
          .toArray( new Polygon[polys.size()] ) );

      // Read next line
      readMifLine( mifReader );

      //Hashtable shading = readShading(mifReader);
      // Shading is not included, as null feature attributes are not
      // supported yet
      ArrayList midValues = readMid( midReader );

      //			midValues.putAll(shading);
      // Create Feature
      feature = buildFeature( polygonType, polyGeom, midValues );

      Debug.debugSimpleMessage( "Read Region (" + polys.size() + ")" );
    }
    catch( NumberFormatException nfexp )
    {
      throw new Exception( "Exception reading Point data from MIF file : ", nfexp );
    }
    catch( IOException ioexp )
    {
      throw new Exception( "IOException reading point data : ", ioexp );
    }

    return feature;
  }

  /**
   * Builds a complete Feature object using the given FeatureType, with the
   * Geometry geom, and the given attributes.
   * 
   * @param featureType
   *          The FeatureType to use to constuct the Feature
   * @param geom
   *          The Geometry to use as the default Geometry
   * @param attribs
   *          The attibutes to use as the Feature's attributes (Attributes must
   *          be set up in the FeatureType)
   * 
   * @return A fully-formed Feature
   * 
   * @throws Exception
   */
  private Feature buildFeature( FeatureType featureType, Geometry geom, ArrayList attribs )
      throws Exception
  {
    int numAttribs = featureType.getProperties().length;

    // add geometry to the attributes
    attribs.add( 0, JTSAdapter.wrap( geom ) );

    if( numAttribs != attribs.size() )
    {
      throw new Exception( "wrong number of attributes passed to buildFeature.\n" + "expected "
          + numAttribs + " got " + attribs.size() );
    }

    // Create Feature
    try
    {
      return FeatureFactory.createFeature( "id", featureType, attribs.toArray() );
    }
    catch( Exception ifexp )
    {
      throw new Exception( "Exception creating feature : ", ifexp );
    }
  }

  /**
   * Reads a single line of the given MID file stream, and returns a hashtable
   * of the data in it, keyed byt he keys in the hColumns hash
   * 
   * @param midReader
   * 
   * @return @throws
   *         Exception
   */
  private ArrayList readMid( BufferedReader midReader ) throws Exception
  {
    ArrayList midValues = new ArrayList();

    if( midReader == null )
    {
      return new ArrayList();
    }

    // The delimeter is a single delimiting character
    String midLine = "";

    try
    {
      midLine = midReader.readLine();
      Debug.debugSimpleMessage( "Read MID " + midLine );
    }
    catch( IOException ioexp )
    {
      throw new Exception( "IOException reading MID file" );
    }

    // read MID tokens
    int col = 0;
    StringTokenizer quotes = new StringTokenizer( midLine, "\"" );

    while( quotes.hasMoreTokens() )
    {
      StringTokenizer delimeters = new StringTokenizer( quotes.nextToken(), hDelimeter + "\0" );

      // Read each delimited value into the ArrayList
      while( delimeters.hasMoreTokens() )
      {
        String token = delimeters.nextToken();
        String type = (String)hColumnsTypes.get( col++ );
        addAttribute( type, token, midValues );
      }

      // Store the whole of the next bit (it's a quoted string)
      if( quotes.hasMoreTokens() )
      {
        String token = quotes.nextToken();
        String type = (String)hColumnsTypes.get( col++ );
        addAttribute( type, token, midValues );
        //Debug.debugSimpleMessage("adding " + token);
      }
    }

    return midValues;
  }

  /**
   * 
   * 
   * @param type
   * @param token
   * @param midValues
   */
  private void addAttribute( String type, String token, ArrayList midValues )
  {
    if( type.equals( "String" ) )
    {
      midValues.add( token );
    }
    else if( type.equals( "Double" ) )
    {
      try
      {
        token = StringExtend.validateString( token, "," );
        midValues.add( new Double( token ) );
      }
      catch( NumberFormatException nfe )
      {
        Debug.debugException( nfe, "Bad double " + token );
        midValues.add( new Double( 0.0 ) );
      }
    }
    else if( type.equals( "Integer" ) )
    {
      try
      {
        //token = StringExtend.validateString( token, "," );
        midValues.add( new Integer( token ) );
      }
      catch( NumberFormatException nfe )
      {
        Debug.debugException( nfe, "Bad integer " + token );
        midValues.add( new Integer( 0 ) );
      }
    }
    else
    {
      Debug.debugSimpleMessage( "Unknown type " + type );
    }
  }

  /**
   * Reads the shading information at the end of Object data
   * 
   * @param line
   * 
   * @throws Exception
   */

  //    private void processShading(String line) throws Exception {
  //        int color;
  //        int r;
  //        int g;
  //        int b;
  //        
  //        if (line == null) {
  //            return;
  //        }
  //        
  //        String shadeType = line.toLowerCase();
  //        String name = clause(shadeType, '(');
  //        String settings = remainder(shadeType, '(');
  //        StringTokenizer st = new StringTokenizer(settings, "(),");
  //        String[] values = new String[st.countTokens()];
  //        
  //        for (int i = 0; st.hasMoreTokens(); i++) {
  //            values[i] = st.nextToken();
  //        }
  //        
  //        if (name.equals("pen")) {
  //            try {
  //                
  //                stroke.setWidth(filterFactory.createLiteralExpression(new
  // Integer(values[0])));
  //                
  //                int pattern = Integer.parseInt(values[1]);
  //                                
  //                stroke.setDashArray(MifStyles.getPenPattern(new Integer(pattern)));
  //                color = Integer.parseInt(values[2]);
  //                
  //                String rgb = Integer.toHexString(color);
  //                                
  //                stroke.setColor(filterFactory.createLiteralExpression(rgb));
  //            } catch (Exception nfe) {
  //                throw new Exception("Error setting up pen", nfe);
  //            }
  //            
  //            return;
  //        } else if (name.equals("brush")) {
  //            Debug.debugSimpleMessage("setting new brush " + settings);
  //            
  //            int pattern = Integer.parseInt(values[0]);
  //            Debug.debugSimpleMessage("pattern = " + pattern);
  //            
  //            Graphic dg = styleFactory.getDefaultGraphic();
  //            dg.addExternalGraphic(MifStyles.getBrushPattern(new Integer(pattern)));
  //            stroke.setGraphicFill(dg);
  //            color = Integer.parseInt(values[1]);
  //            
  //            String rgb = Integer.toHexString(color);
  //            Debug.debugSimpleMessage("color " + color + " -> " + rgb);
  //            fill.setColor(filterFactory.createLiteralExpression(rgb)); // foreground
  //            
  //            if (values.length == 3) { // optional parameter
  //                color = Integer.parseInt(values[2]);
  //                rgb = Integer.toHexString(color);
  //                Debug.debugSimpleMessage("color " + color + " -> " + rgb);
  //                
  //                fill.setBackgroundColor(filterFactory.createLiteralExpression(rgb)); //
  // background
  //            } else {
  //                fill.setBackgroundColor((Expression) null);
  //            }
  //        } else if (name.equals("center")) {
  //            Debug.debugSimpleMessage("setting center " + settings);
  //        } else if (name.equals("smooth")) {
  //            Debug.debugSimpleMessage("setting smooth on");
  //        } else if (name.equals("symbol")) {
  //            Debug.debugSimpleMessage("setting symbol " + settings);
  //            
  //            Mark symbol = null;
  //            ExternalGraphic eg = null;
  //            
  //            if (values.length == 3) { // version 3.0
  //                
  //                //symbol = symbols.get(new Integer(symNumb));
  //            } else if (values.length == 6) {}
  //            else if (values.length == 4) { // custom bitmap
  //                eg = styleFactory.createExternalGraphic("CustSymb/" +
  // values[0],"image/unknown"); // hack!
  //                
  //            } else {
  //                LOGGER.info("unexpected symbol style " + name + settings);
  //            }
  //        } else if (name.equals("font")) {
  //            Debug.debugSimpleMessage("setting font " + settings);
  //        } else {
  //            Debug.debugSimpleMessage("unknown styling directive " + name + settings);
  //        }
  //        
  //        return;
  //    }
  /**
   * Test whether the given line contains a known shading clause keyword (PEN,
   * STYLE, etc.)
   * 
   * @param line
   * 
   * @return
   */
  private boolean isShadingClause( String line )
  {
    line = line.toUpperCase();

    boolean ret = ( ( line.indexOf( CLAUSE_PEN ) != -1 ) || ( line.indexOf( CLAUSE_SYMBOL ) != -1 )
        || ( line.indexOf( CLAUSE_SMOOTH ) != -1 ) || ( line.indexOf( CLAUSE_CENTER ) != -1 ) || line
        .indexOf( CLAUSE_BRUSH ) != -1 );

    return ret;
  }

  /**
   * Loads features from the datasource into the passed collection, based on the
   * passed filter. Note that all data sources must support this method at a
   * minimum.
   * 
   * @param collection
   *          The collection to put the features into.
   * @param query
   *          contains info about request of which features to retrieve.
   * 
   * @throws Exception
   *           For all data source errors.
   */
  public void getFeatures( FeatureCollection collection, WFSQuery query ) throws Exception
  {
    Filter filter = null;

    if( query != null )
    {
      filter = query.getFilter();
    }

    ArrayList features = readMifMid();

    for( int i = 0; i < features.size(); i++ )
    {
      if( ( filter == null ) || filter.evaluate( (Feature)features.get( i ) ) )
      {
        collection.appendFeature( (Feature)features.get( i ) );
      }
    }
  }

  /**
   * Retrieves the featureType that features extracted from this datasource will
   * be created with.
   * 
   * @tasks TODO: implement this method.
   */
  public FeatureType getSchema()
  {
    return null;
  }
}