/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.interpolation.mesh;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.StreamTokenizer;
import java.util.HashMap;
import java.util.List;
import java.util.StringTokenizer;
import java.util.Vector;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.apache.xpath.XPathAPI;
import org.deegree_impl.services.NotSupportedFormatException;
import org.kalypso.java.io.FileUtilities;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import com.vividsolutions.jts.algorithm.CGAlgorithms;

/**
 * KalypsoFileParser
 * <p>
 * 
 * created by
 * 
 * @author kuepfer (27.05.2005)
 */
public class MeshReader
{
  private GM_Surface m_meshEnv;

  private PointTable m_points = null;

  private HashMap m_hasMapPoints = new HashMap();

  private ElementTable m_elements = null;

  private boolean m_initialise = false;

  protected Mesh importMesh( Mesh mesh, File[] files, CS_CoordinateSystem cs, GM_Surface wishbox, String type,
      String shapeBase )
  {
    File nodefile = null;
    File elementfile = null;
    File attributefile = null;
    File gmlfile = null;
    Vector shapefile = new Vector();
    int counter = 0;
    GM_Surface finalWishbox = null;
    try
    {
      //read border line
      if( shapeBase != null )
      {
        GM_Object border = readBorder( shapeBase, cs );

        if( border instanceof GM_Surface )
          mesh.setBorderLine( (GM_Surface)border );
        else if( border instanceof GM_Polygon )
        {

          mesh.setBorderLine( GeometryFactory.createGM_Surface( ( (GM_Polygon)border ).getExteriorRing(), null, null,
              cs ) );
        }
        else
          throw new Exception( "the border line is of type " + border.getClass().toString()
              + " and not as required of type " + GM_Surface.class.getName() + " \nor " + GM_Polygon.class.getName()
              + ".\nProgram aborted!" );
        //kleinstes Polygon um die elemente zu suchen
        if( wishbox == null )
          finalWishbox = mesh.getBorderLine();
        else
          finalWishbox = (GM_Surface)border.intersection( wishbox );
      }
      String fileName;
      //for each file
      for( int i = 0; i < files.length; i++ )
      {
        fileName = files[i].getName();

        // node-file
        if( fileName.matches( ".+\\.(n|N)(o|O)(d|D)(e|E)$" ) )
          nodefile = files[i];
        //element file
        if( fileName.matches( ".+\\.(e|E)(l|L)(e|E)$" ) )
          elementfile = files[i];
        //attribut file containing elevations
        if( fileName.matches( ".+\\.(d|D)(a|A)(t|T)$" ) )
          attributefile = files[i];
        //gml file containing set of polygons/elements
        if( fileName.matches( ".+\\.(g|G)(m|M)(l|L)$" ) )
          gmlfile = files[i];
        if( fileName.matches( ".+\\.(s|S)(h|H)(p|P)$" ) )
        {
          shapefile.add( files[i] );
        }
      }//for i
      if( gmlfile == null && shapefile.size() == 0 )
      {
        System.out.println( "Starting Import.." );
        //reads node file
        m_points = createNodes( nodefile, attributefile, cs );

        createElements( elementfile, cs, mesh, finalWishbox );

        System.out.println( "Import complete -> net created." );
        m_points = null;
      }
      else if( gmlfile != null )
      {

        //read gml file
        System.out.print( "\n" + "Importing gml file.." );
        readGmlFile( gmlfile, cs, mesh );
        System.out.print( "finished" );

        System.out.print( "Import complete -> net created." );
      }
      else if( shapefile.size() > 0 )
      {
        System.out.print( "\n" + "Importing shp file.." );
        //TODO property muss angegeben werden
        String property = "FLIESSTIEFE";
        readShapeFiles( mesh, shapefile, property, cs, wishbox );
      }
      System.out.print( "finished" );

      System.out.print( "Import complete -> net created." );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      System.exit( 0 );
    }
    mesh.setGridInitialize( m_initialise );
    return mesh;
  }

  private PointTable createNodes( File nodefile, File attributefile, CS_CoordinateSystem crs ) throws IOException
  {
    PointTable pt = new PointTable();
//    double xmin = 0;
//    double xmax = 0;
//    double ymin = 0;
//    double ymax = 0;

    System.out.println( "Reading Nodes.." );
    try
    {
      //reads the file into string tokernizer
      StreamTokenizer st = new StreamTokenizer( new InputStreamReader( new FileInputStream( nodefile ) ) );
      st.parseNumbers(); // sets the tokenizer to parse for numbers
      st.nextToken();
      st.nextToken();
      st.nextToken();
      st.nextToken();
      boolean firstLine = true;
      //till endoffile
      while( st.nextToken() != StreamTokenizer.TT_EOF )
      {
        String pointID = String.valueOf( (int)st.nval );
        st.nextToken();
        double xcor = st.nval;
        st.nextToken();
        double ycor = st.nval;
        st.nextToken();
//        if( firstLine == true )
//        {
//          xmin = xcor;
//          xmax = xcor;
//          ymin = ycor;
//          ymax = ycor;
//          firstLine = false;
//        }
//        else
//        {
//          if( xcor < xmin )
//            xmin = xcor;
//          else if( xcor > xmax )
//            xmax = xcor;
//
//          if( ycor < ymin )
//            ymin = ycor;
//          else if( ycor > ymax )
//            ymax = ycor;
//        }
        //create a point from x,y coordinate and with dummy initial
        // elevation attribute
        Point p = new Point( pointID, xcor, ycor, crs );
        pt.addPoint( p );
      }//while
    }//try
    catch( Exception e )
    {
      System.out.println( "Exception: " + e.toString() );
    }
    //assigning BoundigBox of Mesh
    try
    {
//      m_meshEnv = GeometryFactory.createGM_Surface( GeometryFactory.createGM_Envelope( xmin, ymin, xmax, ymax ), crs );

      System.out.print( pt.size() + " points created." );
      System.out.println( "Bounding Box of the Net: " + m_meshEnv );
      System.out.println( "Reading Attributes.." );
      //reads file into streamtokenizer
      StreamTokenizer st = new StreamTokenizer( new InputStreamReader( new FileInputStream( attributefile ) ) );
      st.parseNumbers(); // sets the tonenizer to parse for numbers
      st.nextToken();
      int nrOfRows = (int)st.nval;
      //till endoffile
      while( st.nextToken() != StreamTokenizer.TT_EOF )
      {
        //reads the ID
        String pointID = String.valueOf( (int)( st.nval                                                                                                   ) );
        st.nextToken();
        //reads elevation value
        Double elevation = new Double( st.nval );
        //creates FeatureProperty with the elevation value
        Point p = pt.getPoint( pointID );
        if( p != null )
          p.addAttribute( elevation, true );
        else
          throw new Exception( "Point does not exist!" );
      }//while
    }//try
    catch( Exception e )
    {
      System.out.println( "Exception : " + e.toString() );
    }//catch

    System.out.print( "finished" );

    return pt;

  }//createNodes

  private void createElements( File elementfile, CS_CoordinateSystem crs, Mesh mesh, GM_Surface wishbox )
      throws Exception
  {
    if( wishbox != null && wishbox.contains( m_meshEnv ) )
      mesh.setBoundingBox( m_meshEnv );
    //    GM_Surface bbox = GeometryFactory.createGM_Surface( wishbox, crs );
    System.out.println( "Reading Elements.." );
    //reads file into streamtokenizer
    BufferedReader bEleFReader = new BufferedReader( new InputStreamReader( new FileInputStream( elementfile ) ) );
    StringTokenizer st;
    String eleLine = bEleFReader.readLine();
    if( eleLine == null )
    {
      throw new Exception( "Element file not well formatted." );
    }
    else
    {
      st = new StringTokenizer( eleLine );
    }

    String elementID = null;
    Element e = null, e1 = null, e2 = null;
    Point p = null;
    for( eleLine = bEleFReader.readLine(); eleLine != null; eleLine = bEleFReader.readLine() )
    {
      String vertList = null;
      st = new StringTokenizer( eleLine );
      //st.parseNumbers(); // sets the tonenizer to parse for numbers
      int totalNodes = st.countTokens() - 1;//-1 was not there, but
      // changed later

      if( totalNodes == 3 || totalNodes == 4 )
      {//check element is
        // triangle or
        // parallelogram and
        // nothing else
        if( st.hasMoreTokens() )
        {
          elementID = st.nextToken();
        }
        int nodeCounter = 0;
        GM_Position[] verticies = new GM_Position[totalNodes + 1];
        double[] pointarray = new double[totalNodes + 1];
        while( st.hasMoreTokens() )
        {
          String token = st.nextToken();
          p = m_points.getPoint( token );
          verticies[nodeCounter] = p.getPosition();
          double value = p.getAttribute().doubleValue();
          pointarray[nodeCounter] = value;
          nodeCounter++;
          if( value <= 0.0 )
            m_initialise = true;
        }
        //*System.out.println("nodeocunter:" + nodeCounter);
        if( nodeCounter == 3 )
        {// its element is triangle
          //First vertex equals last vertex - > defines a closed
          // element!
          verticies[3] = verticies[0];
          MeshElement me = new MeshElement( elementID, verticies, pointarray, crs );
          if( wishbox == null )
            mesh.addElement( me );
          else if( me.getGeometry().intersects( wishbox ) )
            mesh.addElement( me );
        }
        if( nodeCounter == 4 )
        {//if element is parallelogram then
          // split that into 2 triangles
          verticies[4] = verticies[0];
          MeshElement me = new MeshElement( elementID, verticies, pointarray, crs );
          int pType = me.getPolygonType();//get type of
          // parallelogram
          // (Convex or Concave)
          if( pType == Element.CONCAVE_POLYGON )
          {//if its Concave,
            // then its
            // not supported
            throw new Exception( "Concave polygon import is not supported yet." );
          }
          else
          {//otherwise if its Convex then split it based on
            // diagonal slope and keep it least
            MeshElement[] splitElements = me.splitElement();

            for( int i = 0; i < splitElements.length; i++ )
            {
              MeshElement element = splitElements[i];
              if( wishbox == null )
                mesh.addElement( element );
              else if( wishbox.contains( element.getGeometry() ) )
                mesh.addElement( element );
            }
          }
        }
      }//if 3 || 4
      else
      {
        throw new Exception( "Element having less than 3 or more than 4 nodes is not supported." );
      }
    }//for
    System.out.print( " finished" );
    System.out.println( "Total MeshElements read: " + mesh.size() );

  }//createElements

  private void readGmlFile( File gmlfile, CS_CoordinateSystem cs, Mesh mesh ) throws Exception
  {
    HashMap points = new HashMap();
    DocumentBuilder db;
    Document d = null;

    m_points = new PointTable();
    //    m_elements = new ElementTable();

    try
    {
      db = DocumentBuilderFactory.newInstance().newDocumentBuilder();
      //read gml file into dom document object
      d = db.parse( gmlfile );

    }
    catch( ParserConfigurationException pce )
    {
      System.out.println( "ParserConfigurationException: " + pce.toString() );
    }
    catch( IOException ioe )
    {
      System.out.println( "IOException:" + ioe.toString() );
    }
    catch( SAXException se )
    {
      System.out.println( "SAXException: " + se.toString() );
    }

    NodeList pointList = null;
    org.w3c.dom.Element root = d.getDocumentElement();

    Node curPoint;

    try
    {
      pointList = null;

      pointList = XPathAPI.selectNodeList( root, ".//featurePoint" );
    }
    catch( Exception exc )
    {
      System.out.println( exc.getMessage() );
      exc.printStackTrace();
    }

    if( pointList != null )
    {

      Node n;
      String pNr;
      //      Point p;
      double xcor, ycor, xmin = 0.0, ymin = 0.0, xmax = 0.0, ymax = 0.0;
      Double attribute;

      try
      {
        for( int i = 0; i < pointList.getLength(); i++ )
        {
          n = pointList.item( i );
          //System.out.println(XPathAPI.selectSingleNode(n,"feId/text()").getNodeValue());

          pNr = XPathAPI.selectSingleNode( n, ".//@fid" ).getNodeValue();

          System.out.println( "reading point :" + pNr );

          xcor = Double.parseDouble( XPathAPI.selectSingleNode( n, ".//X/text()" ).getNodeValue() );
          ycor = Double.parseDouble( XPathAPI.selectSingleNode( n, ".//Y/text()" ).getNodeValue() );

          attribute = new Double( XPathAPI.selectSingleNode( n, ".//riverDepth/text()" ).getNodeValue() );

          if( i == 0 )
          {
            xmin = xcor;
            xmax = xcor;
            ymin = ycor;
            ymax = ycor;
          }
          else
          {
            if( xcor < xmin )
              xmin = xcor;
            else if( xcor > xmax )
              xmax = xcor;

            if( ycor < ymin )
              ymin = ycor;
            else if( ycor > ymax )
              ymax = ycor;
          }
          //         p = new Point( pNr, attribute, xcor, ycor, cs );
          //         m_points.addPoint( p );
          points.put( String.valueOf( xcor ) + String.valueOf( ycor ), attribute );
        }
      }
      catch( Exception exception )
      {
        System.out.println( "Exception: " + exception.toString() );
      }

      //assigning BoundigBox of Mesh
      m_meshEnv = GeometryFactory.createGM_Surface( GeometryFactory.createGM_Envelope( xmin, ymin, xmax, ymax ), cs );
      //logWriter.newLine();
      System.out.println( "Total Nodes read: " + m_points.size() + " points created." );
      System.out.println( "Bounding Box of the Net: " + m_meshEnv.toString() );
    }

    NodeList eleList = null;

    try
    {
      eleList = null;
      eleList = XPathAPI.selectNodeList( root, ".//femMesh" );
    }
    catch( Exception exc )
    {
      System.out.println( exc.getMessage() );
      exc.printStackTrace();
    }

    if( eleList != null )
    {

      Node n;
      String pNr, eNr;
      //      Element e, e1, e2;
      MeshElement me;
      //      Point p;
      String outerBoundry, coordinates[];
      double xcor, ycor;

      try
      {
        for( int i = 0; i < eleList.getLength(); i++ )
        {
          n = eleList.item( i );

          eNr = XPathAPI.selectSingleNode( n, ".//@fid" ).getNodeValue();

          System.out.println( "reading element :" + eNr );

          outerBoundry = XPathAPI.selectSingleNode( n, ".//outerBoundaryIs/LinearRing/coordinates/text()" )
              .getNodeValue();
          coordinates = outerBoundry.split( " " );
          Point[] nodes = new Point[coordinates.length];
          Double a = null;

          int totalNodes = coordinates.length;
          int nodeCounter = 0;
          GM_Position[] verticies = new GM_Position[totalNodes];
          double[] values = new double[totalNodes];
          String vertIDs = null;
          if( totalNodes == 4 || totalNodes == 5 )
          {//check element is
            // triangle or
            // parallelogram and
            // nothing else
            for( int c = 0; c < totalNodes; c++ )
            {
              String[] pnt = coordinates[c].split( "," );
              xcor = Double.parseDouble( pnt[0] );
              ycor = Double.parseDouble( pnt[1] );
              //pNr =
              // XPathAPI.selectSingleNode(e,"//featurePoint/id[.//X/text()=pnt[0]
              // and .//Y/text()=pnt[1]]/text()").getNodeValue();
              curPoint = XPathAPI.selectSingleNode( root, ".//featurePoint[.//X/text()='" + pnt[0]
                  + "' and .//Y/text()= '" + pnt[1] + "']" );
              pNr = XPathAPI.selectSingleNode( curPoint, ".//@fid" ).getNodeValue();
              //Node attr =
              // XPathAPI.selectSingleNode(curPoint,".//riverDepth/text()");

              double value = ( (Double)points.get( String.valueOf( xcor ) + String.valueOf( ycor ) ) ).doubleValue();
              values[nodeCounter] = value;
              verticies[nodeCounter++] = GeometryFactory.createGM_Position( xcor, ycor );
              if( value <= 0.0 )
                m_initialise = true;
            }

            //*System.out.println("nodeocunter:" + nodeCounter);
            if( nodeCounter == 4 )
            {// its element is triangle
              //              e = new Element( eNr, vertIDs, verticies, cs );
              me = new MeshElement( eNr, verticies, values, cs );
              mesh.addElement( me );
              //              m_elements.addElement( e );
            }
            if( nodeCounter == 5 )
            {//if element is parallelogram then
              // split that into 2 triangles
              //              e = new Element( eNr, vertIDs, verticies, cs );
              me = new MeshElement( eNr, verticies, values, cs );
              int pType = me.getPolygonType();//get type of
              // parallelogram
              // (Convex or Concave)
              if( pType == Element.CONCAVE_POLYGON )
              {//if its Concave,
                // then its
                // not supported
                throw new Exception( "Concave polygon import is not supported yet." );
              }
              else
              {//otherwise if its Convex then split it based on
                // diagonal slope and keep it least
                MeshElement[] splitElements = me.splitElement();
                for( int j = 0; j < splitElements.length; j++ )
                {
                  MeshElement element = splitElements[j];
                  mesh.addElement( element );

                }
              }

            }
          }//if 3 || 4
          else
          {
            throw new Exception( "Element having less than 3 or more than 4 nodes is not supported." );
          }

        }//for
      }
      catch( Exception exception )
      {
        System.out.println( "Exception: " + exception.toString() );
      }
      System.out.println( m_elements.size() + " elements created." );
    }//if
    mesh.setBoundingBox( m_meshEnv );
  }//readGmlFile

  private void readShapeFiles( Mesh mesh, Vector shapefile, String property, CS_CoordinateSystem crs, GM_Surface wishbox )
  {
    for( int i = 0; i < shapefile.size(); i++ )
    {
      File file = (File)shapefile.get( i );
      String shapebase = FileUtilities.nameWithoutExtension( file.getPath() );
      try
      {
        GMLWorkspace gml = ShapeSerializer.deserialize( shapebase, crs, null );
        Feature root = gml.getRootFeature();
        List features = (List)root.getProperty( ShapeSerializer.PROPERTY_FEATURE_MEMBER );
        //check geometry type to disdinguish the two diffrent files
        GM_Object geom = ( (Feature)features.get( 0 ) ).getDefaultGeometryProperty();
        if( geom instanceof GM_Point )
          createNodes( (Feature[])features.toArray( new Feature[features.size()] ), ShapeSerializer.PROPERTY_GEOMETRY,
              property );
        if( geom instanceof GM_Surface )
          createElements( mesh, (Feature[])features.toArray( new Feature[features.size()] ),
              ShapeSerializer.PROPERTY_GEOMETRY, wishbox );
      }
      catch( GmlSerializeException e )
      {
        e.printStackTrace();
      }
      catch( GM_Exception e )
      {
        e.printStackTrace();
      }

    }//for
  }

  private void createNodes( Feature[] pointFeature, String geometryPropertyPoint, String valueProperty )
  {
    for( int i = 0; i < pointFeature.length; i++ )
    {
      Feature feature = pointFeature[i];
      GM_Point geom = (GM_Point)feature.getProperty( geometryPropertyPoint );
      Object value = feature.getProperty( valueProperty );
      String key = String.valueOf( geom.getX() ) + "#" +String.valueOf( geom.getY() );
      m_hasMapPoints.put( key, value );
    }
  }

  private void createElements( Mesh mesh, Feature[] meshElements, String geometryProperteyElement, GM_Surface wishbox )
      throws GM_Exception
  {
    CS_CoordinateSystem cs = null;
    for( int i = 0; i < meshElements.length; i++ )
    {

      Feature feature = meshElements[i];
      GM_Position[] positions = null;
      Object geom = feature.getProperty( geometryProperteyElement );
      if( geom instanceof GM_Polygon )
      {
        positions = ( (GM_Polygon)geom ).getExteriorRing();
        cs = ( (GM_Polygon)geom ).getCoordinateSystem();
      }
      else if( geom instanceof GM_Surface )
      {
        positions = ( (GM_Surface)geom ).getSurfaceBoundary().getExteriorRing().getPositions();
        cs = ( (GM_Surface)geom ).getCoordinateSystem();
      }
      double[] values = new double[positions.length];
      for( int j = 0; j < positions.length; j++ )
      {
        GM_Position pos = positions[j];
        Object value = m_hasMapPoints.get( String.valueOf( pos.getX() ) + "#" +String.valueOf( pos.getY() ) );
        if( value != null )
        {
          if( value instanceof Double )
            values[j] = ( (Double)value ).doubleValue();
          else if( value instanceof Float )
            values[j] = ( (Float)value ).doubleValue();
          else
            throw new NumberFormatException( "The value to interpolate is of type " + value.getClass().toString()
                + " instead of Float or Double.\n" + "Change type in shape file!" );
        }
        else
          throw new GM_Exception("Point: " + pos + " has no value assigned. Mesh import aborted");
      }

      try
      {

        MeshElement me = new MeshElement( feature.getId(), positions, values, cs );
        int type = me.getPolygonType();
        int noNodes = me.getValues().length;
        if( type != MeshElement.CONCAVE_POLYGON )
        {
          if( noNodes == 5 )
          {
            MeshElement[] elements = me.splitElement();
            for( int j = 0; j < elements.length; j++ )
            {
              MeshElement e = elements[j];
              if( me.getGeometry().intersects( wishbox ) )
              {
                int orientation = me.getOrientation();
                if( orientation != CGAlgorithms.CLOCKWISE )
                  mesh.addElement( e );
                else
                  mesh.addElement( e.invertOrientation() );
              }
            }
          }
          if( noNodes == 4 )
          {
            int orientation = me.getOrientation();
            if( wishbox.intersects( me.getGeometry() ) )
            {
              if( orientation != CGAlgorithms.CLOCKWISE )
                mesh.addElement( me );
              else
                mesh.addElement( me.invertOrientation() );
            }
          }
        }
        else
          throw new NotSupportedFormatException( "Concave Polygons are not supported." );
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
    }
  }// createElements

  public void importMesh( Mesh mesh, Feature[] points, String geometryPropertyPoints, String valueProperty,
      Feature[] elements, String geometryPropertyElements, GM_Surface wishbox ) throws GM_Exception
  {

    createNodes( points, geometryPropertyPoints, valueProperty );
    createElements( mesh, elements, geometryPropertyElements, wishbox );

  }

  private GM_Object readBorder( String shapefile, CS_CoordinateSystem cs ) throws GmlSerializeException
  {
    GMLWorkspace ws = ShapeSerializer.deserialize( shapefile, cs, null );
    Feature root = ws.getRootFeature();
    FeatureList geoProperty = (FeatureList)root.getProperty( ShapeSerializer.PROPERTY_FEATURE_MEMBER );
    Feature features = geoProperty.toFeatures()[0];
    GM_Object geom = features.getDefaultGeometryProperty();
    return geom;
  }
}
