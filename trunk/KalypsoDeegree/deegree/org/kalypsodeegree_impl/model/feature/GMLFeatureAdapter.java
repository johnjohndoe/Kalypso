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
package org.deegree_impl.model.feature;

import java.io.FileReader;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.StringTokenizer;

import org.deegree.model.feature.DeegreeFeature;
import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureCollection;
import org.deegree.model.feature.FeatureException;
import org.deegree.model.feature.FeatureProperty;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
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
import org.deegree.model.geometry.GM_SurfaceInterpolation;
import org.deegree.ogcbasic.CommonNamespaces;
import org.deegree.xml.ElementList;
import org.deegree.xml.XMLParsingException;
import org.deegree.xml.XMLTools;
import org.deegree_impl.model.cs.Adapters;
import org.deegree_impl.model.cs.ConvenienceCSFactory;
import org.deegree_impl.model.cs.CoordinateSystem;
import org.deegree_impl.model.geometry.GMLAdapter;
import org.deegree_impl.model.geometry.GM_Object_Impl;
import org.deegree_impl.model.geometry.GM_SurfaceInterpolation_Impl;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.IDGenerator;
import org.deegree_impl.tools.TimeTools;
import org.opengis.cs.CS_CoordinateSystem;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * 
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public class GMLFeatureAdapter
{

  private static HashMap crsMap = new HashMap( 100 );

  /**
   * 
   * 
   * @param reader
   * 
   * @return @throws
   *         IOException
   * @throws XMLParsingException
   */
  public static FeatureCollection wrap( Reader reader ) throws XMLParsingException,
      GM_Exception
  {
    Debug.debugMethodBegin();

    Document doc = null;

    try
    {
      doc = XMLTools.parse( reader );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw new XMLParsingException( "coundn't create feature collection " + "from GML/XML", e );
    }

    FeatureCollection fc = wrap( doc.getDocumentElement() );

    Debug.debugMethodEnd();
    return fc;
  }

  /**
   * 
   * 
   * @param root
   *          root element of a GML document
   * 
   * @return instance of a <tt>FeatureCollection</tt>
   * 
   * @throws XMLParsingException
   */
  public static FeatureCollection wrap( Element root ) throws XMLParsingException, GM_Exception
  {
    Debug.debugMethodBegin();

    // <boundedBy>
    //Element element = XMLTools.getRequiredChildByName( "boundedBy", GMLNS,
    // root );
    //GM_Envelope bbox = createBoundedBy( element );
    // <featureMember> or extending elements
    ElementList el = XMLTools.getChildElements( root );

    ArrayList list = new ArrayList( el.getLength() );
    // iterates over the feature members
    for( int i = 0; i < el.getLength(); i++ )
    {
      // exclude the boundingbox; description and name
      if( !el.item( i ).getNodeName().endsWith( "boundedBy" )
          && !el.item( i ).getNodeName().endsWith( "name" )
          && !el.item( i ).getNodeName().endsWith( "description" ) )
      {
        // the first child of a feature member must be a feature
        list.add( createFeature( XMLTools.getChildElements( el.item( i ) ).item( 0 ) ) );
      }
    }
    Feature[] features = new Feature[list.size()];
    features = (Feature[])list.toArray( features );

    // create feature collection and use feature type name + a unique
    // number as id
    IDGenerator idg = IDGenerator.getInstance();
    String id = XMLTools.toLocalName( root.getNodeName() );
    id += idg.generateUniqueID();
    FeatureCollection fc = FeatureFactory.createFeatureCollection( id, features );

    Debug.debugMethodEnd();
    return fc;
  }

  /**
   * exports an instance of a <tt>FeatureCollection</tt> to the passed
   * <tt>OutputStream</tt> formatted as GML. Uses UTF-8 as character set for
   * the created XML/GML
   * 
   * @param fc
   *          feature collection to export
   * @param os
   *          output stream to write to
   * 
   * @throws IOException
   * @throws FeatureException
   */
  public static void export( FeatureCollection fc, OutputStream os ) throws IOException,
      FeatureException
  {
    Debug.debugMethodBegin();

    export( fc, new HashMap(), new HashMap(), new HashMap(), os );

    Debug.debugMethodEnd();
  }

  /**
   * exports an instance of a <tt>FeatureCollection</tt> to the passed
   * <tt>OutputStream</tt> formatted as GML. The passed <tt>Map</tt> s may
   * be empty but they are not allowed to be null. Uses UTF-8 as character set
   * for the created XML/GML
   * 
   * @param fc
   *          feature collection to export
   * @param namespaces
   *          namespaces of the different feature types contained in the feature
   *          collection.
   * @param prefixes
   *          prefixes of the elements to be used. gml: will be used for
   *          GML-namespace; xlink: will be used for XLink ns.
   * @param schemaRef
   *          references to the application schemas the features of the
   *          collection beloangs to. The feature type names will be used as
   *          keys
   * @param os
   *          output stream to write to
   * 
   * @throws IOException
   * @throws FeatureException
   */
  public static void export( FeatureCollection fc, Map namespaces, Map prefixes, Map schemaRef,
      OutputStream os ) throws IOException, FeatureException
  {
    export( fc, namespaces, prefixes, schemaRef, os, "UTF-8" );
  }

  /**
   * exports an instance of a <tt>FeatureCollection</tt> to the passed
   * <tt>OutputStream</tt> formatted as GML. The passed <tt>Map</tt> s may
   * be empty but they are not allowed to be null.
   * 
   * @param fc
   *          feature collection to export
   * @param namespaces
   *          namespaces of the different feature types contained in the feature
   *          collection.
   * @param prefixes
   *          prefixes of the elements to be used. gml: will be used for
   *          GML-namespace; xlink: will be used for XLink ns.
   * @param schemaRef
   *          references to the application schemas the features of the
   *          collection beloangs to. The feature type names will be used as
   *          keys
   * @param os
   *          output stream to write to
   * @param charsetName
   *          The name of the used charset/encoding
   * 
   * @throws IOException
   * @throws FeatureException
   */
  public static void export( FeatureCollection fc, Map namespaces, Map prefixes, Map schemaRef,
      OutputStream os, String charsetName ) throws IOException, FeatureException
  {
    Debug.debugMethodBegin();

    // writes the XML-Header and the opening root element to the Writer
    PrintWriter pw = new PrintWriter( new OutputStreamWriter( os, charsetName ) );
    pw.print( "<?xml version='1.0' encoding='" + charsetName + "'?>" );
    openRootElement( fc, namespaces, prefixes, schemaRef, pw );

    // TODO get bbox of the fc and write it as <gml:boundedBy> to
    // the writer
    GM_Envelope env = fc.getEnvelope();
    if( env == null )
    {
      env = GeometryFactory.createGM_Envelope( -9E9, -9E9, 9E9, 9E9 );
    }
    pw.print( "<gml:boundedBy><gml:box>" );
    pw.print( "<gml:coordinates>" );
    pw.print( env.getMin().getX() + "," + env.getMin().getY() + " " );
    pw.print( env.getMax().getX() + "," + env.getMax().getY() );
    pw.print( "</gml:coordinates>" );
    pw.print( "</gml:box></gml:boundedBy>" );
    // export each feature of the FeatureCollection to the PrintWriter
    // by calling the feature-export method
    for( int i = 0; i < fc.getSize(); i++ )
    {
      Feature feature = fc.getFeature( i );
      pw.print( "<gml:featureMember>" );
      export( feature, prefixes, pw );
      pw.print( "</gml:featureMember>" );
    }

    // closes the XML/GML document
    String prefix = "";
    if( prefixes != null && fc.getFeatureType() != null
        && prefixes.get( fc.getFeatureType().getName() ) != null )
    {
      prefix = prefixes.get( fc.getFeatureType().getName() ) + ":";
    }
    if( fc.getFeatureType() == null )
    {
      pw.print( "</Collection>" );
    }
    else
    {
      String name = fc.getFeatureType().getName();
      // replace invalid chars for XML elements
      name = name.replace( ' ', '_' );
      name = name.replace( '/', '.' );
      name = name.replace( '\\', '.' );
      name = name.replace( ':', '_' );
      if( prefix == null )
      {
        pw.print( "</" + name + ">" );
      }
      else
      {
        pw.print( "</" + prefix + name + ">" );
      }
    }

    pw.close();

    Debug.debugMethodEnd();
  }

  /**
   * Opens a PrintWriter from the passed OutputStream and writes the XML header
   * and the opening root element of the GML document to it
   * 
   * @param feature
   *          feature to export
   * @param namespaces
   *          namespaces of the different feature types contained in the feature
   *          collection.
   * @param prefixes
   *          prefixes of the elements to be used. gml: will be used for
   *          GML-namespace; xlink: will be used for XLink ns.
   * @param schemaRef
   *          references to the application schemas the features of the
   *          collection beloangs to. The feature type names will be used as
   *          keys
   * @param pw
   *          PrintWriter to write to
   * @throws IOException
   * @throws FeatureException
   */
  private static void openRootElement( DeegreeFeature feature, Map namespaces, Map prefixes,
      Map schemaRef, PrintWriter pw ) throws FeatureException
  {
    Debug.debugMethodBegin();

    String name = "Collection";
    String prefix = "";
    if( feature.getFeatureType() != null && feature.getFeatureType().getName() != null )
    {
      name = feature.getFeatureType().getName();
      if( prefixes.get( name ) != null )
      {
        prefix = (String)prefixes.get( name ) + ':';
      }
      // replace invalid chars for XML elements
      name = name.replace( ' ', '_' );
      name = name.replace( '/', '.' );
      name = name.replace( '\\', '.' );
      name = name.replace( ':', '_' );
    }
    pw.print( '<' + prefix + name );
    // add namespace declarations to the root element
    pw.print( " xmlns:gml='http://www.opengis.net/gml' " );
    pw.print( " xmlns:xlink='http://www.w3.org/1999/xlink' " );
    pw.print( " xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' " );
    Iterator iterator = prefixes.keySet().iterator();
    while( iterator.hasNext() )
    {
      Object key = iterator.next();
      prefix = (String)prefixes.get( key );
      String ns = (String)namespaces.get( key );
      if( ns == null )
      {
        throw new FeatureException( "for featuretype: " + key
            + " no namespace has been assigned to the prefix: " + prefix );
      }
      pw.print( " xmlns:" );
      pw.print( prefix + "='" );
      pw.print( ns + "' " );
    }
    // add schemalocations to the root element
    iterator = schemaRef.keySet().iterator();
    while( iterator.hasNext() )
    {
      Object key = iterator.next();
      String sch = (String)schemaRef.get( key );
      String ns = (String)namespaces.get( key );
      if( ns == null )
      {
        throw new FeatureException( "for featuretype: " + key
            + " no namespace has been assigned to the " + "schemalocation: " + sch );
      }
      pw.print( " xsi:schemaLocation='" );
      pw.print( ns + " " );
      pw.print( sch );
    }
    pw.print( ">" );

    Debug.debugMethodEnd();

  }

  /**
   * exports a Feature to the passed PrintWriter
   * 
   * @param feature
   *          feature to export
   * @param prefixes
   *          prefixes of the elements to be used. gml: will be used for
   *          GML-namespace; xlink: will be used for XLink ns.
   * @param pw
   *          PrintWriter to write to
   * 
   * @throws IOException
   */
  public static void export( Feature feature, Map prefixes, PrintWriter pw ) throws IOException,
      FeatureException
  {
    Debug.debugMethodBegin();

    String prefix = "";
    if( prefixes != null && prefixes.get( feature.getFeatureType().getName() ) != null )
    {
      prefix = prefixes.get( feature.getFeatureType().getName() ) + ":";
    }
    String featName = feature.getFeatureType().getName();
    // replace invalid chars for XML elements
    featName = featName.replace( ' ', '_' );
    featName = featName.replace( '/', '.' );
    featName = featName.replace( '\\', '.' );
    featName = featName.replace( ':', '_' );
    String id = feature.getId();
    pw.print( "<" + prefix + featName + " fid=\"" + id + "\">" );

    FeatureTypeProperty[] ftp = feature.getFeatureType().getProperties();
    for( int i = 0; i < ftp.length; i++ )
    {
      String name = ftp[i].getName();
      Object o = feature.getProperty( name );
      name = name.replace( ' ', '_' );
      name = name.replace( '/', '.' );
      pw.print( "<" + prefix + name + ">" );
      if( o == null )
      {}
      else if( o instanceof FeatureCollection )
      {
        printFeatureCollection( (FeatureCollection)o, prefixes, pw );
      }
      else if( o instanceof GM_Object )
      {
        printGeometry( (GM_Object)o, pw );
      }
      else
      {
        pw.print( o.toString() );
      }
      pw.print( "</" + prefix + name + ">" );
    }

    pw.print( "</" + prefix + featName + ">" );

    Debug.debugMethodEnd();
  }

  /**
   * prints the passed FeatureCollection to the also passed PrintWriter
   * formatted as GMLs
   * 
   * @param fc
   *          feature collection to print/export
   * @param prefixes
   *          Map containing prefixes for the XML Elements to create
   * @param pw
   *          target of the printing/export
   * @throws IOException
   * @throws FeatureException
   */
  private static void printFeatureCollection( FeatureCollection fc, Map prefixes, PrintWriter pw )
      throws IOException, FeatureException
  {
    Debug.debugMethodBegin();

    String prefix = "";
    String name = "Collection";
    if( fc.getFeatureType() != null && fc.getFeatureType().getName() != null )
    {
      name = fc.getFeatureType().getName();
      if( prefixes.get( name ) != null )
      {
        prefix = (String)prefixes.get( name ) + ':';
      }
      name = name.replace( ' ', '_' );
      name = name.replace( '/', '.' );
      name = name.replace( '\\', '.' );
      name = name.replace( ':', '_' );
    }
    pw.print( '<' + prefix + name + '>' );

    for( int k = 0; k < fc.getSize(); k++ )
    {
      Feature feature = fc.getFeature( k );
      pw.print( "<gml:featureMember>" );
      export( feature, prefixes, pw );
      pw.print( "</gml:featureMember>" );
    }

    pw.print( "</" + prefix + name + '>' );

    Debug.debugMethodEnd();
  }

  /**
   * prints the passed geometry to the also passed PrintWriter formatted as GML
   * 
   * @param geo
   *          geometry to print/extport
   * @param pw
   *          target of the printing/export
   * @throws FeatureException
   */
  private static void printGeometry( GM_Object geo, PrintWriter pw ) throws FeatureException
  {
    Debug.debugMethodBegin();

    try
    {
      pw.print( GMLAdapter.export( geo ) );
    }
    catch( Exception e )
    {
      throw new FeatureException( "could not export feature to GML", e );
    }

    Debug.debugMethodEnd();
  }

  /**
   * creates an instance of a deegree feature from a contrete gml:_Feature node
   * 
   * @param element
   *          <gml:_Feature>
   * 
   * @return instance of Feature
   * 
   * @throws XMLParsingException
   */
  private static Feature createFeature( Element element ) throws XMLParsingException, GM_Exception
  {
    Debug.debugMethodBegin();

    // <boundedBy>
    /* Element elem = */XMLTools.getChildByName( "boundedBy", CommonNamespaces.GMLNS, element );
    //        GM_Envelope bbox = null;

    //        if ( elem != null ) {
    //            bbox = createBoundedBy( elem );
    //        }

    // feature type is required to create a deegree feature
    FeatureType ft = createFeatureType( element );
    // <properties> or extending elements
    ElementList el = XMLTools.getChildElements( element );
    ArrayList list = new ArrayList( el.getLength() );
    for( int i = 0; i < el.getLength(); i++ )
    {
      // exclude the boundingbox; description and name
      if( !el.item( i ).getNodeName().endsWith( "boundedBy" )
          && !el.item( i ).getNodeName().endsWith( "name" )
          && !el.item( i ).getNodeName().endsWith( "description" ) )
      {
        String propName = XMLTools.toLocalName( el.item( i ).getNodeName() );
        String type = ft.getProperty( propName ).getType();
        if( type.equals( "java.lang.Number" ) )
        {
          String value = XMLTools.getStringValue( el.item( i ) );
          list.add( FeatureFactory.createFeatureProperty( propName, new Double( value ) ) );
        }
        else if( type.equals( "java.lang.String" ) )
        {
          String value = XMLTools.getStringValue( el.item( i ) );
          list.add( FeatureFactory.createFeatureProperty( propName, value ) );
        }
        else if( type.equals( "java.util.Date" ) )
        {
          String value = XMLTools.getStringValue( el.item( i ) );
          Date date = TimeTools.createCalendar( value ).getTime();
          list.add( FeatureFactory.createFeatureProperty( propName, date ) );
        }
        else if( type.equals( "org.deegree.model.geometry.GM_Object" ) )
        {
          list
              .add( FeatureFactory.createFeatureProperty( propName, createGeometry( el.item( i ) ) ) );
        }
        else if( type.equals( "org.deegree.model.feature.FeatureCollection" ) )
        {
          list.add( FeatureFactory.createFeatureProperty( propName, wrap( el.item( i ) ) ) );
        }
        else if( type.equals( "org.deegree.model.feature.Feature" ) )
        {
          list
              .add( FeatureFactory.createFeatureProperty( propName, createFeature( el.item( i ) ) ) );
        }
      }
    }
    FeatureProperty[] fp = new FeatureProperty[list.size()];
    fp = (FeatureProperty[])list.toArray( fp );
    long l = IDGenerator.getInstance().generateUniqueID();
    Feature feature = FeatureFactory.createFeature( "id" + l, ft, fp );

    Debug.debugMethodEnd();
    return feature;
  }

  /**
   * returns an instance of a FeatureType describig the passed feature (element)
   * 
   * @param element
   *          <gml:_Feature>
   * 
   * @return instance of <tt>FeatureType</tt>
   * 
   * @throws XMLParsingException
   */
  private static FeatureType createFeatureType( Element element )
  {
    Debug.debugMethodBegin();

    String featureName = element.getNodeName();

    // <properties> or extending elements
    ElementList el = XMLTools.getChildElements( element );
    ArrayList list = new ArrayList( el.getLength() );
    for( int i = 0; i < el.getLength(); i++ )
    {
      // exclude the boundingbox; description and name
      if( !el.item( i ).getNodeName().endsWith( "boundedBy" )
          && !el.item( i ).getNodeName().endsWith( "name" )
          && !el.item( i ).getNodeName().endsWith( "description" ) )
      {
        String name = XMLTools.toLocalName( el.item( i ).getNodeName() );
        ElementList el_ = XMLTools.getChildElements( el.item( i ) );

        if( ( el_ == null ) || ( el_.getLength() == 0 ) )
        {
          // must be a simple datatype
          String tmp = XMLTools.getStringValue( el.item( i ) );

          try
          {
            // it's a number
            Double.parseDouble( tmp );
            list.add( FeatureFactory.createFeatureTypeProperty( name, "java.lang.Number", true ) );
          }
          catch( Exception e )
          {
            try
            {
              // it's a date
              TimeTools.createCalendar( tmp );
              list.add( FeatureFactory.createFeatureTypeProperty( name, "java.util.Date", true ) );
            }
            catch( Exception ee )
            {
              // it's a string
              list.add( FeatureFactory.createFeatureTypeProperty( name, "java.lang.String", true ) );
            }
          }
        }
        else
        {
          // must be a geometry or a complex (feature) property
          if( isGeometryProperty( el.item( i ) ) )
          {
            // it's a geometry
            list.add( FeatureFactory.createFeatureTypeProperty( name,
                "org.deegree.model.geometry.GM_Object", true ) );
          }
          else
          {
            if( isFeatureCollection( el.item( i ) ) )
            {
              // it's a Feature
              list.add( FeatureFactory.createFeatureTypeProperty( name,
                  "org.deegree.model.feature.FeatureCollection", true ) );
            }
            else
            {
              // it's a Feature
              list.add( FeatureFactory.createFeatureTypeProperty( name,
                  "org.deegree.model.feature.Feature", true ) );
            }
          }
        }
      }
    }
    FeatureTypeProperty[] ftp = new FeatureTypeProperty[list.size()];
    ftp = (FeatureTypeProperty[])list.toArray( ftp );

    FeatureType ft = FeatureFactory.createFeatureType( null, null, featureName, ftp );

    Debug.debugMethodEnd();
    return ft;
  }

  /**
   * returns true if the passed element encapsulates a feature collection
   */
  private static boolean isFeatureCollection( Element element )
  {
    Debug.debugMethodBegin();

    ElementList el = XMLTools.getChildElements( element );
    for( int i = 0; i < el.getLength(); i++ )
    {
      if( !el.item( i ).getNodeName().endsWith( "boundedBy" )
          && !el.item( i ).getNodeName().endsWith( "name" )
          && !el.item( i ).getNodeName().endsWith( "description" ) )
      {
        ElementList el_ = XMLTools.getChildElements( element );
        for( int k = 0; k < el_.getLength(); k++ )
        {
          if( !el_.item( k ).getNodeName().endsWith( "boundedBy" )
              && !el_.item( k ).getNodeName().endsWith( "name" )
              && !el_.item( k ).getNodeName().endsWith( "description" ) )
          {
            if( isGeometryProperty( el_.item( k ) ) )
            {
              return false;
            }
            if( XMLTools.getChildElements( el_.item( k ) ).getLength() > 0 )
            {
              return true;
            }
          }
        }
        return false;
      }
    }

    Debug.debugMethodEnd();
    return true;
  }

  /**
   * returns true if the submitted element contains just excatly one
   * childelement that have to be a geometry
   */
  private static boolean isGeometryProperty( Element element )
  {
    Debug.debugMethodBegin();

    boolean flag = false;
    ElementList el = XMLTools.getChildElements( element );

    for( int i = 0; i < el.getLength(); i++ )
    {
      Element elem = el.item( i );
      String name = XMLTools.toLocalName( elem.getNodeName() );

      if( name.equals( "Point" ) || name.equals( "LineString" ) || name.equals( "Polygon" )
          || name.equals( "MultiPoint" ) || name.equals( "MultiLineString" )
          || name.equals( "MultiPolygon" ) || name.equals( "Box" ) || name.equals( "MultiGeometry" ) )
      {
        flag = true;
        break;
      }
      else
      {
        flag = false;
        break;
      }
    }

    Debug.debugMethodEnd();
    return flag;
  }


  /**
   * 
   * 
   * @param element
   * 
   * @return @throws
   *         XMLParsingException
   */
  private static GM_Object createGeometry( Element element ) throws XMLParsingException,
      GM_Exception
  {
    Debug.debugMethodBegin();
    element = XMLTools.getFirstElement( element );
    String nodeName = XMLTools.toLocalName( element.getNodeName() );

    GM_Object geom = null;

    // in a future release this should be validate against
    // the schema the element belongs to
    if( nodeName.equals( "Point" ) )
    {
      geom = createPoint( element );
    }
    else if( nodeName.equals( "LineString" ) )
    {
      geom = createLineString( element );
    }
    else if( nodeName.equals( "Polygon" ) )
    {
      geom = createPolygon( element );
    }
    else if( nodeName.equals( "Box" ) )
    {
      throw new GM_Exception( "Box is a not supported geomerty at the moment" );
    }
    else if( nodeName.equals( "MultiPoint" ) )
    {
      geom = createMultiPoint( element );
    }
    else if( nodeName.equals( "MultiLineString" ) )
    {
      geom = createMultiLineString( element );
    }
    else if( nodeName.equals( "MultiPolygon" ) )
    {
      geom = createMultiSurface( element );
    }
    else if( nodeName.equals( "GeometryCollection" ) )
    {
      throw new GM_Exception( "GeometryCollection is a not supported " + "geomerty at the moment" );
    }

    Debug.debugMethodEnd();

    return geom;
  }

  /**
   * returns an instance of GM_Position created from the passed coord
   * 
   * @param element
   *          <coord>
   * 
   * @return instance of <tt>GM_Position</tt>
   * 
   * @throws XMLParsingException
   */
  private static GM_Position createPositionFromCoord( Element element ) throws XMLParsingException
  {
    Debug.debugMethodBegin();

    GM_Position pos = null;
    Element elem = XMLTools.getRequiredChildByName( "X", CommonNamespaces.GMLNS, element );
    double x = Double.parseDouble( XMLTools.getStringValue( elem ) );
    elem = XMLTools.getRequiredChildByName( "Y", CommonNamespaces.GMLNS, element );
    double y = Double.parseDouble( XMLTools.getStringValue( elem ) );
    elem = XMLTools.getChildByName( "Z", CommonNamespaces.GMLNS, element );

    if( elem != null )
    {
      double z = Double.parseDouble( XMLTools.getStringValue( elem ) );
      pos = GeometryFactory.createGM_Position( new double[]
      {
          x,
          y,
          z } );
    }
    else
    {
      pos = GeometryFactory.createGM_Position( new double[]
      {
          x,
          y } );
    }

    Debug.debugMethodEnd();
    return pos;
  }

  /**
   * returns an array of GM_Positions created from the passed coordinates
   * 
   * @param element
   *          <coordinates>
   * 
   * @return instance of <tt>GM_Position[]</tt>
   * 
   * @throws XMLParsingException
   */
  public static GM_Position[] createPositionFromCoordinates( Element element )
  {
    Debug.debugMethodBegin();

    GM_Position[] points = null;

    String ts = XMLTools.getAttrValue( element, "ts" );

    if( ts == null )
    {
      ts = " ";
    }

    String ds = XMLTools.getAttrValue( element, "decimal" );

    if( ds == null )
    {
      ds = ".";
    }

    String cs = XMLTools.getAttrValue( element, "cs" );

    if( cs == null )
    {
      cs = ",";
    }

    String value = XMLTools.getStringValue( element );

    // first tokenizer, tokens the tuples
    StringTokenizer tuple = new StringTokenizer( value, ts );
    points = new GM_Position[tuple.countTokens()];

    int i = 0;

    while( tuple.hasMoreTokens() )
    {
      String s = tuple.nextToken();
      // second tokenizer, tokens the coordinates
      StringTokenizer coort = new StringTokenizer( s, cs );
      double[] p = new double[coort.countTokens()];

      for( int k = 0; k < p.length; k++ )
      {
        p[k] = Double.parseDouble( coort.nextToken() );
      }

      points[i++] = GeometryFactory.createGM_Position( p );
    }

    Debug.debugMethodEnd();
    return points;
  }

  /**
   * returns an instance of CS_CoordinateSystem corrsponding to the passed crs
   * name
   * 
   * @param name
   *          name of the crs
   * 
   * @return
   */
  private static CS_CoordinateSystem getCRS( String name )
  {

    if( ( name != null ) && ( name.length() > 2 ) )
    {
      if( name.startsWith( "http://www.opengis.net/gml/srs/" ) )
      {
        // as declared in the GML 2.1.1 specification
        //http://www.opengis.net/gml/srs/epsg.xml#4326
        int p = name.lastIndexOf( "/" );

        if( p >= 0 )
        {
          name = name.substring( p, name.length() );
          p = name.indexOf( "." );

          String s1 = name.substring( 1, p ).toUpperCase();
          p = name.indexOf( "#" );

          String s2 = name.substring( p + 1, name.length() );
          name = s1 + ":" + s2;
        }
      }
    }

    CS_CoordinateSystem crs = (CS_CoordinateSystem)crsMap.get( name );

    if( crs == null )
    {
      CoordinateSystem cs = ConvenienceCSFactory.getInstance().getCSByName( name );
      Adapters adapter = Adapters.getDefault();
      crs = adapter.export( cs );
      crsMap.put( name, crs );
    }

    return crs;
  }

  /**
   * returns an instance of a point created from the passed <gml:Point>
   * 
   * @param element
   *          <gml:Point>
   * 
   * @return instance of GM_Point
   * 
   * @throws XMLParsingException
   */
  private static GM_Point createPoint( Element element ) throws XMLParsingException
  {
    Debug.debugMethodBegin();

    String srs = XMLTools.getAttrValue( "srsName", element );
    CS_CoordinateSystem crs = null;
    if( srs != null )
    {
      crs = getCRS( srs );
    }

    Element elem = XMLTools.getChildByName( "coord", CommonNamespaces.GMLNS, element );

    GM_Position[] pos = null;

    if( elem != null )
    {
      pos = new GM_Position[1];
      pos[0] = createPositionFromCoord( elem );
    }
    else
    {
      elem = XMLTools.getChildByName( "coordinates", CommonNamespaces.GMLNS, element );

      if( elem != null )
      {
        pos = createPositionFromCoordinates( elem );
      }
    }

    GM_Point point = GeometryFactory.createGM_Point( pos[0], crs );

    Debug.debugMethodEnd();
    return point;
  }

  /**
   * returns an instance of a curve created from the passed <gml:LineString>
   * 
   * @param element
   *          <gml:LineString>
   * 
   * @return instance of GM_Curve
   * 
   * @throws XMLParsingException
   */
  private static GM_Curve createLineString( Element element ) throws XMLParsingException,
      GM_Exception
  {
    Debug.debugMethodBegin();

    String srs = XMLTools.getAttrValue( "srsName", element );
    CS_CoordinateSystem crs = null;
    if( srs != null )
    {
      crs = getCRS( srs );
    }

    Element elem = XMLTools.getChildByName( "coordinates", CommonNamespaces.GMLNS, element );
    GM_Position[] pos = createPositionFromCoordinates( elem );

    GM_Curve curve = GeometryFactory.createGM_Curve( pos, crs );

    Debug.debugMethodEnd();
    return curve;
  }

  /**
   * returns an instance of a surface created from the passed <gml:Polygon>
   * 
   * @param element
   *          <gml:Polygon>
   * 
   * @return instance of GM_Surface
   * 
   * @throws XMLParsingException
   */
  private static GM_Surface createPolygon( Element element ) throws XMLParsingException,
      GM_Exception
  {
    Debug.debugMethodBegin();

    String srs = XMLTools.getAttrValue( "srsName", element );
    CS_CoordinateSystem crs = null;
    if( srs != null )
    {
      crs = getCRS( srs );
    }

    Element outs = XMLTools.getRequiredChildByName( "outerBoundaryIs", CommonNamespaces.GMLNS,
        element );
    Element ring = XMLTools.getRequiredChildByName( "LinearRing", CommonNamespaces.GMLNS, outs );
    Element elem = XMLTools.getChildByName( "coordinates", CommonNamespaces.GMLNS, ring );
    GM_Position[] outterRing = createPositionFromCoordinates( elem );

    GM_Position[][] innerRings = null;
    ElementList inns = XMLTools.getChildElementsByName( "innerBoundaryIs", CommonNamespaces.GMLNS,
        element );
    if( inns != null && inns.getLength() > 0 )
    {
      innerRings = new GM_Position[inns.getLength()][];
      for( int i = 0; i < innerRings.length; i++ )
      {
        ring = XMLTools
            .getRequiredChildByName( "LinearRing", CommonNamespaces.GMLNS, inns.item( i ) );
        elem = XMLTools.getChildByName( "coordinates", CommonNamespaces.GMLNS, ring );
        innerRings[i] = createPositionFromCoordinates( elem );
      }
    }

    GM_SurfaceInterpolation si = new GM_SurfaceInterpolation_Impl();
    GM_Surface surface = GeometryFactory.createGM_Surface( outterRing, innerRings, si, crs );

    Debug.debugMethodEnd();
    return surface;
  }

  /**
   * returns an instance of a multi point created from the passed
   * <gml:MultiPoint>
   * 
   * @param element
   *          <gml:MultiPoint>
   * 
   * @return instance of GM_MultiPoint
   * 
   * @throws XMLParsingException
   */
  private static GM_MultiPoint createMultiPoint( Element element ) throws XMLParsingException,
      GM_Exception
  {
    Debug.debugMethodBegin();

    String srs = XMLTools.getAttrValue( "srsName", element );
    CS_CoordinateSystem crs = null;
    if( srs != null )
    {
      crs = getCRS( srs );
    }

    ElementList el = XMLTools.getChildElementsByName( "geometryMember", CommonNamespaces.GMLNS,
        element );
    GM_Point[] points = new GM_Point[el.getLength()];
    for( int i = 0; i < points.length; i++ )
    {
      points[i] = createPoint( XMLTools.getFirstElement( el.item( i ) ) );
      ( (GM_Object_Impl)points[i] ).setCoordinateSystem( crs );
    }

    GM_MultiPoint mp = GeometryFactory.createGM_MultiPoint( points );

    Debug.debugMethodEnd();
    return mp;
  }

  /**
   * returns an instance of a multi point created from the passed
   * <gml:MultiLineString>
   * 
   * @param element
   *          <gml:MultiLineString>
   * 
   * @return instance of GM_MultiCurve
   * 
   * @throws XMLParsingException
   */
  private static GM_MultiCurve createMultiLineString( Element element ) throws XMLParsingException,
      GM_Exception
  {
    Debug.debugMethodBegin();

    String srs = XMLTools.getAttrValue( "srsName", element );
    CS_CoordinateSystem crs = null;
    if( srs != null )
    {
      crs = getCRS( srs );
    }

    ElementList el = XMLTools.getChildElementsByName( "geometryMember", CommonNamespaces.GMLNS,
        element );
    GM_Curve[] curves = new GM_Curve[el.getLength()];
    for( int i = 0; i < curves.length; i++ )
    {
      curves[i] = createLineString( XMLTools.getFirstElement( el.item( i ) ) );
      ( (GM_Object_Impl)curves[i] ).setCoordinateSystem( crs );
    }

    GM_MultiCurve mp = GeometryFactory.createGM_MultiCurve( curves );

    Debug.debugMethodEnd();
    return mp;
  }

  /**
   * returns an instance of a multi point created from the passed
   * <gml:MultiLineString>
   * 
   * @param element
   *          <gml:MultiLineString>
   * 
   * @return instance of GM_MultiCurve
   * 
   * @throws XMLParsingException
   */
  private static GM_MultiSurface createMultiSurface( Element element ) throws XMLParsingException,
      GM_Exception
  {
    Debug.debugMethodBegin();

    String srs = XMLTools.getAttrValue( "srsName", element );
    CS_CoordinateSystem crs = null;
    if( srs != null )
    {
      crs = getCRS( srs );
    }

    ElementList el = XMLTools.getChildElementsByName( "geometryMember", CommonNamespaces.GMLNS,
        element );
    GM_Surface[] surfaces = new GM_Surface[el.getLength()];
    for( int i = 0; i < surfaces.length; i++ )
    {
      surfaces[i] = createPolygon( XMLTools.getFirstElement( el.item( i ) ) );
      ( (GM_Object_Impl)surfaces[i] ).setCoordinateSystem( crs );
    }

    GM_MultiSurface mp = GeometryFactory.createGM_MultiSurface( surfaces );

    Debug.debugMethodEnd();
    return mp;
  }

  public static void main( String[] args ) throws Exception
  {
    FeatureCollection fc = GMLFeatureAdapter.wrap( new FileReader( "c:/temp/temp.xml" ) );
    System.out.println( fc );
  }

}