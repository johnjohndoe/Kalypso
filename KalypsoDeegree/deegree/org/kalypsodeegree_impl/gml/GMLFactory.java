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
package org.deegree_impl.gml;

import org.deegree.gml.*;
import org.deegree.model.feature.*;
import org.deegree.model.geometry.*;
import org.deegree.xml.XMLTools;

import org.deegree_impl.model.feature.XLinkFeatureTypeProperty;
import org.deegree_impl.tools.*;

import org.w3c.dom.*;

/**
 * class containing factory methods for creating GML-Objects from DOM-Elements
 * and GM_XXXX geometries.
 * 
 * <p>
 * ----------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version 17.02.2002
 *          <p>
 */
public class GMLFactory
{
  /**
   * creates a GMLGeometry from a DOM Element
   * 
   * @param element
   *          DOM Element containing a GMLGeometry
   */
  public synchronized static GMLGeometry createGMLGeometry( Element element ) throws GMLException
  {
    Debug.debugMethodBegin( "GMLFactory", "createGMLGeometry" );

    String nodeName = XMLTools.toLocalName( element.getNodeName() );

    GMLGeometry geom = null;

    // in a future release this should be validate against
    // the schema the element belongs to
    if( nodeName.equals( "Point" ) )
    {
      geom = new GMLPoint_Impl( element );
    }
    else if( nodeName.equals( "LineString" ) )
    {
      geom = new GMLLineString_Impl( element );
    }
    else if( nodeName.equals( "Polygon" ) )
    {
      geom = new GMLPolygon_Impl( element );
    }
    else if( nodeName.equals( "Box" ) )
    {
      geom = new GMLBox_Impl( element );
    }
    else if( nodeName.equals( "MultiPoint" ) )
    {
      geom = new GMLMultiPoint_Impl( element );
    }
    else if( nodeName.equals( "MultiLineString" ) )
    {
      geom = new GMLMultiLineString_Impl( element );
    }
    else if( nodeName.equals( "MultiPolygon" ) )
    {
      geom = new GMLMultiPolygon_Impl( element );
    }
    else if( nodeName.equals( "GeometryCollection" ) )
    {
      geom = new GMLGeometryCollection_Impl( element );
    }

    Debug.debugMethodEnd();

    return geom;
  }

  /**
   * creates a GMLGeometry from a GM_Object
   * 
   * @param geo
   *          geometry
   */
  public synchronized static GMLGeometry createGMLGeometry( GM_Object geo ) throws GMLException
  {
    Debug.debugMethodBegin( "GMLFactory", "createGMLGeometry(GM_Object)" );

    GMLDocument_Impl doc = new GMLDocument_Impl();
    GMLGeometry geom = null;

    if( geo instanceof GM_Point )
    {
      geom = createGMLPoint( (GM_Point)geo, doc );
    }
    else if( geo instanceof GM_Curve )
    {
      geom = createGMLLineString( (GM_Curve)geo, doc );
    }
    else if( geo instanceof GM_Surface )
    {
      geom = createGMLPolygon( (GM_Surface)geo, doc );
    }
    else if( geo instanceof GM_MultiPoint )
    {
      geom = createGMLMultiPoint( (GM_MultiPoint)geo, doc );
    }
    else if( geo instanceof GM_MultiCurve )
    {
      geom = createGMLMultiLineString( (GM_MultiCurve)geo, doc );
    }
    else if( geo instanceof GM_MultiSurface )
    {
      geom = createGMLMultiPolygon( (GM_MultiSurface)geo, doc );
    }

    Debug.debugMethodEnd();
    return geom;
  }

  /**
   * creates a GMLPoint from a GM_Point
   * 
   * @param geo
   *          point
   */
  private static GMLPoint createGMLPoint( GM_Point geo, GMLDocument_Impl doc ) throws GMLException
  {
    Debug.debugMethodBegin( "GMLFactory", "createGMLPoint" );

    Element coord = doc.getDocument()
        .createElementNS( GMLGeometricMapping.GMLNS, "gml:coordinates" );

    GMLCoordinates gmlCo = new GMLCoordinates_Impl( coord );
    gmlCo.setCoordinates( geo.getX() + "," + geo.getY()
        + ( ( geo.getCoordinateDimension() == 3 ) ? "," + geo.getZ() : "" ) );
    gmlCo.setCoordinateSeperator( ',' );
    gmlCo.setDecimalSeperator( '.' );
    gmlCo.setTupleSeperator( ' ' );

    GMLPoint gmlP = GMLPoint_Impl.createGMLPoint( doc.getDocument() );
    gmlP.setCoordinates( gmlCo );

    try
    {
      String s = geo.getCoordinateSystem().getName();
      s = s.replace( ' ', ':' );
      s = StringExtend.replace( s, ":", ".xml#", true );
      gmlP.setSrs( "http://www.opengis.net/gml/srs/" + s );
    }
    catch( Exception e )
    {
      gmlP.setSrs( "null" );
    }

    Debug.debugMethodEnd();
    return gmlP;
  }

  /**
   * creates a GMLLineString from a GM_Curve
   * 
   * @param geo
   *          GM_Curve
   */
  private static GMLLineString createGMLLineString( GM_Curve geo, GMLDocument_Impl doc )
      throws GMLException
  {
    Debug.debugMethodBegin( "GMLFactory", "createGMLLineString" );

    Element coord = doc.getDocument()
        .createElementNS( GMLGeometricMapping.GMLNS, "gml:coordinates" );

    GMLCoordinates gmlCo = new GMLCoordinates_Impl( coord );

    StringBuffer sb = null;

    try
    {
      GM_LineString ls = geo.getAsLineString();

      sb = new StringBuffer( ls.getNumberOfPoints() * 35 );

      for( int i = 0; i < ls.getNumberOfPoints(); i++ )
      {
        GM_Position pt = ls.getPositionAt( i );
        sb.append( pt.getX() + "," + pt.getY()
            + ( ( pt.getAsArray().length == 3 ) ? "," + pt.getZ() : "" ) + " " );
      }
    }
    catch( Exception e )
    {
      throw new GMLException( "couldn't create GMLLineString \n" + e );
    }

    gmlCo.setCoordinates( sb.toString().trim() );
    gmlCo.setCoordinateSeperator( ',' );
    gmlCo.setDecimalSeperator( '.' );
    gmlCo.setTupleSeperator( ' ' );

    GMLLineString gmlLs = GMLLineString_Impl.createGMLLineString( doc.getDocument() );
    gmlLs.setCoordinates( gmlCo );

    try
    {
      String s = geo.getCoordinateSystem().getName();
      s = s.replace( ' ', ':' );
      s = StringExtend.replace( s, ":", ".xml#", true );
      gmlLs.setSrs( "http://www.opengis.net/gml/srs/" + s );
    }
    catch( Exception e )
    {
      gmlLs.setSrs( "null" );
    }

    Debug.debugMethodEnd();
    return gmlLs;
  }

  /**
   * creates a GMLPolygon from a GM_Surface
   * 
   * @param geo
   *          GM_Surface
   */
  private static GMLPolygon createGMLPolygon( GM_Surface geo, GMLDocument_Impl doc )
      throws GMLException
  {
    Debug.debugMethodBegin( "GMLFactory", "createGMLSurface" );

    GM_Position[] ex = null;
    GM_Position[][] in = null;

    try
    {
      ex = geo.getSurfacePatchAt( 0 ).getExteriorRing();
      in = geo.getSurfacePatchAt( 0 ).getInteriorRings();
    }
    catch( Exception e )
    {
      throw new GMLException( "couldn't create GMLPolygon \n" + e );
    }

    // exterior ring
    Element coord = doc.getDocument()
        .createElementNS( GMLGeometricMapping.GMLNS, "gml:coordinates" );
    GMLCoordinates gmlCo = new GMLCoordinates_Impl( coord );

    StringBuffer sb = null;

    try
    {
      sb = new StringBuffer( ex.length * 35 );

      if( ex[0].getAsArray().length == 2 )
      {
        for( int i = 0; i < ex.length; i++ )
        {
          sb.append( ex[i].getX() + "," + ex[i].getY() + " " );
        }
      }
      else
      {
        for( int i = 0; i < ex.length; i++ )
        {
          sb.append( ex[i].getX() + "," + ex[i].getY() + "," + ex[i].getZ() + " " );
        }
      }
    }
    catch( Exception e )
    {
      throw new GMLException( "couldn't create GMLPolygon \n" + e );
    }

    gmlCo.setCoordinates( sb.toString().trim() );
    gmlCo.setCoordinateSeperator( ',' );
    gmlCo.setDecimalSeperator( '.' );
    gmlCo.setTupleSeperator( ' ' );

    GMLLinearRing gmlRing = GMLLinearRing_Impl.createGMLLinearRing( doc.getDocument() );
    gmlRing.setCoordinates( gmlCo );

    GMLPolygon gmlPoly = GMLPolygon_Impl.createGMLPolygon( doc.getDocument() );
    gmlPoly.setExteriorRing( gmlRing );

    // interior rings
    if( in != null )
    {
      for( int i = 0; i < in.length; i++ )
      {
        coord = doc.getDocument().createElementNS( GMLGeometricMapping.GMLNS, "gml:coordinates" );
        gmlCo = new GMLCoordinates_Impl( coord );

        try
        {
          sb = new StringBuffer( in[i].length * 35 );

          if( in[0][0].getAsArray().length == 2 )
          {
            for( int j = 0; j < in[i].length; j++ )
            {
              sb.append( in[i][j].getX() + "," + in[i][j].getY() + " " );
            }
          }
          else
          {
            for( int j = 0; j < in[i].length; j++ )
            {
              sb.append( in[i][j].getX() + "," + in[i][j].getY() + "," + in[i][j].getZ() + " " );
            }
          }
        }
        catch( Exception e )
        {
          throw new GMLException( "couldn't create GMLPolygon \n" + e );
        }

        gmlCo.setCoordinates( sb.toString().trim() );
        gmlCo.setCoordinateSeperator( ',' );
        gmlCo.setDecimalSeperator( '.' );
        gmlCo.setTupleSeperator( ' ' );

        gmlRing = GMLLinearRing_Impl.createGMLLinearRing( doc.getDocument() );
        gmlRing.setCoordinates( gmlCo );

        try
        {
          gmlRing.setSrs( geo.getCoordinateSystem().getName() );
        }
        catch( Exception e )
        {
          gmlRing.setSrs( "null" );
        }

        gmlPoly.addInteriorRing( gmlRing );
      }
    }

    try
    {
      String s = geo.getCoordinateSystem().getName();
      s = s.replace( ' ', ':' );
      s = StringExtend.replace( s, ":", ".xml#", true );
      gmlPoly.setSrs( "http://www.opengis.net/gml/srs/" + s );
    }
    catch( Exception e )
    {
      gmlPoly.setSrs( "null" );
    }

    Debug.debugMethodEnd();
    return gmlPoly;
  }

  /**
   * creates a GMLMultiPoint from a GM_MultiPoint
   * 
   * @param geo
   *          GM_MultiPoint
   */
  private static GMLMultiPoint createGMLMultiPoint( GM_MultiPoint geo, GMLDocument_Impl doc )
      throws GMLException
  {
    Debug.debugMethodBegin( "GMLFactory", "createGMLPoint" );

    GMLMultiPoint gmlMPoint = GMLMultiPoint_Impl.createGMLMultiPoint( doc.getDocument() );

    try
    {
      for( int i = 0; i < geo.getSize(); i++ )
      {
        gmlMPoint.addPoint( createGMLPoint( geo.getPointAt( i ), doc ) );
      }
    }
    catch( Exception e )
    {
      throw new GMLException( "couldn't create GMLMultiPoint \n" + e );
    }

    try
    {
      String s = geo.getCoordinateSystem().getName();
      s = s.replace( ' ', ':' );
      s = StringExtend.replace( s, ":", ".xml#", true );
      gmlMPoint.setSrs( "http://www.opengis.net/gml/srs/" + s );
    }
    catch( Exception e )
    {
      gmlMPoint.setSrs( "null" );
    }

    Debug.debugMethodEnd();
    return gmlMPoint;
  }

  /**
   * creates a GMLMultiLineString from a GM_MultiCurve
   * 
   * @param geo
   *          GM_MultiCurve
   */
  private static GMLMultiLineString createGMLMultiLineString( GM_MultiCurve geo,
      GMLDocument_Impl doc ) throws GMLException
  {
    Debug.debugMethodBegin( "GMLFactory", "createGMLPoint" );

    GMLMultiLineString gmlMLineString = GMLMultiLineString_Impl.createGMLMultiLineString( doc
        .getDocument() );

    try
    {
      for( int i = 0; i < geo.getSize(); i++ )
      {
        gmlMLineString.addLineString( createGMLLineString( geo.getCurveAt( i ), doc ) );
      }
    }
    catch( Exception e )
    {
      throw new GMLException( "couldn't create GMLMultiLineString \n" + e );
    }

    try
    {
      String s = geo.getCoordinateSystem().getName();
      s = s.replace( ' ', ':' );
      s = StringExtend.replace( s, ":", ".xml#", true );
      gmlMLineString.setSrs( "http://www.opengis.net/gml/srs/" + s );
    }
    catch( Exception e )
    {
      gmlMLineString.setSrs( "null" );
    }

    Debug.debugMethodEnd();
    return gmlMLineString;
  }

  /**
   * creates a GMLMultiPolygon from a GM_MultiSurface
   * 
   * @param geo
   *          GM_MultiSurface
   */
  private static GMLMultiPolygon createGMLMultiPolygon( GM_MultiSurface geo, GMLDocument_Impl doc )
      throws GMLException
  {
    Debug.debugMethodBegin( "GMLFactory", "createGMLPoint" );

    GMLMultiPolygon gmlMPolygon = GMLMultiPolygon_Impl.createGMLMultiPolygon( doc.getDocument() );

    try
    {
      for( int i = 0; i < geo.getSize(); i++ )
      {
        gmlMPolygon.addPolygon( createGMLPolygon( geo.getSurfaceAt( i ), doc ) );
      }
    }
    catch( Exception e )
    {
      throw new GMLException( "couldn't create GMLMultiPolygon \n" + e );
    }

    try
    {
      String s = geo.getCoordinateSystem().getName();
      s = s.replace( ' ', ':' );
      s = StringExtend.replace( s, ":", ".xml#", true );
      gmlMPolygon.setSrs( "http://www.opengis.net/gml/srs/" + s );
    }
    catch( Exception e )
    {
      gmlMPolygon.setSrs( "null" );
    }

    Debug.debugMethodEnd();
    return gmlMPolygon;
  }

  /**
   * creates a GMLFeature from a XML Element
   */
  public static GMLFeature createGMLFeature( Element element ) throws GMLException
  {
    return null;
  }

  /**
   * creates a GMLFeature from a XML Element
   */
  public static GMLFeature createGMLFeature( Document doc, Feature feature ) throws GMLException
  {
    Debug.debugMethodBegin( "GMLFactory", "createGMLFeature(Feature)" );

    GMLFeature gmlF = GMLFeature_Impl.createGMLFeature( doc, feature.getFeatureType().getName() );

    FeatureType ft = feature.getFeatureType();
    FeatureTypeProperty[] ftp = ft.getProperties();
    Object[] properties = feature.getProperties();

    for( int i = 0; i < properties.length; i++ )
    {
      GMLProperty prop = null;

      if( properties[i] instanceof GM_Object )
      {
        GMLGeometry geom = createGMLGeometry( (GM_Object)properties[i] );
        prop = GMLGeoProperty_Impl.createGMLGeoProperty( ftp[i].getName(), geom );
      }
      else
      {
        if( properties[i] != null )
        {
          System.out.println("prop:"+properties[i].getClass().toString()+" "+properties[i].toString());
          if( ftp[i] instanceof XLinkFeatureTypeProperty )
            prop = GMLProperty_Impl.createGMLProperty( doc, ftp[i], properties[i].toString() );
          else
            prop = GMLProperty_Impl.createGMLProperty( doc, ftp[i].getName(), properties[i]
                .toString() );
        }
        else
        {
          prop = GMLProperty_Impl.createGMLProperty( doc, ftp[i].getName(), "" );
        }
      }

      gmlF.addProperty( prop );
    }

    Debug.debugMethodEnd();
    return gmlF;
  }

  /**
   * creates an empty GMLFeatureCollection
   */
  public static GMLFeatureCollection createGMLFeatureCollection( String name )
  {
    return new GMLFeatureCollection_Impl( name );
  }

  /**
   * creates a GMLFeatureCollection from a XML Element
   */
  public static GMLFeatureCollection createGMLFeatureCollection( Element element )
      throws GMLException
  {
    return null;
  }
}

/*
 * Changes to this class. What the people haven been up to:
 * 
 * $Log$
 * Revision 1.2  2004/08/11 11:20:16  doemming
 * *** empty log message ***
 * Revision 1.1.1.1 2004/05/11 16:43:24 doemming
 * backup of local modified deegree sources
 * 
 * Revision 1.8 2004/03/29 10:37:13 poth no message
 * 
 * Revision 1.7 2004/02/18 14:55:25 poth no message
 * 
 * Revision 1.6 2004/01/22 07:49:29 poth no message
 * 
 * Revision 1.5 2003/05/15 09:37:40 poth no message
 * 
 * Revision 1.4 2003/04/23 15:44:39 poth no message
 * 
 * Revision 1.3 2003/04/17 11:23:45 axel_schaefer wrong debug-end message in
 * createGMLGeometry(Element)
 * 
 * Revision 1.2 2002/10/21 08:19:02 poth no message
 * 
 * Revision 1.1.1.1 2002/09/25 16:01:03 poth no message
 * 
 * Revision 1.10 2002/08/19 15:59:29 ap no message
 * 
 * Revision 1.9 2002/08/05 16:11:02 ap no message
 * 
 * Revision 1.8 2002/08/01 08:56:56 ap no message
 *  
 */
