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
package org.kalypsodeegree_impl.gml;

import java.net.URL;
import java.util.Iterator;
import java.util.List;

import org.kalypsodeegree.gml.GMLCoordinates;
import org.kalypsodeegree.gml.GMLDocument;
import org.kalypsodeegree.gml.GMLException;
import org.kalypsodeegree.gml.GMLFeature;
import org.kalypsodeegree.gml.GMLFeatureCollection;
import org.kalypsodeegree.gml.GMLGeometry;
import org.kalypsodeegree.gml.GMLLineString;
import org.kalypsodeegree.gml.GMLLinearRing;
import org.kalypsodeegree.gml.GMLMultiLineString;
import org.kalypsodeegree.gml.GMLMultiPoint;
import org.kalypsodeegree.gml.GMLMultiPolygon;
import org.kalypsodeegree.gml.GMLPoint;
import org.kalypsodeegree.gml.GMLPolygon;
import org.kalypsodeegree.gml.GMLProperty;
import org.kalypsodeegree.model.feature.DeegreeFeature;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureAssociationTypeProperty;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_LineString;
import org.kalypsodeegree.model.geometry.GM_MultiCurve;
import org.kalypsodeegree.model.geometry.GM_MultiPoint;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.ogcbasic.CommonNamespaces;
import org.kalypsodeegree.xml.XMLTools;
import org.kalypsodeegree_impl.extension.ITypeHandler;
import org.kalypsodeegree_impl.extension.TypeRegistryException;
import org.kalypsodeegree_impl.extension.TypeRegistrySingleton;
import org.kalypsodeegree_impl.gml.schema.Mapper;
import org.kalypsodeegree_impl.tools.Debug;
import org.kalypsodeegree_impl.tools.StringExtend;
import org.w3c.dom.Element;

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
   * @param element DOM Element containing a GMLGeometry
   */
  public synchronized static GMLGeometry createGMLGeometry( Element element )
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
   * @param geo geometry
   */
  public synchronized static GMLGeometry createGMLGeometry( GMLDocument doc, GM_Object geo ) throws GMLException
  {
    Debug.debugMethodBegin( "GMLFactory", "createGMLGeometry(GM_Object)" );

    if( doc == null )
      doc = new GMLDocument_Impl();
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
   * @param geo point
   */
  private static GMLPoint createGMLPoint( GM_Point geo, GMLDocument doc )
  {
    Debug.debugMethodBegin( "GMLFactory", "createGMLPoint" );

    Element coord = doc.getDocument().createElementNS( CommonNamespaces.GMLNS, "gml:coordinates" );

    GMLCoordinates gmlCo = new GMLCoordinates_Impl( coord );
    gmlCo.setCoordinates( geo.getX() + "," + geo.getY() + ( ( geo.getCoordinateDimension() == 3 ) ? "," + geo.getZ() : "" ) );
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
   * @param geo GM_Curve
   */
  private static GMLLineString createGMLLineString( GM_Curve geo, GMLDocument doc ) throws GMLException
  {
    Debug.debugMethodBegin( "GMLFactory", "createGMLLineString" );

    Element coord = doc.getDocument().createElementNS( CommonNamespaces.GMLNS, "gml:coordinates" );

    GMLCoordinates gmlCo = new GMLCoordinates_Impl( coord );

    StringBuffer sb = null;

    try
    {
      GM_LineString ls = geo.getAsLineString();

      sb = new StringBuffer( ls.getNumberOfPoints() * 35 );

      for( int i = 0; i < ls.getNumberOfPoints(); i++ )
      {
        GM_Position pt = ls.getPositionAt( i );
        sb.append( pt.getX() + "," + pt.getY() + ( ( pt.getAsArray().length == 3 ) ? "," + pt.getZ() : "" ) + " " );
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
   * @param geo GM_Surface
   */
  private static GMLPolygon createGMLPolygon( GM_Surface geo, GMLDocument doc ) throws GMLException
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
    Element coord = doc.getDocument().createElementNS( CommonNamespaces.GMLNS, "gml:coordinates" );
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
        coord = doc.getDocument().createElementNS( CommonNamespaces.GMLNS, "gml:coordinates" );
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
   * @param geo GM_MultiPoint
   */
  private static GMLMultiPoint createGMLMultiPoint( GM_MultiPoint geo, GMLDocument doc ) throws GMLException
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
   * @param geo GM_MultiCurve
   */
  private static GMLMultiLineString createGMLMultiLineString( GM_MultiCurve geo, GMLDocument doc ) throws GMLException
  {
    Debug.debugMethodBegin( "GMLFactory", "createGMLPoint" );

    GMLMultiLineString gmlMLineString = GMLMultiLineString_Impl.createGMLMultiLineString( doc.getDocument() );

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
   * @param geo GM_MultiSurface
   */
  private static GMLMultiPolygon createGMLMultiPolygon( GM_MultiSurface geo, GMLDocument doc ) throws GMLException
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
  public static GMLFeature createGMLFeature( final GMLDocument doc, final DeegreeFeature feature, URL context ) throws GMLException
  {
    Debug.debugMethodBegin( "GMLFactory", "createGMLFeature(Feature)" );

    final GMLFeature gmlF = doc.createGMLFeature( feature.getFeatureType() );

    final FeatureType ft = feature.getFeatureType();
    final FeatureTypeProperty[] ftp = ft.getProperties();
    final Object[] properties = feature.getProperties();
    for( int i = 0; i < properties.length; i++ )
    {
      GMLProperty prop = null;

      if( properties[i] instanceof GM_Object )
      {
        prop = doc.createGMLGeoProperty( ftp[i], (GM_Object)properties[i] );
      }
      else
      {
        if( properties[i] != null )
          prop = doc.createGMLProperty( ftp[i], properties[i].toString() );
        else
          prop = doc.createGMLProperty( ftp[i], "" );
      }

      gmlF.addProperty( prop );
    }

    Debug.debugMethodEnd();
    return gmlF;
  }

  /**
   * creates an empty GMLFeatureCollection
   * 
   * @deprecated
   */
  public static GMLFeatureCollection createGMLFeatureCollection( String name )
  {
    return new GMLFeatureCollection_Impl( name );
  }

  public static GMLFeature createGMLFeature( final GMLDocument doc, Feature feature, URL context ) throws GMLException
  {
    Debug.debugMethodBegin( "GMLFactory", "createGMLFeature(Feature)" );

    final GMLFeature gmlFeature = doc.createGMLFeature( feature.getFeatureType() );

    final FeatureType ft = feature.getFeatureType();

    final FeatureTypeProperty[] ftp = ft.getProperties();
    final Object[] properties = feature.getProperties();
    for( int i = 0; i < ftp.length; i++ )
      addGMLProperties( doc, context, gmlFeature, properties[i], ftp[i], ft.getMinOccurs( i ) );

    final String id = feature.getId();
    if( id != null )
      gmlFeature.setId( id );
    return gmlFeature;
  }

  private static void addGMLProperties( final GMLDocument doc, URL context, final GMLFeature gmlFeature, final Object value, final FeatureTypeProperty ftp,
      final int min ) throws GMLException
  {

    // marshalling
    final ITypeHandler typeHandler = TypeRegistrySingleton.getTypeRegistry().getTypeHandlerForClassName( ftp.getType() );

    GMLProperty prop = null;
    if( value instanceof List )
    {
      final Iterator iterator = ( (List)value ).iterator();
      while( iterator.hasNext() )
        addGMLProperties( doc, context, gmlFeature, iterator.next(), ftp, min );
    }
    else if( value == null )
    {
      if( min > 0 )
        prop = doc.createGMLProperty( ftp, "" );
    }
    else if( typeHandler != null )
    {
      final Element element = doc.createElementNS( ftp.getNamespace(), ftp.getName() );
      try
      {
        //      TODO give context not null
        typeHandler.marshall( value, element, context );
      }
      catch( TypeRegistryException e )
      {
        e.printStackTrace();
      }
      prop = new GMLCustomProperty_Impl( ftp, element );
    }
    else if( value instanceof GM_Object )
    {
      prop = doc.createGMLGeoProperty( ftp, (GM_Object)value );
    }
    else if( value instanceof Feature )
    {
      final Feature fe = (Feature)value;
      final GMLFeature gmlFe = createGMLFeature( doc, fe, context );
      prop = doc.createGMLProperty( ftp, gmlFe.getAsElement() );
    }
    else if( value instanceof String && ftp instanceof FeatureAssociationTypeProperty )
    {
      // gmlproperty of featureassociation must be created with featuretype
      String href = value.toString(); //fid
      if( href != null && href.length() > 0 )
        prop = doc.createGMLProperty( ftp, href );
    }
    else
      prop = doc.createGMLProperty( ftp, Mapper.mapJavaValueToXml( value ) );

    // TODO integrate typehandler ??
    if( prop != null )
      gmlFeature.addProperty( prop );
  }
}

/*
 * Changes to this class. What the people haven been up to:
 * 
 * $Log$
 * Revision 1.17  2005/06/15 15:16:58  doemming
 * *** empty log message ***
 *
 * Revision 1.16  2005/06/05 22:43:54  doemming
 * *** empty log message ***
 * Revision 1.15 2005/05/03 11:38:52 belger *** empty
 * log message ***
 * 
 * Revision 1.14 2005/03/13 12:52:15 belger *** empty log message ***
 * 
 * Revision 1.13 2005/03/08 11:01:04 doemming *** empty log message ***
 * 
 * Revision 1.12 2005/03/04 15:05:04 doemming *** empty log message ***
 * 
 * Revision 1.11 2005/02/28 13:34:14 doemming *** empty log message *** Revision
 * 1.10 2005/02/15 17:52:53 belger *** empty log message ***
 * 
 * Revision 1.9 2005/02/08 18:43:59 belger *** empty log message *** Revision
 * 1.8 2005/01/18 12:50:42 doemming *** empty log message ***
 * 
 * Revision 1.7 2004/11/22 01:29:50 doemming *** empty log message ***
 * 
 * Revision 1.6 2004/10/07 19:28:24 doemming *** empty log message ***
 * 
 * Revision 1.5 2004/10/07 14:09:13 doemming *** empty log message *** Revision
 * 1.1 2004/09/02 23:56:58 doemming *** empty log message *** Revision 1.3
 * 2004/08/31 13:03:31 doemming *** empty log message *** Revision 1.9
 * 2004/04/07 06:43:48 poth no message
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
