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
package org.deegree_impl.gml;

import org.deegree.gml.GMLBox;
import org.deegree.gml.GMLException;
import org.deegree.gml.GMLGeoProperty;
import org.deegree.gml.GMLGeometry;
import org.deegree.gml.GMLGeometryCollection;
import org.deegree.gml.GMLLineString;
import org.deegree.gml.GMLMultiLineString;
import org.deegree.gml.GMLMultiPoint;
import org.deegree.gml.GMLMultiPolygon;
import org.deegree.gml.GMLPoint;
import org.deegree.gml.GMLPolygon;
import org.deegree.ogcbasic.CommonNamespaces;
import org.deegree.xml.DOMPrinter;
import org.deegree.xml.XMLTools;
import org.deegree_impl.tools.Debug;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * 
 * 
 * <p>
 * ----------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version 07.02.2001
 *          <p>
 */
public class GMLGeoProperty_Impl extends GMLProperty_Impl implements GMLGeoProperty
{
  /**
   * Creates a new GMLGeoProperty_Impl object.
   * 
   * @param element
   */
  public GMLGeoProperty_Impl( Element element )
  {
    super( element );
  }

  /**
   * factory method to create a GMLGeoProperty. the property that will be return
   * doesn't contain a value.
   */
  public static GMLGeoProperty createGMLGeoProperty( Document doc, int geoType )
      throws GMLException
  {
    Debug.debugMethodBegin( "GMLProperty_Impl", "createGMLGeoProperty(Document, int)" );

    Element elem = null;

    // constants are inherited from GMLProperty
    switch( geoType )
    {
    case POINT:
      elem = doc.createElementNS( CommonNamespaces.GMLNS, "gml:pointProperty" );
      break;
    case LINESTRING:
      elem = doc.createElementNS( CommonNamespaces.GMLNS, "gml:lineStringProperty" );
      break;
    case POLYGON:
      elem = doc.createElementNS( CommonNamespaces.GMLNS, "gml:polygonProperty" );
      break;
    case MULTIPOINT:
      elem = doc.createElementNS( CommonNamespaces.GMLNS, "gml:multiPointProperty" );
      break;
    case MULTILINESTRING:
      elem = doc.createElementNS( CommonNamespaces.GMLNS, "gml:multiLineStringProperty" );
      break;
    case MULTIPOLYGON:
      elem = doc.createElementNS( CommonNamespaces.GMLNS, "gml:multiPolygonProperty" );
      break;
    case MULTIGEOMETRY:
      elem = doc.createElementNS( CommonNamespaces.GMLNS, "gml:multiGeometryProperty" );
      break;
    default:
      throw new GMLException( geoType + ": isn't a valid identifier for a geometry property!" );
    }

    GMLGeoProperty ls = new GMLGeoProperty_Impl( elem );

    Debug.debugMethodEnd();
    return ls;
  }

  /**
   * factory method to create a GMLGeoProperty. the property that will be return
   * doesn't contain a value.
   */
  public static GMLGeoProperty createGMLGeoProperty( String name, GMLGeometry geom )
      throws GMLException
  {
    Debug.debugMethodBegin( "GMLProperty_Impl", "createGMLGeoProperty(String,GMLGeometry)" );

    Document doc = ( (GMLGeometry_Impl)geom ).getAsElement().getOwnerDocument();
    Element elem = doc.createElement( name );
    elem.appendChild( ( (GMLGeometry_Impl)geom ).getAsElement() );

    GMLGeoProperty g = new GMLGeoProperty_Impl( elem );

    Debug.debugMethodEnd();

    return g;
  }

  /**
   * factory method to create a GMLGeoProperty. the property that will be return
   * doesn't contain a value.
   */
  public static GMLGeoProperty createGMLGeoProperty( GMLGeometry geom ) throws GMLException
  {
    Debug.debugMethodBegin( "GMLProperty_Impl", "createGMLGeoProperty(GMLGeometry)" );

    int geoType = 0;

    if( geom instanceof GMLPoint )
    {
      geoType = POINT;
    }
    else if( geom instanceof GMLLineString )
    {
      geoType = LINESTRING;
    }
    else if( geom instanceof GMLPolygon )
    {
      geoType = POLYGON;
    }
    else if( geom instanceof GMLMultiPoint )
    {
      geoType = MULTIPOINT;
    }
    else if( geom instanceof GMLMultiLineString )
    {
      geoType = MULTILINESTRING;
    }
    else if( geom instanceof GMLMultiPolygon )
    {
      geoType = MULTIPOLYGON;
    }
    else if( geom instanceof GMLGeometryCollection )
    {
      geoType = MULTIGEOMETRY;
    }
    else if( geom instanceof GMLBox )
    {
      geoType = BOX;
    }

    Document doc = ( (GMLGeometry_Impl)geom ).getAsElement().getOwnerDocument();
    GMLGeoProperty ls = createGMLGeoProperty( doc, geoType );

    ls.setGeoPropetryValue( geom );

    Debug.debugMethodEnd();
    return ls;
  }

  /**
   * returns the value of the property
   */
  public GMLGeometry getGeoPropertyValue()
  {
    Debug.debugMethodBegin( this, "getGeoPropertyValue" );

    GMLGeometry result = null;

    // get the element holding the geometry
    Element geom = XMLTools.getFirstElement( element );

    // determine the geometry type
    int type = this.getPropertyType();

    // constants are inherited from GMLProperty
    switch( type )
    {
    case POINT:
      result = new GMLPoint_Impl( geom );
      break;
    case LINESTRING:
      result = new GMLLineString_Impl( geom );
      break;
    case POLYGON:
      result = new GMLPolygon_Impl( geom );
      break;
    case MULTIPOINT:
      result = new GMLMultiPoint_Impl( geom );
      break;
    case MULTILINESTRING:
      result = new GMLMultiLineString_Impl( geom );
      break;
    case MULTIPOLYGON:
      result = new GMLMultiPolygon_Impl( geom );
      break;
    case MULTIGEOMETRY:
      result = new GMLGeometryCollection_Impl( geom );
      break;
    default:
      result = null;
    }

    Debug.debugMethodEnd();
    return result;
  }

  /**
   * @see org.deegree_impl.gml.GMLGeoProperty_Impl#getPropertyValue()
   */
  public void setGeoPropetryValue( GMLGeometry value ) throws GMLException
  {
    Debug.debugMethodBegin( this, "setGeoPropertyValue" );

    // this method will throw a exception if the submitted geometry
    // isn't of the same type as the property respresented by the class
    validate( value );

    NodeList nl = element.getChildNodes();

    if( nl != null )
    {
      for( int i = 0; i < nl.getLength(); i++ )
      {
        element.removeChild( nl.item( i ) );
      }
    }

    Element elem = ( (GMLGeometry_Impl)value ).getAsElement();

    XMLTools.insertNodeInto( elem, element );

    Debug.debugMethodEnd();
  }

  /**
   * validate if the submitted geometry is the same type as the geometry tag
   */
  private void validate( GMLGeometry value ) throws GMLException
  {
    Debug.debugMethodBegin( this, "validate" );

    int geoType = this.getPropertyType();

    // constants are inherited from GMLProperty
    switch( geoType )
    {
    case POINT:
    {
      if( !( value instanceof GMLPoint ) )
      {
        throw new GMLException( "A pointProperty only can contain points!" );
      }

      break;
    }
    case LINESTRING:
    {
      if( !( value instanceof GMLLineString ) )
      {
        throw new GMLException( "A lineStringProperty only can contain linestrings!" );
      }

      break;
    }
    case POLYGON:
    {
      if( !( value instanceof GMLPolygon ) )
      {
        throw new GMLException( "A polygonProperty only can contain polygons!" );
      }

      break;
    }
    case MULTIPOINT:
    {
      if( !( value instanceof GMLMultiPoint ) )
      {
        throw new GMLException( "A multiPointProperty only can contain multipoints!" );
      }

      break;
    }
    case MULTILINESTRING:
    {
      if( !( value instanceof GMLMultiLineString ) )
      {
        throw new GMLException( "A multiLineStringProperty only can contain multilinestrings!" );
      }

      break;
    }
    case MULTIPOLYGON:
    {
      if( !( value instanceof GMLMultiPolygon ) )
      {
        throw new GMLException( "A multiPolygonProperty only can contain multipolygons!" );
      }

      break;
    }
    case MULTIGEOMETRY:
    {
      if( !( value instanceof GMLGeometryCollection ) )
      {
        throw new GMLException( "A multiGeometryProperty only can contain geometry collections!" );
      }

      break;
    }
    default:
      throw new GMLException( geoType + ": isn't a valid identifier for a geometry property!" );
    }

    Debug.debugMethodEnd();
  }

  /**
   * will be overwritten by this class in a way that an exception will be thrown
   * if the method is called. Because it doesn't make any sense to set the value
   * of a geo property as a String.
   */
  public void setPropertyValue( String value )
  {
    throw new NoSuchMethodError( "It doesn't make any sense to set the value "
        + "of a geo property as a String" );
  }

  /**
   * 
   * 
   * @return
   */
  public String toString()
  {
    return DOMPrinter.nodeToString( element, "" );
  }
}

/*
 * Changes to this class. What the people haven been up to:
 * 
 * $Log$
 * Revision 1.4  2005/01/18 12:50:42  doemming
 * *** empty log message ***
 *
 * Revision 1.3  2004/10/07 14:09:14  doemming
 * *** empty log message ***
 *
 * Revision 1.1  2004/09/02 23:56:58  doemming
 * *** empty log message ***
 * Revision 1.3 2004/08/31 13:03:30 doemming
 * *** empty log message *** Revision 1.7 2004/04/07 06:43:48 poth no message
 * 
 * Revision 1.6 2004/03/02 07:38:14 poth no message
 * 
 * Revision 1.5 2003/11/26 17:05:35 poth no message
 * 
 * Revision 1.4 2003/11/26 07:47:18 poth no message
 * 
 * Revision 1.3 2003/04/23 15:44:39 poth no message
 * 
 * Revision 1.2 2003/02/24 09:34:42 poth no message
 * 
 * Revision 1.1.1.1 2002/09/25 16:01:02 poth no message
 * 
 * Revision 1.8 2002/08/19 15:59:29 ap no message
 * 
 * Revision 1.7 2002/08/05 16:11:02 ap no message
 * 
 * Revision 1.6 2002/08/01 08:56:56 ap no message
 *  
 */
