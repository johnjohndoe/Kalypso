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

import org.deegree.gml.GMLComplexProperty;
import org.deegree.gml.GMLProperty;
import org.deegree.model.feature.FeatureAssociationTypeProperty;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.ogcbasic.CommonNamespaces;
import org.deegree.xml.DOMPrinter;
import org.deegree.xml.XMLTools;
import org.deegree_impl.tools.Debug;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
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
public class GMLProperty_Impl implements GMLProperty
{
  protected Element element = null;

  private FeatureTypeProperty myFeatureTypeProperty = null;

  /**
   * Creates a new GMLProperty_Impl object.
   * 
   * @param element
   */
  public GMLProperty_Impl( Element element )
  {
    this.element = element;
  }

  public GMLProperty_Impl( FeatureTypeProperty ftp, Element element )
  {
    myFeatureTypeProperty = ftp;
    this.element = element;
  }

  /**
   *  
   */
  public Element getAsElement()
  {
    return element;
  }

  /**
   * returns the name of the property
   */
  public String getName()
  {
    Debug.debugMethodBegin( this, "getName" );
    Debug.debugMethodEnd();
    String namespaceURI = element.getNamespaceURI();
    if( namespaceURI != null )
      return element.getNamespaceURI() + ":" + element.getLocalName();
    return element.getLocalName();
    //    String s = element.getNodeName();
    //    return s;
  }

  /**
   * returns true if the submitted element contains just excatly one
   * childelement that have to be a geometry
   */
  private boolean isGeometryProperty()
  {
    boolean flag = false;
    String name = XMLTools.toLocalName( element.getNodeName() );

    if( name.equals( "pointProperty" ) || name.equals( "lineStringProperty" )
        || name.equals( "polygonProperty" ) || name.equals( "multiPointProperty" )
        || name.equals( "multiLineStringProperty" ) || name.equals( "multiPolygonProperty" )
        || name.equals( "boundedBy" ) )
    {
      flag = true;
    }

    if( !flag )
    {
      NodeList nl = element.getChildNodes();

      if( nl != null )
      {
        for( int i = 0; i < nl.getLength(); i++ )
        {
          if( nl.item( i ) instanceof Element )
          {
            Element el = (Element)nl.item( i );
            name = XMLTools.toLocalName( el.getNodeName() );

            if( name.equals( "Point" ) || name.equals( "LineString" ) || name.equals( "Polygon" )
                || name.equals( "MultiPoint" ) || name.equals( "MultiLineString" )
                || name.equals( "MultiPolygon" ) || name.equals( "Box" )
                || name.equals( "MultiGeometry" ) )
            {
              flag = true;
            }
            else
            {
              flag = false;
              break;
            }
          }
        }
      }
    }

    Debug.debugMethodEnd();
    return flag;
  }

  /**
   * returns the type of a property.
   * <p>
   * this method may be removed in future definitions of this interface because
   * this can be determined by validiating the property name against the
   * describing schema.
   * </p>
   * <p>
   * At the moment only String-type and geometry types are supported. Complex
   * types like feature or feature collections aren't supported yet.
   * </p>
   */
  public int getPropertyType()
  {
    Debug.debugMethodBegin( this, "getPropertyType" );

    int result = UNKNOWNTYPE;

    // if the node name is associated with a formal gml geo property
    // name its a geometry
    if( isGeometryProperty() )
    {
      // get the geo type of the node
      result = getGeoType();
    }
    // if the first child is a text node STRING is
    // the right result
    else if( XMLTools.getFirstElement( element ) == null )
    {
      result = STRING;
    }
    else if( element.getElementsByTagNameNS( CommonNamespaces.GMLNS, "featureMember" ).getLength() > 0 )
    {
      result = FEATURECOLLECTION;
    }
    else if( XMLTools.getFirstElement( element ) != null )
    {
      result = FEATURE;
    } // else the result is unknown
    else
    {
      result = UNKNOWNTYPE;
    }

    Debug.debugMethodEnd();
    return result;
  }

  /**
   * performs the mapping of the gml geoType (String) to its int representation
   * for more simple handling.
   */
  private int getGeoType()
  {
    Debug.debugMethodBegin( this, "getGeoType" );

    // constants are inherited from GMLProperty
    int result = UNKNOWNTYPE;

    NodeList nl = element.getChildNodes();

    if( ( nl != null ) && ( nl.getLength() > 0 ) )
    {
      for( int i = 0; i < nl.getLength(); i++ )
      {
        if( nl.item( i ) instanceof Element )
        {
          Element el = (Element)nl.item( i );
          String name = XMLTools.toLocalName( el.getNodeName() );

          if( name.equals( "Point" ) )
          {
            result = POINT;
          }
          else if( name.equals( "LineString" ) )
          {
            result = LINESTRING;
          }
          else if( name.equals( "Polygon" ) )
          {
            result = POLYGON;
          }
          else if( name.equals( "MultiPoint" ) )
          {
            result = MULTIPOINT;
          }
          else if( name.equals( "MultiLineString" ) )
          {
            result = MULTILINESTRING;
          }
          else if( name.equals( "MultiPolygon" ) )
          {
            result = MULTIPOLYGON;
          }
          else if( name.equals( "Box" ) )
          {
            result = BOX;
          }
          else if( name.equals( "MultiGeometry" ) )
          {
            result = MULTIGEOMETRY;
          }
          else
          {
            result = UNKNOWNTYPE;
          }
        }
      }
    }
    else
    {
      String name = XMLTools.toLocalName( element.getNodeName() );

      if( name.equals( "pointProperty" ) )
      {
        result = POINT;
      }
      else if( name.equals( "lineStringProperty" ) )
      {
        result = LINESTRING;
      }
      else if( name.equals( "polygonProperty" ) )
      {
        result = POLYGON;
      }
      else if( name.equals( "multiPointProperty" ) )
      {
        result = MULTIPOINT;
      }
      else if( name.equals( "multiLineStringProperty" ) )
      {
        result = MULTILINESTRING;
      }
      else if( name.equals( "multiPolygonProperty" ) )
      {
        result = MULTIPOLYGON;
      }
      else if( name.equals( "boundedBy" ) )
      {
        result = BOX;
      }
    }

    Debug.debugMethodEnd();
    return result;
  }

  /**
   * returns the value of the property
   */
  public Object getPropertyValue()
  {
    Debug.debugMethodBegin( this, "getPropertyValue" );

    Object result = null;

    // if the node is an instance of a text node, then return
    // it value (text)
    int pt = getPropertyType();

    switch( pt )
    {
    case STRING:
    {
      Node node = element.getFirstChild();

      if( node != null )
      {
        result = node.getNodeValue();
      }

      break;
    }
    case POINT:
      result = new GMLPoint_Impl( XMLTools.getFirstElement( element ) );
      break;
    case LINESTRING:
      result = new GMLLineString_Impl( XMLTools.getFirstElement( element ) );
      break;
    case POLYGON:
      result = new GMLPolygon_Impl( XMLTools.getFirstElement( element ) );
      break;
    case MULTIPOINT:
      result = new GMLMultiPoint_Impl( XMLTools.getFirstElement( element ) );
      break;
    case MULTILINESTRING:
      result = new GMLMultiLineString_Impl( XMLTools.getFirstElement( element ) );
      break;
    case MULTIPOLYGON:
      result = new GMLMultiPolygon_Impl( XMLTools.getFirstElement( element ) );
      break;
    case MULTIGEOMETRY:
      result = new GMLGeometryCollection_Impl( XMLTools.getFirstElement( element ) );
      break;
    case FEATURE:
      if( this instanceof GMLComplexProperty )
        result = ( (GMLComplexProperty)this ).getComplexPropertyValue();
      else
        result = new GMLFeature_Impl( element );
      break;
    case FEATURECOLLECTION:
      result = new GMLFeatureCollection_Impl( element );
      break;
    }

    Debug.debugMethodEnd();
    return result;
  }

  /**
   * @see org.deegree_impl.gml.GMLProperty_Impl#getPropertyValue()
   *      <p>
   *      This set-method only supports text propetries. More complex property
   *      types must be inherited from this interface and define additional
   *      set-methods.
   *      </p>
   *      <p>
   *      Notice: if the old value has been removed this may change the
   *      properties type. At a future release a validation against the schema
   *      of the document has to be performed.
   *      </p>
   */
  public void setPropertyValue( String value )
  {
    Debug.debugMethodBegin( this, "setPropertyValue" );

    if( myFeatureTypeProperty instanceof FeatureAssociationTypeProperty )
    {
      setAttributeValue( "#" + value );
      return;
    }
    Node node = element.getFirstChild();

    // remove the propetry value if it already exist
    if( node != null )
    {
      element.removeChild( node );
    }

    Node node_ = null;
    if( value != null
        && ( value.length() > 1000 || value.indexOf( '<' ) >= 0 || value.indexOf( '>' ) >= 0
            || value.indexOf( '&' ) >= 0 || value.indexOf( '"' ) >= 0 || value.indexOf( "'" ) >= 0 ) )
    {
      node_ = element.getOwnerDocument().createCDATASection( value );
    }
    else
    {
      if( value != null )
      {
        node_ = element.getOwnerDocument().createTextNode( value );
      }
      else
      {
        node_ = element.getOwnerDocument().createTextNode( "" );
      }
    }

    // set the new properties value.
    element.appendChild( node_ );
    Debug.debugMethodEnd();
  }

  private void setAttributeValue( String value )
  {
    //  element.setAttribute("xmlns:xlink","http://www.w3.org/1999/xlink");
    element.setAttributeNS( "http://www.w3.org/1999/xlink", "xlink:href", value );
    // TODO use full qualified namespace-name
  }

  /**
   * 
   * @see java.lang.Object#toString()
   */
  public String toString()
  {
    return DOMPrinter.nodeToString( element, "" );
  }

  /**
   * @see org.deegree.gml.GMLProperty#getAttributeValue(java.lang.String,
   *      java.lang.String)
   */
  public Object getAttributeValue( String nameSpace, String attributeValue )
  {
    return element.getAttributeNS( nameSpace, attributeValue );
  }

  /**
   * 
   * @see org.deegree.gml.GMLProperty#getElement()
   */
  public Element getElement()
  {
    return element;
  }

  public void setPropertyValue( Element valueElement )
  {
    Node node = element.getFirstChild();
    // remove the propetry value if it already exist
    if( node != null )
    {
      element.removeChild( node );
    }
    element.appendChild( valueElement );
  }
}

/*
 * Changes to this class. What the people haven been up to:
 * 
 * $Log$
 * Revision 1.11  2005/02/08 18:43:59  belger
 * *** empty log message ***
 *
 * Revision 1.10  2005/01/18 12:50:42  doemming
 * *** empty log message ***
 *
 * Revision 1.9  2005/01/12 10:40:53  doemming
 * *** empty log message ***
 * Revision 1.8 2004/11/23 10:37:57 doemming ***
 * empty log message *** Revision 1.7 2004/11/22 01:29:50 doemming *** empty log
 * message ***
 * 
 * Revision 1.6 2004/11/16 10:44:16 doemming *** empty log message ***
 * 
 * Revision 1.5 2004/10/07 14:09:13 doemming *** empty log message ***
 * 
 * Revision 1.1 2004/09/02 23:56:58 doemming *** empty log message *** Revision
 * 1.4 2004/08/31 14:35:15 doemming *** empty log message *** Revision 1.3
 * 2004/08/18 20:27:32 belger *** empty log message ***
 * 
 * Revision 1.2 2004/08/11 11:20:16 doemming *** empty log message ***
 * 
 * Revision 1.1.1.1 2004/05/11 16:43:24 doemming backup of local modified
 * deegree sources
 * 
 * Revision 1.6 2004/03/02 07:38:14 poth no message
 * 
 * Revision 1.5 2003/09/22 09:58:05 poth no message
 * 
 * Revision 1.4 2003/06/03 15:56:39 poth no message
 * 
 * Revision 1.3 2003/05/15 09:37:40 poth no message
 * 
 * Revision 1.2 2003/04/23 15:44:40 poth no message
 * 
 * Revision 1.1.1.1 2002/09/25 16:01:05 poth no message
 * 
 * Revision 1.9 2002/08/19 15:59:29 ap no message
 * 
 * Revision 1.8 2002/08/05 16:11:02 ap no message
 * 
 * Revision 1.7 2002/08/01 08:56:56 ap no message
 * 
 * Revision 1.6 2002/07/31 06:26:06 ap no message
 * 
 * Revision 1.5 2002/07/19 14:47:08 ap no message
 * 
 *  
 */
