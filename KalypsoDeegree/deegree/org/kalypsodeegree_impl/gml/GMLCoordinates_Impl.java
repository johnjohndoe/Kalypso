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

import org.deegree.gml.GMLCoordinates;
import org.deegree.ogcbasic.CommonNamespaces;
import org.deegree.xml.DOMPrinter;
import org.deegree.xml.XMLTools;
import org.deegree_impl.tools.Debug;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;

/**
 * <p>
 * As an alternative to the sequence of &lt;coords&gt;, coordinates can also be
 * conveyed by a single string. By default the coordinates in a tuple are
 * separated by commas, and successive tuples are separated by a space character
 * (#x20). While these delimiters are specified by several attributes, a user is
 * free to define a localized coordinates list that is derived by restriction
 * from gml:CoordinatesType. An instance document could then employ the xsi:type
 * attribute to substitute the localized coordinates list wherever a
 * <coordinates>element is expected; such a subtype could employ other
 * delimiters to reflect local usage.
 * <p>
 * It is expected that a specialized client application will extract and
 * validate string content, as these functions will not be performed by a
 * general XML parser. The formatting attributes will assume their default
 * values if they are not specified for a particular instance; the
 * &lt;coordinates&gt; element must conform to the XML Schema fragments.
 * 
 * <p>
 * ----------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version 07.02.2001
 *          <p>
 */
public class GMLCoordinates_Impl implements GMLCoordinates
{
  private org.w3c.dom.Element element = null;

  /**
   * constructor: creates a GMLCoordinate with an element
   */
  public GMLCoordinates_Impl( org.w3c.dom.Element element )
  {
    Debug.debugMethodBegin( this, "GMLCoordinates_Impl(Element)" );
    this.element = element;
    Debug.debugMethodEnd();
  }

  /**
   * 
   * 
   * @param doc
   * @param coordinates
   * 
   * @return
   */
  public static GMLCoordinates createGMLCoordinates( Document doc, String coordinates )
  {
    Element elem = doc.createElementNS( CommonNamespaces.GMLNS, "gml:coordinates" );
    GMLCoordinates c = new GMLCoordinates_Impl( elem );
    c.setCoordinates( coordinates );
    c.setCoordinateSeperator( ',' );
    c.setDecimalSeperator( '.' );
    c.setTupleSeperator( ' ' );
    return c;
  }

  /**
   * returns the GMLCoordinate as an element
   */
  public Element getAsElement()
  {
    return element;
  }

  /**
   * returns the coordinates in the &lt;coordinates&gt; tag
   */
  public String getCoordinates()
  {
    Debug.debugMethodBegin( this, "getCoordinates" );

    String s = null;
    NodeList nl = element.getChildNodes();

    if( ( nl != null ) && ( nl.getLength() > 0 ) )
    {
      s = element.getFirstChild().getNodeValue();
    }

    Debug.debugMethodEnd();
    return s;
  }

  /**
   * sets the coordinates in the &lt;coordinates&gt; tag
   */
  public void setCoordinates( String coordinates )
  {
    Debug.debugMethodBegin( this, "setCoordinates" );

    // neu, wenn keiner da sonst alten wegschmeissen.
    if( getCoordinates() != null )
    {
      NodeList nl = element.getChildNodes();

      if( ( nl != null ) && ( nl.getLength() > 0 ) )
      {
        for( int i = 0; i < nl.getLength(); i++ )
        {
          element.removeChild( nl.item( i ) );
        }
      }
    }

    Text text = element.getOwnerDocument().createTextNode( coordinates );
    element.appendChild( text );

    Debug.debugMethodEnd();
  }

  /**
   * return the character used as decimal seperator
   */
  public char getDecimalSeperator()
  {
    Debug.debugMethodBegin( this, "getDecimalSeperator()" );

    char decimal = DEFAULT_DECIMAL;

    if( XMLTools.getAttrValue( element, "decimal" ) != null )
    {
      String s = XMLTools.getAttrValue( element, "decimal" );
      decimal = s.charAt( 0 );
    }

    Debug.debugMethodEnd();
    return decimal;
  }

  /**
   * @see org.deegree_impl.gml.GMLCoordinates_Impl#getDecimalSeperator()
   */
  public void setDecimalSeperator( char decimalSeperator )
  {
    Debug.debugMethodBegin( this, "setDecimalSeperator(char)" );
    element.setAttribute( "decimal", new Character( decimalSeperator ).toString() );
    Debug.debugMethodEnd();
  }

  /**
   * return the character used as coordinate seperator
   */
  public char getCoordinateSeperator()
  {
    Debug.debugMethodBegin( this, "getCoordinateSeperator()" );

    char cs = DEFAULT_CS;

    if( XMLTools.getAttrValue( element, "cs" ) != null )
    {
      String s = XMLTools.getAttrValue( element, "cs" );
      cs = s.charAt( 0 );
    }

    Debug.debugMethodEnd();
    return cs;
  }

  /**
   * @see org.deegree_impl.gml.GMLCoordinates_Impl#getCoordinateSeperator()
   */
  public void setCoordinateSeperator( char coordinateSeperator )
  {
    Debug.debugMethodBegin( this, "setCoordinateSeperator(char)" );
    element.setAttribute( "cs", new Character( coordinateSeperator ).toString() );
    Debug.debugMethodEnd();
  }

  /**
   * return the character used as tuple seperator
   */
  public char getTupleSeperator()
  {
    Debug.debugMethodBegin( this, "getTupleSeperator()" );

    char ts = DEFAULT_TS;

    if( XMLTools.getAttrValue( element, "ts" ) != null )
    {
      String s = XMLTools.getAttrValue( element, "ts" );
      ts = s.charAt( 0 );
    }

    Debug.debugMethodEnd();
    return ts;
  }

  /**
   * @see org.deegree_impl.gml.GMLCoordinates_Impl#getTupleSeperator()
   */
  public void setTupleSeperator( char tupleSeperator )
  {
    Debug.debugMethodBegin( this, "setTupleSeperator(char)" );
    element.setAttribute( "ts", new Character( tupleSeperator ).toString() );
    Debug.debugMethodEnd();
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
 * Revision 1.3  2004/10/07 14:09:14  doemming
 * *** empty log message ***
 *
 * Revision 1.1  2004/09/02 23:56:58  doemming
 * *** empty log message ***
 * Revision 1.3 2004/08/31 13:03:31 doemming
 * *** empty log message *** Revision 1.6 2004/04/07 06:43:48 poth no message
 * 
 * Revision 1.5 2004/03/01 07:45:47 poth no message
 * 
 * Revision 1.4 2004/02/23 07:47:48 poth no message
 * 
 * Revision 1.3 2004/01/03 13:46:45 poth no message
 * 
 * Revision 1.2 2003/04/23 15:44:39 poth no message
 * 
 * Revision 1.1.1.1 2002/09/25 16:01:01 poth no message
 * 
 * Revision 1.8 2002/08/19 15:59:29 ap no message
 * 
 * Revision 1.7 2002/08/05 16:11:02 ap no message
 * 
 * Revision 1.6 2002/08/01 08:56:56 ap no message
 */
