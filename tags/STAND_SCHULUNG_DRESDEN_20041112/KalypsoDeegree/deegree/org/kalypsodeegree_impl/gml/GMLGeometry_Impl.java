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

import org.deegree.gml.GMLGeometry;
import org.deegree.ogcbasic.CommonNamespaces;
import org.deegree.xml.DOMPrinter;
import org.deegree.xml.XMLTools;
import org.deegree_impl.tools.Debug;
import org.w3c.dom.Element;

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
public class GMLGeometry_Impl implements GMLGeometry
{
  protected Element element = null;

  /**
   * Creates a new GMLGeometry_Impl object.
   * 
   * @param element
   */
  protected GMLGeometry_Impl( Element element )
  {
    this.element = element;
  }

  /**
   * 
   * 
   * @return
   */
  public Element getAsElement()
  {
    return element;
  }

  /**
   * returns the name of the Geometry. As default a geometry is named using the
   * types specified within the geometry.xsd schema file. But the user is
   * allowed to extend this types to create his own geometries with its own
   * names
   */
  public String getName()
  {
    Debug.debugMethodBegin( this, "getName" );

    String s = element.getNodeName();

    Debug.debugMethodEnd();
    return s;
  }

  /**
   * returns the ID of the geometry
   */
  public String getId()
  {
    Debug.debugMethodBegin( this, "getId" );

    String s = XMLTools.getAttrValue( element, "gid" );

    Debug.debugMethodEnd();
    return s;
  }

  /**
   * @see org.deegree_impl.gml.GMLGeometry_Impl#getId()
   */
  public void setId( String id )
  {
    Debug.debugMethodBegin();

    element.setAttribute( "gid", id );

    Debug.debugMethodEnd();
  }

  /**
   * returns the spatial reference system of the geometry The format is like
   * <tt>EPSG:4326</tt>.
   */
  public String getSrs()
  {

    Debug.debugMethodBegin( this, "getSrs" );

    String s = XMLTools.getAttrValue( element, "srsName" );

    if( ( s != null ) && ( s.length() > 2 ) )
    {
      if( s.startsWith( "http://www.opengis.net/gml/srs/" ) )
      {
        // as declared in the GML 2.1.1 specification
        //http://www.opengis.net/gml/srs/epsg.xml#4326
        int p = s.lastIndexOf( "/" );

        if( p >= 0 )
        {
          s = s.substring( p, s.length() );
          p = s.indexOf( "." );

          String s1 = s.substring( 1, p ).toUpperCase();
          p = s.indexOf( "#" );

          String s2 = s.substring( p + 1, s.length() );
          s = s1 + ":" + s2;
        }
      }
    }
    Debug.debugMethodEnd();
    return s;
  }

  /**
   * @see org.deegree_impl.gml.GMLGeometry_Impl#getSrs()
   */
  public void setSrs( String srs )
  {
    Debug.debugMethodBegin();

    element.setAttribute( "srsName", srs );

    Debug.debugMethodEnd();
  }

  /**
   * 
   * 
   * @return
   */
  public String toString()
  {
    String s = element.getNodeName();
    if( s.indexOf( ':' ) > 0 )
    {
      s = s.substring( 0, s.indexOf( ':' ) );
      if( XMLTools.getAttrValue( element, "xmlns:" + s ) == null )
      {
        element.setAttribute( "xmlns:gml", CommonNamespaces.GMLNS );
      }
    }
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
 * Revision 1.3 2004/08/31 13:02:55 doemming ***
 * empty log message *** Revision 1.10 2004/06/01 15:54:23 poth no message
 * 
 * Revision 1.9 2004/04/27 06:40:43 poth no message
 * 
 * Revision 1.8 2004/04/07 06:43:48 poth no message
 * 
 * Revision 1.7 2004/03/02 07:38:14 poth no message
 * 
 * Revision 1.6 2004/01/03 13:46:45 poth no message
 * 
 * Revision 1.5 2003/11/26 17:05:35 poth no message
 * 
 * Revision 1.4 2003/06/10 07:52:07 poth no message
 * 
 * Revision 1.3 2003/04/23 15:44:40 poth no message
 * 
 * Revision 1.2 2002/12/16 10:55:49 poth no message
 * 
 * Revision 1.1.1.1 2002/09/25 16:01:04 poth no message
 * 
 * Revision 1.7 2002/08/19 15:59:29 ap no message
 * 
 * Revision 1.6 2002/08/05 16:11:02 ap no message
 * 
 *  
 */
