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
   * @see java.lang.Object#toString()
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
 * Revision 1.5  2005/02/28 13:34:14  doemming
 * *** empty log message ***
 *
 * Revision 1.4  2005/01/18 12:50:42  doemming
 * *** empty log message ***
 *
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
