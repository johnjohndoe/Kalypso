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

import org.deegree.gml.GMLCoord;
import org.deegree.gml.GMLCoordinates;
import org.deegree.gml.GMLPoint;
import org.deegree.ogcbasic.CommonNamespaces;
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
public class GMLPoint_Impl extends GMLGeometry_Impl implements GMLPoint
{
  /**
   * Creates a new GMLPoint_Impl object.
   * 
   * @param element
   */
  public GMLPoint_Impl( Element element )
  {
    super( element );
  }

  /**
   * factory method to create a GMLPoint. The default location of the point is
   * [-9E99,-9E99]
   */
  public static GMLPoint createGMLPoint( Document doc )
  {
    Debug.debugMethodBegin( "", "createGMLPoint" );

    Element elem = doc.createElementNS( CommonNamespaces.GMLNS, "gml:Point" );
    GMLPoint point = new GMLPoint_Impl( elem );

    // create a coord to located the point
    GMLCoord coord = GMLCoord_Impl.createGMLCoord( doc );
    coord.setCoord( -9E99, -9E99 );

    // set the default coordinate of the point
    point.setCoord( coord );

    Debug.debugMethodEnd();
    return point;
  }

  /**
   * returns the coordinate (location) of the point as GMLCoord
   */
  public GMLCoord getCoord()
  {
    Debug.debugMethodBegin( this, "getCoord" );

    NodeList nl = element.getElementsByTagNameNS( CommonNamespaces.GMLNS, "coord" );
    GMLCoord coord = null;

    if( ( nl != null ) && ( nl.getLength() > 0 ) )
    {
      coord = new GMLCoord_Impl( (Element)nl.item( 0 ) );
    }

    Debug.debugMethodEnd();
    return coord;
  }

  /**
   * @see org.deegree_impl.gml.GMLPoint_Impl#getCoord()
   */
  public void setCoord( GMLCoord coord )
  {
    Debug.debugMethodBegin( this, "setCoord" );

    NodeList nl = element.getElementsByTagNameNS( CommonNamespaces.GMLNS, "coord" );

    // if a coord tag already exists remove it
    if( ( nl != null ) && ( nl.getLength() > 0 ) )
    {
      element.removeChild( nl.item( 0 ) );
    }

    nl = element.getElementsByTagNameNS( CommonNamespaces.GMLNS, "coordinates" );

    // if a coordinates tag exists remove it
    if( ( nl != null ) && ( nl.getLength() > 0 ) )
    {
      element.removeChild( nl.item( 0 ) );
    }

    // insert the submitted coord
    XMLTools.insertNodeInto( ( (GMLCoord_Impl)coord ).getAsElement(), element );

    Debug.debugMethodEnd();
  }

  /**
   * returns the coordinate (location) of the point as GMLCoordinates
   */
  public GMLCoordinates getCoordinates()
  {
    Debug.debugMethodBegin( this, "getCoordinates()" );

    NodeList nl = element.getElementsByTagNameNS( CommonNamespaces.GMLNS, "coordinates" );

    GMLCoordinates c = null;

    if( ( nl != null ) && ( nl.getLength() > 0 ) )
    {
      c = new GMLCoordinates_Impl( (Element)nl.item( 0 ) );
    }

    Debug.debugMethodEnd();
    return c;
  }

  /**
   * @see org.deegree_impl.gml.GMLPoint_Impl#getCoordinates()
   */
  public void setCoordinates( GMLCoordinates coordinates )
  {
    Debug.debugMethodBegin( this, "setCoordinates(GMLCoordinates)" );

    NodeList nl = element.getElementsByTagNameNS( CommonNamespaces.GMLNS, "coordinates" );

    // if a coordinates tag already exists remove it
    if( ( nl != null ) && ( nl.getLength() > 0 ) )
    {
      element.removeChild( nl.item( 0 ) );
    }

    nl = element.getElementsByTagNameNS( CommonNamespaces.GMLNS, "coord" );

    // if a coord tag exists remove it
    if( ( nl != null ) && ( nl.getLength() > 0 ) )
    {
      element.removeChild( nl.item( 0 ) );
    }

    XMLTools.insertNodeInto( ( (GMLCoordinates_Impl)coordinates ).getAsElement(), element );

    Debug.debugMethodEnd();
  }

}

/*
 * Changes to this class. What the people haven been up to:
 * 
 * $Log$
 * Revision 1.4  2005/01/18 12:50:42  doemming
 * *** empty log message ***
 *
 * Revision 1.3  2004/10/07 14:09:13  doemming
 * *** empty log message ***
 *
 * Revision 1.1  2004/09/02 23:56:58  doemming
 * *** empty log message ***
 * Revision 1.3 2004/08/31 13:03:31 doemming ***
 * empty log message *** Revision 1.5 2004/04/07 06:43:48 poth no message
 * 
 * Revision 1.4 2004/03/02 07:38:14 poth no message
 * 
 * Revision 1.3 2003/11/26 17:05:35 poth no message
 * 
 * Revision 1.2 2003/04/23 15:44:40 poth no message
 * 
 * Revision 1.1.1.1 2002/09/25 16:01:04 poth no message
 * 
 * Revision 1.6 2002/08/19 15:59:29 ap no message
 * 
 * Revision 1.5 2002/08/05 16:11:02 ap no message
 * 
 * Revision 1.4 2002/08/01 08:56:56 ap no message
 */
