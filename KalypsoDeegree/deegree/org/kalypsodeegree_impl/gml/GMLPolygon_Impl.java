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

import org.kalypsodeegree.gml.GMLLinearRing;
import org.kalypsodeegree.gml.GMLPolygon;
import org.kalypsodeegree.ogcbasic.CommonNamespaces;
import org.kalypsodeegree.xml.XMLTools;
import org.kalypsodeegree_impl.tools.Debug;
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
public class GMLPolygon_Impl extends GMLGeometry_Impl implements GMLPolygon
{
  /**
   * Creates a new GMLPolygon_Impl object.
   * 
   * @param element
   */
  public GMLPolygon_Impl( Element element )
  {
    super( element );
  }

  /**
   * factory method to create a GMLPolygon. the poly that will be returned doesn't contain any ring
   */
  public static GMLPolygon createGMLPolygon( Document doc )
  {
    Debug.debugMethodBegin( "GMLPolygon_Impl", "createGMLPolygon" );

    Element elem = doc.createElementNS( CommonNamespaces.GMLNS, "gml:Polygon" );
    GMLPolygon poly = new GMLPolygon_Impl( elem );

    Debug.debugMethodEnd();
    return poly;
  }

  public Element getAsElement()
  {
    return element;
  }

  /**
   * returns the exterior ring of the polygon
   */
  public GMLLinearRing getExteriorRing()
  {
    Debug.debugMethodBegin( this, "getExteriorRing" );

    NodeList nl = element.getElementsByTagNameNS( CommonNamespaces.GMLNS, "outerBoundaryIs" );

    GMLLinearRing exring = new GMLLinearRing_Impl( (Element)nl.item( 0 ) );

    Debug.debugMethodEnd();
    return exring;
  }

  /**
   * @see org.kalypsodeegree_impl.gml.GMLPolygon_Impl#getExteriorRing() If an exterior ring already exsists, it will be
   *      removed before setting the new one.
   */
  public void setExteriorRing( GMLLinearRing exteriorRing )
  {
    Debug.debugMethodBegin( this, "getExteriorRing" );

    NodeList nl = element.getElementsByTagNameNS( CommonNamespaces.GMLNS, "outerBoundaryIs" );

    // remove exterior ring if already exists
    if( ( nl != null ) && ( nl.getLength() > 0 ) )
    {
      element.removeChild( nl.item( 0 ) );
    }

    Element elem = element.getOwnerDocument().createElementNS( CommonNamespaces.GMLNS, "gml:outerBoundaryIs" );
    element.appendChild( elem );

    // insert the submitted ring
    XMLTools.insertNodeInto( ( (GMLLinearRing_Impl)exteriorRing ).getAsElement(), elem );

    Debug.debugMethodEnd();
  }

  /**
   * returns the interior rings of the polygon. if no interior rings exists null should be returned
   */
  public GMLLinearRing[] getInteriorRings()
  {
    Debug.debugMethodBegin( "", "getInteriorRings" );

    NodeList nl = element.getElementsByTagNameNS( CommonNamespaces.GMLNS, "innerBoundaryIs" );

    GMLLinearRing[] lrs = null;

    if( ( nl != null ) && ( nl.getLength() > 0 ) )
    {
      // get memory for every inner ring of the polygon
      lrs = new GMLLinearRing[nl.getLength()];

      // loop over every inner boundary
      for( int i = 0; i < nl.getLength(); i++ )
      {
        // get the ring that builds the boundary
        NodeList nl_ = ( (Element)nl.item( i ) ).getElementsByTagNameNS( CommonNamespaces.GMLNS, "LinearRing" );
        lrs[i] = new GMLLinearRing_Impl( (Element)nl_.item( 0 ) );
      }
    }

    Debug.debugMethodEnd();
    return lrs;
  }

  /**
   * adds a interior ring to the polygon. if the submitted ring isn't not completly contained within the exterior ring
   * an exception should be thrown.
   */
  public void addInteriorRing( GMLLinearRing interiorRing )
  {
    Debug.debugMethodBegin( this, "addInteriorRing" );

    //TODO
    // check if submitted interior ring is located completly
    // within the exterior ring of the polygon
    Element elem = element.getOwnerDocument().createElementNS( CommonNamespaces.GMLNS, "gml:innerBoundaryIs" );
    element.appendChild( elem );

    // insert the submitted ring
    XMLTools.insertNodeInto( ( (GMLLinearRing_Impl)interiorRing ).getAsElement(), elem );

    Debug.debugMethodEnd();
  }

}

/*
 * Changes to this class. What the people haven been up to:
 * 
 * $Log$
 * Revision 1.7  2005/06/20 14:07:46  belger
 * Formatierung
 * Revision 1.6 2005/03/15 10:49:34 belger *** empty log message ***
 * 
 * Revision 1.5 2005/03/08 11:01:04 doemming *** empty log message ***
 * 
 * Revision 1.4 2005/01/18 12:50:42 doemming *** empty log message ***
 * 
 * Revision 1.3 2004/10/07 14:09:13 doemming *** empty log message ***
 * 
 * Revision 1.1 2004/09/02 23:56:58 doemming *** empty log message *** Revision 1.3 2004/08/31 13:03:30 doemming ***
 * empty log message *** Revision 1.6 2004/07/09 07:16:56 poth no message
 * 
 * Revision 1.5 2004/04/07 06:43:48 poth no message
 * 
 * Revision 1.4 2004/03/02 07:38:14 poth no message
 * 
 * Revision 1.3 2003/11/26 17:05:35 poth no message
 * 
 * Revision 1.2 2003/04/23 15:44:40 poth no message
 * 
 * Revision 1.1.1.1 2002/09/25 16:01:05 poth no message
 * 
 * Revision 1.6 2002/08/19 15:59:29 ap no message
 * 
 * Revision 1.5 2002/08/05 16:11:02 ap no message
 * 
 * Revision 1.4 2002/08/01 08:56:56 ap no message
 */
