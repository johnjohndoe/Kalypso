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

import org.deegree.gml.GMLCoord;
import org.deegree.gml.GMLCoordinates;
import org.deegree.gml.GMLException;
import org.deegree.gml.GMLLinearRing;
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
public class GMLLinearRing_Impl extends GMLGeometry_Impl implements GMLLinearRing
{
  /**
   * Creates a new GMLLinearRing_Impl object.
   * 
   * @param element
   */
  public GMLLinearRing_Impl( Element element )
  {
    super( element );
  }

  /**
   * factory method to create a GMLLinearRing. the linear ring that will be
   * returned doesn't contain any point
   */
  public static GMLLinearRing createGMLLinearRing( Document doc )
  {
    Debug.debugMethodBegin( "", "createGMLLinearRing" );

    Element elem = doc.createElementNS( CommonNamespaces.GMLNS, "gml:LinearRing" );
    GMLLinearRing ls = new GMLLinearRing_Impl( elem );

    Debug.debugMethodEnd();
    return ls;
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
   * returns the coordinates (location) of the LineString as as array of
   * GMLCoord
   */
  public GMLCoord[] getCoord()
  {
    Debug.debugMethodBegin( "", "getCoord" );

    NodeList nl = element.getElementsByTagNameNS( CommonNamespaces.GMLNS, "coord" );

    GMLCoord[] coords = null;

    if( ( nl != null ) && ( nl.getLength() > 0 ) )
    {
      coords = new GMLCoord[nl.getLength()];

      for( int i = 0; i < nl.getLength(); i++ )
      {
        coords[i] = new GMLCoord_Impl( (Element)nl.item( i ) );
      }
    }

    Debug.debugMethodEnd();
    return coords;
  }

  /**
   * @see org.deegree_impl.gml.GMLLinearRing_Impl#getCoord() a exception should
   *      be thrown if the coords are not homogen or the coords are not building
   *      a closed ring.
   */
  public void setCoord( GMLCoord[] coord ) throws GMLException
  {
    Debug.debugMethodBegin( "", "setCoords" );

    NodeList nl = element.getElementsByTagNameNS( CommonNamespaces.GMLNS, "coord" );

    // if there already coors remove them
    if( ( nl != null ) && ( nl.getLength() > 0 ) )
    {
      for( int i = 0; i < nl.getLength(); i++ )
      {
        element.removeChild( nl.item( i ) );
      }
    }

    for( int i = 0; i < coord.length; i++ )
    {
      // insert the submitted coord
      XMLTools.insertNodeInto( ( (GMLCoord_Impl)coord[i] ).getAsElement(), element );
    }

    Debug.debugMethodEnd();
  }

  /**
   * returns the coordinate (location) of the point as GMLCoordinates
   */
  public GMLCoordinates getCoordinates()
  {
    Debug.debugMethodBegin( "", "getCoordinates" );

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
   * @see org.deegree_impl.gml.GMLLinearRing_Impl#getCoordinates() a exception
   *      should be thrown if the coords are not building a closed ring.
   */
  public void setCoordinates( GMLCoordinates coordinates ) throws GMLException
  {
    Debug.debugMethodBegin( this, "setCoordinates(GMLCoordinates)" );

    NodeList nl = element.getElementsByTagNameNS( CommonNamespaces.GMLNS, "coordinates" );

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
 * Revision 1.3  2004/10/07 14:09:14  doemming
 * *** empty log message ***
 *
 * Revision 1.1  2004/09/02 23:56:58  doemming
 * *** empty log message ***
 * Revision 1.3 2004/08/31 13:03:30 doemming
 * *** empty log message *** Revision 1.5 2004/04/07 06:43:48 poth no message
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
 *  
 */
