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

import java.util.ArrayList;

import org.kalypsodeegree.gml.GMLGeometry;
import org.kalypsodeegree.gml.GMLGeometryCollection;
import org.kalypsodeegree.gml.GMLLineString;
import org.kalypsodeegree.gml.GMLMultiLineString;
import org.kalypsodeegree.gml.GMLMultiPoint;
import org.kalypsodeegree.gml.GMLMultiPolygon;
import org.kalypsodeegree.gml.GMLPoint;
import org.kalypsodeegree.gml.GMLPolygon;
import org.kalypsodeegree.ogcbasic.CommonNamespaces;
import org.kalypsodeegree.xml.XMLTools;
import org.kalypsodeegree_impl.tools.Debug;
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
class GMLGeometryCollection_Impl extends GMLGeometry_Impl implements GMLGeometryCollection
{
  /**
   * Creates a new GMLGeometryCollection_Impl object.
   * 
   * @param element
   */
  public GMLGeometryCollection_Impl( org.w3c.dom.Element element )
  {
    super( element );
  }

  /**
   * returns the geometries contained within the collection
   */
  public GMLGeometry[] getGeometries()
  {
    Debug.debugMethodBegin( "", "getGeometries" );

    ArrayList list = new ArrayList();
    NodeList nl = element.getChildNodes();

    if( ( nl != null ) && ( nl.getLength() > 0 ) )
    {
      for( int i = 0; i < nl.getLength(); i++ )
      {
        if( nl.item( i ) instanceof Element )
        {
          Element elem = XMLTools.getFirstElement( nl.item( i ) );
          if( elem == null )
          {}
          else if( XMLTools.toLocalName( elem.getNodeName() ).equals( "Point" ) )
          {
            list.add( new GMLPoint_Impl( elem ) );
          }
          else if( XMLTools.toLocalName( elem.getNodeName() ).equals( "LineString" ) )
          {
            list.add( new GMLLineString_Impl( elem ) );
          }
          else if( XMLTools.toLocalName( elem.getNodeName() ).equals( "Polygon" ) )
          {
            list.add( new GMLPolygon_Impl( elem ) );
          }
          else if( XMLTools.toLocalName( nl.item( i ).getNodeName() ).equals( "MultiPoint" ) )
          {
            list.add( new GMLMultiPoint_Impl( elem ) );
          }
          else if( XMLTools.toLocalName( nl.item( i ).getNodeName() ).equals( "MultiLineString" ) )
          {
            list.add( new GMLMultiLineString_Impl( elem ) );
          }
          else if( XMLTools.toLocalName( nl.item( i ).getNodeName() ).equals( "MultiPolygon" ) )
          {
            list.add( new GMLMultiPolygon_Impl( elem ) );
          }
        }
      }
    }

    Debug.debugMethodEnd();
    return (GMLGeometry[])list.toArray( new GMLGeometry[list.size()] );
  }

  /**
   * @see org.kalypsodeegree_impl.gml.GMLGeometryCollection_Impl#getGeometries()
   */
  public void addGeometry( GMLGeometry geometry )
  {
    Debug.debugMethodBegin( this, "addGeometry" );

    Element elem = null;

    if( geometry instanceof GMLPoint )
    {
      elem = ( (GMLPoint_Impl)geometry ).getAsElement();
    }
    else if( geometry instanceof GMLLineString )
    {
      elem = ( (GMLLineString_Impl)geometry ).getAsElement();
    }
    else if( geometry instanceof GMLPolygon )
    {
      elem = ( (GMLPolygon_Impl)geometry ).getAsElement();
    }
    else if( geometry instanceof GMLMultiPoint )
    {
      elem = ( (GMLMultiPoint_Impl)geometry ).getAsElement();
    }
    else if( geometry instanceof GMLMultiLineString )
    {
      elem = ( (GMLMultiLineString_Impl)geometry ).getAsElement();
    }
    else if( geometry instanceof GMLMultiPolygon )
    {
      elem = ( (GMLMultiPolygon_Impl)geometry ).getAsElement();
    }

    Element elem_ = element.getOwnerDocument().createElementNS( CommonNamespaces.GMLNS, "gml:geometryMember" );
    XMLTools.insertNodeInto( elem, elem_ );
    element.appendChild( elem_ );

    Debug.debugMethodEnd();
  }

}

/*
 * Changes to this class. What the people haven been up to:
 * 
 * $Log$
 * Revision 1.6  2005/06/20 14:07:46  belger
 * Formatierung
 * Revision 1.5 2005/03/08 11:01:03 doemming *** empty log message ***
 * 
 * Revision 1.4 2005/01/18 12:50:42 doemming *** empty log message ***
 * 
 * Revision 1.3 2004/10/07 14:09:14 doemming *** empty log message ***
 * 
 * Revision 1.1 2004/09/02 23:56:58 doemming *** empty log message *** Revision 1.3 2004/08/31 13:03:31 doemming ***
 * empty log message *** Revision 1.9 2004/04/07 06:43:48 poth no message
 * 
 * Revision 1.8 2004/03/02 07:38:14 poth no message
 * 
 * Revision 1.7 2003/11/26 17:05:35 poth no message
 * 
 * Revision 1.6 2003/07/14 09:12:45 poth no message
 * 
 * Revision 1.5 2003/05/15 09:37:40 poth no message
 * 
 * Revision 1.4 2003/05/07 12:47:23 axel_schaefer no message
 * 
 * Revision 1.3 2003/05/05 15:48:59 poth no message
 * 
 * Revision 1.1.1.1 2002/09/25 16:01:01 poth no message
 * 
 * Revision 1.6 2002/08/19 15:59:29 ap no message
 * 
 * Revision 1.5 2002/08/05 16:11:02 ap no message
 * 
 * Revision 1.4 2002/08/01 08:56:56 ap no message
 *  
 */
