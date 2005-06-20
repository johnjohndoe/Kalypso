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

import org.kalypsodeegree.gml.GMLBox;
import org.kalypsodeegree.gml.GMLCoord;
import org.kalypsodeegree.gml.GMLCoordinates;
import org.kalypsodeegree.model.geometry.GM_Position;
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
public class GMLBox_Impl extends GMLGeometry_Impl implements GMLBox
{
  /**
   * Creates a new GMLBox_Impl object.
   * 
   * @param element
   */
  public GMLBox_Impl( Element element )
  {
    super( element );
  }

  /**
   * factory method to create an initial box tag. the upper left corner is set to [0,0] and the lower right corner to
   * [1,1].
   */
  public static GMLBox createGMLBox( Document doc )
  {
    Debug.debugMethodBegin();

    Element elem = doc.createElementNS( CommonNamespaces.GMLNS, "gml:Box" );

    GMLBox box = new GMLBox_Impl( elem );

    GMLCoord min = GMLCoord_Impl.createGMLCoord( doc );
    min.setCoord( 0, 0 );

    GMLCoord max = GMLCoord_Impl.createGMLCoord( doc );
    max.setCoord( 1, 1 );

    box.setMin( min );
    box.setMax( max );

    Debug.debugMethodEnd();

    return box;
  }

  /**
   * returns the coord with the minx and miny coordinate The method assumes that there exists a two dimensional
   * coordinate system
   */
  public GMLCoord getMin()
  {
    Debug.debugMethodBegin( this, "getMin" );

    GMLCoord coord = null;

    if( element != null )
    {
      NodeList nl = element.getElementsByTagNameNS( CommonNamespaces.GMLNS, "coord" );

      if( ( nl != null ) && ( nl.getLength() > 0 ) )
      {
        GMLCoord coord1 = new GMLCoord_Impl( (Element)nl.item( 0 ) );
        GMLCoord coord2 = new GMLCoord_Impl( (Element)nl.item( 1 ) );

        double x_1 = coord1.getX();
        double y_1 = coord1.getY();
        double x_2 = coord2.getX();
        double y_2 = coord2.getY();

        double dum = 0;

        if( x_1 > x_2 )
        {
          dum = x_1;
          x_1 = x_2;
          x_2 = dum;
        }

        if( y_1 > y_2 )
        {
          dum = y_1;
          y_1 = y_2;
          y_2 = dum;
        }

        Element elem = element.getOwnerDocument().createElementNS( CommonNamespaces.GMLNS, "gml:coord" );

        coord = new GMLCoord_Impl( elem );
        coord.setCoord( x_1, y_1 );
      }
      else
      {
        GMLCoordinates[] coords = this.getCoordinates();
        GM_Position[] pos1 = GMLCoordinatesParser_Impl.coordinatesToPoints( coords[0] );

        double xmin = pos1[0].getX();
        double ymin = pos1[0].getY();

        if( pos1[0].getX() > pos1[1].getX() )
        {
          xmin = pos1[1].getX();
        }

        if( pos1[0].getY() > pos1[1].getY() )
        {
          ymin = pos1[1].getY();
        }

        Element elem = element.getOwnerDocument().createElementNS( CommonNamespaces.GMLNS, "gml:coord" );

        coord = new GMLCoord_Impl( elem );
        coord.setCoord( xmin, ymin );
      }
    }

    Debug.debugMethodEnd();
    return coord;
  }

  /**
   * @see org.kalypsodeegree_impl.gml.GMLBox_Impl#getMin()
   */
  public void setMin( GMLCoord min )
  {
    Debug.debugMethodBegin();

    NodeList nl = element.getElementsByTagNameNS( CommonNamespaces.GMLNS, "coord" );

    Element elem = element.getOwnerDocument().createElementNS( CommonNamespaces.GMLNS, "gml:coord" );

    XMLTools.copyNode( ( (GMLCoord_Impl)min ).getAsElement(), elem );

    // if already exists within the box replace the
    // first of them with the submitted coord
    if( ( nl != null ) && ( nl.getLength() > 0 ) )
    {
      element.replaceChild( elem, nl.item( 0 ) );
    } // else append the submitted coord
    else
    {
      nl = element.getElementsByTagNameNS( CommonNamespaces.GMLNS, "coordinates" );

      if( ( nl != null ) && ( nl.getLength() > 0 ) )
      {
        element.replaceChild( elem, nl.item( 0 ) );
      }
      else
      {
        element.appendChild( elem );
      }
    }

    Debug.debugMethodEnd();
  }

  /**
   * returns the coord with the maxx and maxy coordinate
   */
  public GMLCoord getMax()
  {
    Debug.debugMethodBegin( this, "getMax" );

    GMLCoord coord = null;

    if( element != null )
    {
      NodeList nl = element.getElementsByTagNameNS( CommonNamespaces.GMLNS, "coord" );

      if( ( nl != null ) && ( nl.getLength() > 0 ) )
      {
        GMLCoord coord1 = new GMLCoord_Impl( (Element)nl.item( 0 ) );
        GMLCoord coord2 = new GMLCoord_Impl( (Element)nl.item( 1 ) );

        double x_1 = coord1.getX();
        double y_1 = coord1.getY();
        double x_2 = coord2.getX();
        double y_2 = coord2.getY();

        double dum = 0;

        if( x_1 > x_2 )
        {
          dum = x_1;
          x_1 = x_2;
          x_2 = dum;
        }

        if( y_1 > y_2 )
        {
          dum = y_1;
          y_1 = y_2;
          y_2 = dum;
        }

        Element elem = element.getOwnerDocument().createElementNS( CommonNamespaces.GMLNS, "gml:coord" );

        coord = new GMLCoord_Impl( elem );
        coord.setCoord( x_2, y_2 );
      }
      else
      {
        GMLCoordinates[] coords = this.getCoordinates();
        GM_Position[] pos1 = GMLCoordinatesParser_Impl.coordinatesToPoints( coords[0] );

        double xmax = pos1[1].getX();
        double ymax = pos1[1].getY();

        if( pos1[0].getX() < pos1[1].getX() )
        {
          xmax = pos1[01].getX();
        }

        if( pos1[0].getY() < pos1[1].getY() )
        {
          ymax = pos1[1].getY();
        }

        Element elem = element.getOwnerDocument().createElementNS( CommonNamespaces.GMLNS, "gml:coord" );

        coord = new GMLCoord_Impl( elem );
        coord.setCoord( xmax, ymax );
      }
    }

    Debug.debugMethodEnd();
    return coord;
  }

  /**
   * @see org.kalypsodeegree_impl.gml.GMLBox_Impl#getMax()
   */
  public void setMax( GMLCoord max )
  {
    Debug.debugMethodBegin( this, "setMax" );

    NodeList nl = element.getElementsByTagName( "gml:coord" );

    Element elem = element.getOwnerDocument().createElementNS( CommonNamespaces.GMLNS, "gml:coord" );
    XMLTools.copyNode( ( (GMLCoord_Impl)max ).getAsElement(), elem );

    // if already exists within the box replace the
    // first of them with the submitted coord
    if( ( nl != null ) && ( nl.getLength() > 1 ) )
    {
      element.replaceChild( elem, nl.item( 1 ) );
    } // else append the submitted coord
    else
    {
      nl = element.getElementsByTagNameNS( CommonNamespaces.GMLNS, "coordinates" );

      if( ( nl != null ) && ( nl.getLength() > 0 ) )
      {
        element.replaceChild( elem, nl.item( 0 ) );
      }
      else
      {
        element.appendChild( elem );
      }
    }

    Debug.debugMethodEnd();
  }

  /**
   * returns the coordinates of the box
   */
  public GMLCoordinates[] getCoordinates()
  {
    Debug.debugMethodBegin( this, "getCoordinates" );

    GMLCoordinates[] coordinates = null;

    if( element != null )
    {
      NodeList nl = element.getElementsByTagNameNS( CommonNamespaces.GMLNS, "coordinates" );

      if( ( nl != null ) && ( nl.getLength() > 0 ) )
      {
        coordinates = new GMLCoordinates[2];
        coordinates[0] = new GMLCoordinates_Impl( (Element)nl.item( 0 ) );
        coordinates[1] = new GMLCoordinates_Impl( (Element)nl.item( 1 ) );
      }
    }

    Debug.debugMethodEnd();

    return coordinates;
  }

  /**
   * @see org.kalypsodeegree_impl.gml.GMLBox_Impl#getMin()
   */
  public void setCoordinates( GMLCoordinates coordinates )
  {
    Debug.debugMethodBegin( this, "setCoordinates" );

    NodeList nl = element.getElementsByTagNameNS( CommonNamespaces.GMLNS, "coordinates" );

    Element elem = element.getOwnerDocument().createElementNS( CommonNamespaces.GMLNS, "gml:coordinates" );

    XMLTools.copyNode( ( (GMLCoordinates_Impl)coordinates ).getAsElement(), elem );

    // if already exists within the box replace the
    // first of them with the submitted coord
    if( ( nl != null ) && ( nl.getLength() > 0 ) )
    {
      element.replaceChild( elem, nl.item( 0 ) );
    } // else append the submitted coord
    else
    {
      nl = element.getElementsByTagNameNS( CommonNamespaces.GMLNS, "coord" );

      if( ( nl != null ) && ( nl.getLength() > 0 ) )
      {
        element.removeChild( nl.item( 0 ) );
        element.removeChild( nl.item( 1 ) );
      }

      element.appendChild( elem );
    }

    Debug.debugMethodEnd();
  }

}

/*
 * Changes to this class. What the people haven been up to:
 * 
 * $Log$
 * Revision 1.6  2005/06/20 14:07:46  belger
 * Formatierung
 * Revision 1.5 2005/03/08 11:01:04 doemming *** empty log message ***
 * 
 * Revision 1.4 2005/01/18 12:50:42 doemming *** empty log message ***
 * 
 * Revision 1.3 2004/10/07 14:09:14 doemming *** empty log message ***
 * 
 * Revision 1.1 2004/09/02 23:56:58 doemming *** empty log message *** Revision 1.3 2004/08/31 13:02:26 doemming ***
 * empty log message *** Revision 1.10 2004/04/07 06:43:48 poth no message
 * 
 * Revision 1.9 2004/03/01 07:45:47 poth no message
 * 
 * Revision 1.8 2004/01/26 08:15:38 poth no message
 * 
 * Revision 1.7 2003/11/26 17:05:35 poth no message
 * 
 * Revision 1.6 2003/07/10 08:43:12 poth no message
 * 
 * Revision 1.5 2003/05/30 07:27:49 poth no message
 * 
 * Revision 1.4 2003/05/15 09:37:39 poth no message
 * 
 * Revision 1.3 2003/04/23 15:43:37 poth no message
 * 
 * Revision 1.2 2003/04/17 10:55:33 axel_schaefer the element is called "gml:Box" with a uppercase 'B'.
 * 
 * Revision 1.1.1.1 2002/09/25 16:01:03 poth no message
 * 
 * Revision 1.6 2002/08/19 15:59:29 ap no message
 * 
 * Revision 1.5 2002/08/05 16:11:02 ap no message
 * 
 *  
 */
