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
import org.deegree.gml.GMLException;
import org.deegree.ogcbasic.CommonNamespaces;
import org.deegree.xml.DOMPrinter;
import org.deegree_impl.tools.Debug;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;

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
public class GMLCoord_Impl implements GMLCoord
{
  private Element element = null;

  /**
   * Creates a new GMLCoord_Impl object.
   * 
   * @param element
   */
  public GMLCoord_Impl( Element element )
  {
    this.element = element;
  }

  /**
   * factory method to create an empty GMLCoord. The coord contains a x-tag to
   * be a valid gml:coord. the value is set to -9E99.
   */
  public static GMLCoord createGMLCoord( Document doc )
  {
    Debug.debugMethodBegin( "", "createGMLCoord" );

    Element elem = doc.createElementNS( CommonNamespaces.GMLNS, "gml:coord" );
    GMLCoord coord = new GMLCoord_Impl( elem );

    coord.setCoord( -9E99 );

    Debug.debugMethodEnd();

    return coord;
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
   * returns the x-value of the coordinate
   */
  public double getX()
  {
    Debug.debugMethodBegin( this, "getX" );

    NodeList nl = element.getElementsByTagNameNS( CommonNamespaces.GMLNS, "X" );

    String x = nl.item( 0 ).getFirstChild().getNodeValue();

    double d = Double.parseDouble( x );

    Debug.debugMethodEnd();
    return d;
  }

  /**
   * returns the y-value of the coordinate. if no y-value is defined -9E99 will
   * be returned
   */
  public double getY()
  {
    Debug.debugMethodBegin( this, "getY" );

    double d = -9E99;
    NodeList nl = element.getElementsByTagName( "gml:Y" );

    if( ( nl != null ) && ( nl.getLength() > 0 ) )
    {
      String y = nl.item( 0 ).getFirstChild().getNodeValue();
      d = Double.parseDouble( y );
    }

    Debug.debugMethodEnd();
    return d;
  }

  /**
   * returns the z-value of the coordinate. if no z-value is defined -9E99 will
   * be returned
   */
  public double getZ()
  {
    Debug.debugMethodBegin( this, "getZ" );

    double d = -9E99;
    NodeList nl = element.getElementsByTagNameNS( CommonNamespaces.GMLNS, "Z" );

    if( ( nl != null ) && ( nl.getLength() > 0 ) )
    {
      String z = nl.item( 0 ).getFirstChild().getNodeValue();
      d = Double.parseDouble( z );
    }

    Debug.debugMethodEnd();
    return d;
  }

  /**
   * returns the coordinate as double array
   */
  public double[] getCoord()
  {
    Debug.debugMethodBegin( this, "getCoord()" );

    int dim = getDimension();

    double[] d = new double[dim];
    d[0] = getX();

    if( dim > 1 )
    {
      d[1] = getY();
    }

    if( dim > 2 )
    {
      d[2] = getZ();
    }

    Debug.debugMethodEnd();
    return d;
  }

  /**
   * sets the coordinate value(s) using a double array. the min length of the
   * equals 1 the max length 3
   */
  public void setCoord( double[] coord ) throws GMLException
  {
    Debug.debugMethodBegin( this, "setCoord(double[])" );

    if( ( coord == null ) || ( coord.length > 3 ) || ( coord.length == 0 ) )
    {
      throw new GMLException( "wrong dimension of coord array!" );
    }

    if( coord.length == 1 )
    {
      setCoord( coord[0] );
    }
    else if( coord.length == 2 )
    {
      setCoord( coord[0], coord[1] );
    }
    else if( coord.length == 3 )
    {
      setCoord( coord[0], coord[1], coord[3] );
    }

    Debug.debugMethodEnd();
  }

  /**
   * set the coordinate as one-dimensional point
   */
  public void setCoord( double x )
  {
    Debug.debugMethodBegin( this, "setCoord(double)" );

    NodeList nl = element.getElementsByTagNameNS( CommonNamespaces.GMLNS, "X" );

    // if a x-tag already exsists overwrite its value
    if( ( nl != null ) && ( nl.getLength() > 0 ) )
    {
      nl.item( 0 ).getFirstChild().setNodeValue( "" + x );
    } // else create a new x-tag
    else
    {
      Element elem = element.getOwnerDocument().createElementNS( CommonNamespaces.GMLNS, "gml:X" );
      element.appendChild( elem );

      Text text = element.getOwnerDocument().createTextNode( "" + x );
      elem.appendChild( text );
    }

    Debug.debugMethodEnd();
  }

  /**
   * set the coordinate as two-dimensional point
   */
  public void setCoord( double x, double y )
  {
    Debug.debugMethodBegin( this, "setCoord(double, double)" );

    setCoord( x );

    NodeList nl = element.getElementsByTagNameNS( CommonNamespaces.GMLNS, "Y" );

    // if a y-tag already exsists overwrite its value
    if( ( nl != null ) && ( nl.getLength() > 0 ) )
    {
      nl.item( 0 ).getFirstChild().setNodeValue( "" + y );
    } // else create a new y-tag
    else
    {
      Element elem = element.getOwnerDocument().createElementNS( CommonNamespaces.GMLNS, "gml:Y" );
      element.appendChild( elem );

      Text text = element.getOwnerDocument().createTextNode( "" + y );
      elem.appendChild( text );
    }

    Debug.debugMethodEnd();
  }

  /**
   * set the coordinate as three-dimensional point
   */
  public void setCoord( double x, double y, double z )
  {
    Debug.debugMethodBegin( this, "setCoord(double, double, double)" );

    setCoord( x, y );

    NodeList nl = element.getElementsByTagNameNS( CommonNamespaces.GMLNS, "Z" );

    // if a z-tag already exsists overwrite its value
    if( ( nl != null ) && ( nl.getLength() > 0 ) )
    {
      nl.item( 0 ).getFirstChild().setNodeValue( "" + z );
    } // else create a new z-tag
    else
    {
      Element elem = element.getOwnerDocument().createElementNS( CommonNamespaces.GMLNS, "gml:Z" );
      element.appendChild( elem );

      Text text = element.getOwnerDocument().createTextNode( "" + z );
      elem.appendChild( text );
    }

    Debug.debugMethodEnd();
  }

  /**
   * retuns the dimension of the coordinate
   */
  public int getDimension()
  {
    Debug.debugMethodBegin( this, "getDimension" );

    int dim = 1;

    // if a Y (and a Z) tag exists increase the dimension of
    // the coord
    NodeList nl = element.getElementsByTagNameNS( CommonNamespaces.GMLNS, "Y" );

    if( ( nl != null ) && ( nl.getLength() > 0 ) )
    {
      dim++;
    }

    nl = element.getElementsByTagNameNS( CommonNamespaces.GMLNS, "Z" );

    if( ( nl != null ) && ( nl.getLength() > 0 ) )
    {
      dim++;
    }

    Debug.debugMethodEnd();
    return dim;
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
 * Revision 1.3 2004/08/31 13:03:30 doemming ***
 * empty log message *** Revision 1.4 2004/04/07 06:43:48 poth no message
 * 
 * Revision 1.3 2003/05/15 09:37:40 poth no message
 * 
 * Revision 1.2 2003/04/23 15:44:39 poth no message
 * 
 * Revision 1.1.1.1 2002/09/25 16:01:03 poth no message
 * 
 * Revision 1.6 2002/08/19 15:59:29 ap no message
 * 
 * Revision 1.5 2002/08/05 16:11:02 ap no message
 * 
 * Revision 1.4 2002/08/01 08:56:56 ap no message
 *  
 */
