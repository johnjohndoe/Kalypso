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
import org.deegree.gml.GMLMultiPoint;
import org.deegree.gml.GMLPoint;
import org.deegree.ogcbasic.CommonNamespaces;
import org.deegree_impl.tools.Debug;
import org.w3c.dom.Document;
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
public class GMLMultiPoint_Impl extends GMLGeometryCollection_Impl implements GMLMultiPoint
{
  /**
   * Creates a new GMLMultiPoint_Impl object.
   * 
   * @param element
   */
  public GMLMultiPoint_Impl( Element element )
  {
    super( element );
  }

  /**
   * factory method to create a GMLMultiPoint. the collection that will be empty
   */
  public static GMLMultiPoint createGMLMultiPoint( Document doc )
  {
    Debug.debugMethodBegin( "", "createGMLMultiPoint" );

    Element elem = doc.createElementNS( CommonNamespaces.GMLNS, "gml:MultiPoint" );
    GMLMultiPoint ls = new GMLMultiPoint_Impl( elem );

    Debug.debugMethodEnd();
    return ls;
  }

  /**
   * returns all points contained within the collection
   */
  public GMLPoint[] getPoints()
  {
    Debug.debugMethodBegin( this, "getPoints" );

    GMLGeometry[] g = super.getGeometries();
    GMLPoint[] p = new GMLPoint[g.length];

    for( int i = 0; i < g.length; i++ )
    {
      p[i] = (GMLPoint)g[i];
    }

    Debug.debugMethodEnd();
    return p;
  }

  /**
   * @see org.deegree_impl.gml.GMLMultiPoint_Impl#getPoints()
   */
  public void addPoint( GMLPoint point )
  {
    Debug.debugMethodBegin( this, "addPoint(GMLPoint)" );
    super.addGeometry( point );
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
 * Revision 1.3 2004/08/31 13:03:31 doemming
 * *** empty log message *** Revision 1.7 2004/04/07 06:43:48 poth no message
 * 
 * Revision 1.6 2004/03/02 07:38:14 poth no message
 * 
 * Revision 1.5 2004/02/19 10:08:56 poth no message
 * 
 * Revision 1.4 2004/01/03 13:46:45 poth no message
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
 *  
 */
