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
import org.deegree.gml.GMLLineString;
import org.deegree.gml.GMLMultiLineString;
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
public class GMLMultiLineString_Impl extends GMLGeometryCollection_Impl implements
    GMLMultiLineString
{
  /**
   * Creates a new GMLMultiLineString_Impl object.
   * 
   * @param element
   */
  public GMLMultiLineString_Impl( Element element )
  {
    super( element );
  }

  /**
   * factory method to create a GMLMultiLineString. the collection that will be
   * empty
   */
  public static GMLMultiLineString createGMLMultiLineString( Document doc )
  {
    Debug.debugMethodBegin( "", "createGMLMultiLineString" );

    Element elem = doc.createElementNS( GMLGeometricMapping.GMLNS, "gml:MultiLineString" );
    GMLMultiLineString ls = new GMLMultiLineString_Impl( elem );

    Debug.debugMethodEnd();
    return ls;
  }

  /**
   * returns all linestrings contained within the collection
   */
  public GMLLineString[] getLineStrings()
  {
    Debug.debugMethodBegin( this, "getLineStrings" );

    GMLGeometry[] g = super.getGeometries();
    GMLLineString[] ls = new GMLLineString[g.length];

    for( int i = 0; i < g.length; i++ )
    {
      ls[i] = (GMLLineString)g[i];
    }

    Debug.debugMethodEnd();
    return ls;
  }

  /**
   * @see org.deegree_impl.gml.GMLMultiLineString_Impl#getLineStrings()
   */
  public void addLineString( GMLLineString lineString )
  {
    Debug.debugMethodBegin( this, "addLineString" );
    super.addGeometry( lineString );
    Debug.debugMethodEnd();
  }

}

/*
 * Changes to this class. What the people haven been up to:
 * 
 * $Log$
 * Revision 1.2  2004/08/30 00:36:58  doemming
 * *** empty log message ***
 * Revision 1.1.1.1 2004/05/11 16:43:24
 * doemming backup of local modified deegree sources
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
 * Revision 1.1.1.1 2002/09/25 16:01:04 poth no message
 * 
 * Revision 1.6 2002/08/19 15:59:29 ap no message
 * 
 * Revision 1.5 2002/08/05 16:11:02 ap no message
 * 
 * Revision 1.4 2002/08/01 08:56:56 ap no message
 * 
 *  
 */
