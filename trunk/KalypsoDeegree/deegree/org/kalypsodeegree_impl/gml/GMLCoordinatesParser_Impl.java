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

import java.util.StringTokenizer;

import org.kalypsodeegree.gml.GMLCoordinates;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.Debug;

/**
 * This class parses the &lt;coordinates&gt;
 * <p>
 * ----------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:axel.schaefer@operamail.com">Axel Schaefer </a>
 * @version 26.11.2001
 *          <p>
 */
public class GMLCoordinatesParser_Impl
{
  public static GM_Position[] coordinatesToPoints( GMLCoordinates coordinates )
  {
    Debug.debugMethodBegin( "", "coordinatesToPoints(GMLCoordinates)" );

    GM_Position[] points = null;
    // first tokenizer, tokens the tuples
    StringTokenizer tuple = new StringTokenizer( coordinates.getCoordinates(), new Character(
        coordinates.getTupleSeperator() ).toString() );
    String cs = "" + coordinates.getCoordinateSeperator();
    points = new GM_Position[tuple.countTokens()];

    int i = 0;

    while( tuple.hasMoreTokens() )
    {
      String s = tuple.nextToken();
      // second tokenizer, tokens the coordinates
      StringTokenizer coort = new StringTokenizer( s, cs );
      double[] p = new double[coort.countTokens()];

      for( int k = 0; k < p.length; k++ )
      {
        p[k] = Double.parseDouble( coort.nextToken() );
      }

      points[i] = GeometryFactory.createGM_Position( p );
      ++i;
    }

    Debug.debugMethodEnd();
    return points;
  }

}

/*
 * Changes to this class. What the people haven been up to:
 * 
 * $Log$
 * Revision 1.6  2005/04/15 19:46:42  belger
 * *** empty log message ***
 *
 * Revision 1.5  2005/03/08 11:01:03  doemming
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
 * Revision 1.3 2004/08/31 13:03:30
 * doemming *** empty log message *** Revision 1.10 2004/03/31 07:12:06 poth no
 * message
 * 
 * Revision 1.9 2004/03/03 17:02:09 poth no message
 * 
 * Revision 1.8 2004/02/23 07:47:48 poth no message
 * 
 * Revision 1.7 2004/02/19 10:08:56 poth no message
 * 
 * Revision 1.6 2004/02/11 08:06:05 poth no message
 * 
 * Revision 1.5 2004/01/03 13:46:45 poth no message
 * 
 * Revision 1.4 2003/04/23 15:44:39 poth no message
 * 
 * Revision 1.3 2003/04/17 13:54:46 poth no message
 * 
 * Revision 1.2 2002/11/18 17:15:35 poth no message
 * 
 * Revision 1.1.1.1 2002/09/25 16:01:03 poth no message
 * 
 * Revision 1.3 2002/08/19 15:59:29 ap no message
 * 
 * Revision 1.2 2002/04/23 07:56:57 ap no message
 * 
 * Revision 1.1 2002/04/04 16:22:40 ap no message
 * 
 * Revision 1.5 2002/03/05 16:52:39 ap no message
 * 
 * Revision 1.4 2002/02/21 12:10:11 ap no message
 * 
 * Revision 1.3 2001/12/05 15:05:04 axel as diverse corrections after testing
 * 
 * Revision 1.2 2001/12/03 15:00:05 axel point and linestring tested
 * 
 * Revision 1.1 2001/11/28 15:10:24 axel as: completed 'n' tested
 * 
 *  
 */
