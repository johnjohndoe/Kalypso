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

import java.util.StringTokenizer;

import org.deegree.gml.GMLCoordinates;
import org.deegree.model.geometry.GM_Position;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.deegree_impl.tools.Debug;


/**
 * This class parses the &lt;coordinates&gt;
 * <p>----------------------------------------------------------</p>
 *
 * @author <a href="mailto:axel.schaefer@operamail.com">Axel Schaefer</a>
 * @version 26.11.2001
 * <p>
 */
public class GMLCoordinatesParser_Impl {
    /**
     *
     *
     * @param coordinates 
     *
     * @return 
     */
    public static GM_Position[] coordinatesToPoints( GMLCoordinates coordinates ) {
        Debug.debugMethodBegin( "", "coordinatesToPoints(GMLCoordinates)" );

        GM_Position[] points = null;
        // first tokenizer, tokens the tuples
        StringTokenizer tuple = new StringTokenizer( coordinates.getCoordinates(), 
                                                     new Character( coordinates.getTupleSeperator() ).toString() );
        String cs = "" + coordinates.getCoordinateSeperator();
        points = new GM_Position[tuple.countTokens()];

        int i = 0;
        
        while ( tuple.hasMoreTokens() ) {
            String s = tuple.nextToken();
            // second tokenizer, tokens the coordinates
            StringTokenizer coort = new StringTokenizer( s, cs );
            double[] p = new double[coort.countTokens()];

            for ( int k = 0; k < p.length; k++ ) {
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
 * Revision 1.1  2004/05/11 16:43:24  doemming
 * Initial revision
 *
 * Revision 1.9  2004/03/03 17:02:09  poth
 * no message
 *
 * Revision 1.8  2004/02/23 07:47:48  poth
 * no message
 *
 * Revision 1.7  2004/02/19 10:08:56  poth
 * no message
 *
 * Revision 1.6  2004/02/11 08:06:05  poth
 * no message
 *
 * Revision 1.5  2004/01/03 13:46:45  poth
 * no message
 *
 * Revision 1.4  2003/04/23 15:44:39  poth
 * no message
 *
 * Revision 1.3  2003/04/17 13:54:46  poth
 * no message
 *
 * Revision 1.2  2002/11/18 17:15:35  poth
 * no message
 *
 * Revision 1.1.1.1  2002/09/25 16:01:03  poth
 * no message
 *
 * Revision 1.3  2002/08/19 15:59:29  ap
 * no message
 *
 * Revision 1.2  2002/04/23 07:56:57  ap
 * no message
 *
 * Revision 1.1  2002/04/04 16:22:40  ap
 * no message
 *
 * Revision 1.5  2002/03/05 16:52:39  ap
 * no message
 *
 * Revision 1.4  2002/02/21 12:10:11  ap
 * no message
 *
 * Revision 1.3  2001/12/05 15:05:04  axel
 * as diverse corrections after testing
 *
 * Revision 1.2  2001/12/03 15:00:05  axel
 * point and linestring tested
 *
 * Revision 1.1  2001/11/28 15:10:24  axel
 * as: completed 'n' tested
 *
 *
 */
