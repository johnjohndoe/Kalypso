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
package org.deegree_impl.services.wms.capabilities;

import org.deegree.model.geometry.*;
import org.deegree.services.wms.capabilities.*;
import org.deegree.xml.Marshallable;

import org.deegree_impl.model.geometry.*;


/**
 * Layers may have zero or more <BoundingBox> elements that are either stated
 * explicitly or inherited from a parent Layer. Each BoundingBox states the
 * bounding rectangle of the map data in a particular spatial reference system;
 * the attribute SRS indicates which SRS applies. If the data area is shaped
 * irregularly then the BoundingBox gives the minimum enclosing rectangle.The
 * attributes minx, miny, maxx, maxy indicate the edges of the bounding box in
 * units of the specified SRS. Optional resx and resy attributes indicate the
 * spatial resolution of the data in those same units.
 * <p></p>
 * A Layer may have multiple BoundingBox element, but each one shall state a
 * different SRS. A Layer inherits any BoundingBox values defined by its parents.
 * A BoundingBox inherited from the parent Layer for a particular SRS is replaced
 * by any declaration for the same SRS in the child Layer. A BoundingBox in the
 * child for a new SRS not already declared by the parent is added to the list
 * of bounding boxes for the child Layer. A single Layer element shall not
 * contain more than one BoundingBox for the same SRS.
 * <p>----------------------------------------------------------------------</p>
 *
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version 2002-03-01
 */
class LayerBoundingBox_Impl extends GM_Envelope_Impl implements LayerBoundingBox, Marshallable {
    private String sRS = null;
    private double resx = 0;
    private double resy = 0;

    /**
    * default constructor
    */
    LayerBoundingBox_Impl() {
    }

    /**
    * constructor initializing the class with the <LayerBoundingBox>
    */
    LayerBoundingBox_Impl( GM_Position min, GM_Position max, String srs, double resx, double resy ) {
        super( min, max );
        setSRS( srs );
        setResx( resx );
        setResy( resy );
    }

    /**
     * spatial resolution of the layers data in x-direction. If the resolution
     * isn't known <tt>-1</tt> will be returned.
     */
    public double getResx() {
        return resx;
    }

    /**
    * sets spatial resolution of the layers data in x-direction
    */
    public void setResx( double resx ) {
        this.resx = resx;
    }

    /**
     * spatial resolution of the layers data in x-direction. If the resolution
     * isn't known <tt>-1</tt> will be returned.
     */
    public double getResy() {
        return resy;
    }

    /**
    * sets spatial resolution of the layers data in x-direction
    */
    public void setResy( double resy ) {
        this.resy = resy;
    }

    /**
     * returns the name the spatial reference system of the bounding box
     */
    public String getSRS() {
        return sRS;
    }

    /**
     * sets the name of the spatial reference system of the bounding box
     */
    public void setSRS( String srs ) {
        sRS = srs;
    }

    /**
     *
     *
     * @return 
     */
    public String toString() {
        String ret = null;
        ret = "resx = " + resx + "\n";
        ret += ( "resy = " + resy + "\n" );
        ret += ( "sRS = " + sRS + "\n" );
        return ret;
    }

    /**
     * Returns an XML representation of this object.
     */
    public String exportAsXML() {
        StringBuffer sb = new StringBuffer();

        sb.append( "<BoundingBox" ).append( " SRS=\"" ).append( sRS ).append( "\"" )
          .append( " minx=\"" ).append( getMin().getX() ).append( "\"" ).append( " miny=\"" )
          .append( getMin().getY() ).append( "\"" ).append( " maxx=\"" ).append( getMax().getX() )
          .append( "\"" ).append( " maxy=\"" ).append( getMax().getY() ).append( "\"" );
        if ( resx > 0 ) {
            sb.append( " resx=\"" ).append( resx ).append( "\"" );
        }
        if ( resy > 0 ) {
            sb.append( " resy=\"" ).append( resy ).append( "\"" );
        }
        sb.append( "/>" );

        return sb.toString();
    }
}