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
package org.deegree_impl.model.geometry;

import java.awt.geom.Rectangle2D;
import java.io.Serializable;

import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Position;


/**
 * a boundingbox as child of a GM_Polygon isn't part
 * of the iso19107 spec but it simplifies the geometry handling
 * within jago
 *
 * <P>------------------------------------------------------------</P>
 * @author Andreas Poth href="mailto:poth@lat-lon.de"
 * @author Markus Bedel href="mailto:bedel@giub.uni-bonn.de"
 * @version $Id$
 *
 */
public class GM_Envelope_Impl implements GM_Envelope, Serializable {
    /** Use serialVersionUID for interoperability. */
    private final static long serialVersionUID = 1081219767894344990L;
    private GM_Position max = null;
    private GM_Position min = null;

    /**
     * Creates a new GM_Envelope_Impl object.
     */
    public GM_Envelope_Impl() {
        this.min = new GM_Position_Impl();
        this.max = new GM_Position_Impl();
    }

    /**
     * Creates a new GM_Envelope_Impl object.
     *
     * @param min 
     * @param max 
     */
    public GM_Envelope_Impl( GM_Position min, GM_Position max ) {
        this.min = min;
        this.max = max;
    }

    /**
     *
     *
     * @return 
     */
    public Object clone() {
        return new GM_Envelope_Impl( (GM_Position)( (GM_Position_Impl)min ).clone(), 
                                     (GM_Position)( (GM_Position_Impl)max ).clone() );
    }

    /**
     * returns the minimum coordinates of bounding box
     */
    public GM_Position getMin() {
        return min;
    }

    /**
     * returns the maximum coordinates of bounding box
     */
    public GM_Position getMax() {
        return max;
    }

    /**
     * returns the width of bounding box
     */
    public double getWidth() {
        return this.getMax().getX() - this.getMin().getX();
    }

    /**
     * returns the height of bounding box
     */
    public double getHeight() {
        return this.getMax().getY() - this.getMin().getY();
    }

    /**
     * returns true if the bounding box conatins the specified GM_Point
     */
    public boolean contains( GM_Position point ) {
        if ( ( point.getX() >= min.getX() ) && ( point.getX() <= max.getX() ) && 
             ( point.getY() >= min.getY() ) && ( point.getY() <= max.getY() ) ) {
            return true;
        }

        return false;
    }

    /**
     * returns true if this envelope and the submitted intersects
     */
    public boolean intersects( GM_Envelope bb ) {
        // coordinates of this GM_Envelope's BBOX
        double west1 = min.getX();
        double south1 = min.getY();
        double east1 = max.getX();
        double north1 = max.getY();

        // coordinates of the other GM_Envelope's BBOX        
        double west2 = bb.getMin().getX();
        double south2 = bb.getMin().getY();
        double east2 = bb.getMax().getX();
        double north2 = bb.getMax().getY();

        // special cases: one box lays completly inside the other one
        if ( ( west1 <= west2 ) && ( south1 <= south2 ) && ( east1 >= east2 ) && 
                 ( north1 >= north2 ) ) {
            return true;
        }

        if ( ( west1 >= west2 ) && ( south1 >= south2 ) && ( east1 <= east2 ) && 
                 ( north1 <= north2 ) ) {
            return true;
        }

        // in any other case of intersection, at least one line of the BBOX has
        // to cross a line of the other BBOX
        // check western boundary of box 1
        // "touching" boxes must not intersect
        if ( ( west1 >= west2 ) && ( west1 < east2 ) ) {
            if ( ( south1 <= south2 ) && ( north1 > south2 ) ) {
                return true;
            }

            if ( ( south1 < north2 ) && ( north1 >= north2 ) ) {
                return true;
            }
        }

        // check eastern boundary of box 1
        // "touching" boxes must not intersect
        if ( ( east1 > west2 ) && ( east1 <= east2 ) ) {
            if ( ( south1 <= south2 ) && ( north1 > south2 ) ) {
                return true;
            }

            if ( ( south1 < north2 ) && ( north1 >= north2 ) ) {
                return true;
            }
        }

        // check southern boundary of box 1
        // "touching" boxes must not intersect
        if ( ( south1 >= south2 ) && ( south1 < north2 ) ) {
            if ( ( west1 <= west2 ) && ( east1 > west2 ) ) {
                return true;
            }

            if ( ( west1 < east2 ) && ( east1 >= east2 ) ) {
                return true;
            }
        }

        // check northern boundary of box 1
        // "touching" boxes must not intersect
        if ( ( north1 > south2 ) && ( north1 <= north2 ) ) {
            if ( ( west1 <= west2 ) && ( east1 > west2 ) ) {
                return true;
            }

            if ( ( west1 < east2 ) && ( east1 >= east2 ) ) {
                return true;
            }
        }

        return false;
    }

    /**
     * returns true if all points of the submitted
     * bounding box are within this bounding box
     */
    public boolean contains( GM_Envelope bb ) {
        GM_Position p1 = new GM_Position_Impl( bb.getMin().getX(), bb.getMin().getY() );
        GM_Position p2 = new GM_Position_Impl( bb.getMin().getX(), bb.getMax().getY() );
        GM_Position p3 = new GM_Position_Impl( bb.getMax().getX(), bb.getMin().getY() );
        GM_Position p4 = new GM_Position_Impl( bb.getMax().getX(), bb.getMax().getY() );

        boolean ins = ( this.contains( p1 ) && this.contains( p2 ) && this.contains( p3 ) && 
                      this.contains( p4 ) );
        return ins;
    }

    /**
     * returns a new GM_Envelope object representing the intersection of this
     * GM_Envelope with the specified GM_Envelope. * Note: If there is no
     * intersection at all GM_Envelope will be null.
     * @param bb the GM_Envelope to be intersected with this GM_Envelope
     * @return the largest GM_Envelope contained in both the specified GM_Envelope
     * and in this GM_Envelope.
     */
    public GM_Envelope createIntersection( GM_Envelope bb ) {
        Rectangle2D rect = new Rectangle2D.Double( bb.getMin().getX(), bb.getMin().getY(), 
                                                   bb.getWidth(), bb.getHeight() );
        Rectangle2D rect2 = new Rectangle2D.Double( this.getMin().getX(), this.getMin().getY(), 
                                                    this.getWidth(), this.getHeight() );

        if ( rect2.intersects( bb.getMin().getX(), bb.getMin().getY(), bb.getWidth(), 
                               bb.getHeight() ) ) {
            rect = rect.createIntersection( rect2 );
        } else {
            rect = null;
        }

        if ( rect == null ) {
            return null;
        }

        double xmin = rect.getX();
        double ymin = rect.getY();
        double xmax = rect.getX() + rect.getWidth();
        double ymax = rect.getY() + rect.getHeight();

        GM_Position p1 = new GM_Position_Impl( xmin, ymin );
        GM_Position p2 = new GM_Position_Impl( xmax, ymax );

        return new GM_Envelope_Impl( p1, p2 );
    }

    /**
     * checks if this point is completly equal to the submitted geometry
     */
    public boolean equals( Object other ) {
        if ( ( other == null ) || !( other instanceof GM_Envelope_Impl ) ) {
            return false;
        }

        return ( min.equals( ( (GM_Envelope)other ).getMin() ) && 
               max.equals( ( (GM_Envelope)other ).getMax() ) );
    }

    /**
     *
     *
     * @param b 
     *
     * @return 
     */
    public GM_Envelope getBuffer( double b ) {
        GM_Position bmin = new GM_Position_Impl( new double[] { min.getX() - b, min.getY() - b } );
        GM_Position bmax = new GM_Position_Impl( new double[] { max.getX() + b, max.getY() + b } );
        return GeometryFactory.createGM_Envelope( bmin, bmax );
    }
    
    /**
     * @see org.deegree.model.geometry.GM_Envelope#merge(org.deegree.model.geometry.GM_Envelope)
     */
    public GM_Envelope merge(GM_Envelope envelope) {
    	double minx = min.getX();
    	double miny = min.getY();
    	double maxx = max.getX();
    	double maxy = max.getY();
    	if ( envelope.getMin().getX() < minx ) {
    		minx = envelope.getMin().getX(); 
    	}
    	if ( envelope.getMin().getY() < miny ) {
    		miny = envelope.getMin().getY(); 
    	}
    	if ( envelope.getMax().getX() > maxx ) {
    		maxx = envelope.getMax().getX(); 
    	}
    	if ( envelope.getMax().getY() > maxy ) {
    		maxy = envelope.getMax().getY(); 
    	}
    	return GeometryFactory.createGM_Envelope(minx, miny, maxx, maxy);
    }

    /**
     *
     *
     * @return 
     */
    public String toString() {
        String ret = null;
        ret = "min = " + min;
        ret += ( " max = " + max + "\n" );
        return ret;
    }
	

}

/*
 * Changes to this class. What the people haven been up to:
 *
 * $Log$
 * Revision 1.1  2004/05/11 16:43:25  doemming
 * Initial revision
 *
 * Revision 1.13  2004/03/02 07:38:14  poth
 * no message
 *
 * Revision 1.12  2004/02/23 07:47:50  poth
 * no message
 *
 * Revision 1.11  2004/01/27 07:55:44  poth
 * no message
 *
 * Revision 1.10  2004/01/08 09:50:22  poth
 * no message
 *
 * Revision 1.9  2003/09/14 14:05:08  poth
 * no message
 *
 * Revision 1.8  2003/07/10 15:24:23  mrsnyder
 * Started to implement LabelDisplayElements that are bound to a Polygon.
 * Fixed error in GM_MultiSurface_Impl.calculateCentroidArea().
 *
 * Revision 1.7  2003/07/03 12:32:26  poth
 * no message
 *
 * Revision 1.6  2003/03/20 12:10:29  mrsnyder
 * Rewrote intersects() method.
 *
 * Revision 1.5  2003/03/19 15:30:04  axel_schaefer
 * Intersects: crossing envelopes, but points are not in envelope
 *
 *
 */
