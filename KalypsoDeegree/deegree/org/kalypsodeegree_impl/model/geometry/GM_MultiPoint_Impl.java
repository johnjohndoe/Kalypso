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

import java.io.Serializable;

import org.deegree.model.geometry.*;

import org.opengis.cs.*;


/**
* default implementierung of the GM_MultiPoint interface of
* package jago.model. 
*
* <p>------------------------------------------------------------</p>
* @version 12.6.2001
* @author Andreas Poth href="mailto:poth@lat-lon.de"
* <p>
*/
final class GM_MultiPoint_Impl extends GM_MultiPrimitive_Impl implements GM_MultiPoint, Serializable {
    /** Use serialVersionUID for interoperability. */
    private final static long serialVersionUID = -1105623021535230655L;

    /**
     * Creates a new GM_MultiPoint_Impl object.
     *
     * @param crs 
     */
    public GM_MultiPoint_Impl( CS_CoordinateSystem crs ) {
        super( crs );
    }

    /**
     * Creates a new GM_MultiPoint_Impl object.
     *
     * @param gmp 
     */
    public GM_MultiPoint_Impl( GM_Point[] gmp ) {
        super( null );

        for ( int i = 0; i < gmp.length; i++ ) {
            aggregate.add( gmp[i] );
        }

    }

    /**
     * Creates a new GM_MultiPoint_Impl object.
     *
     * @param gmp 
     * @param crs 
     */
    public GM_MultiPoint_Impl( GM_Point[] gmp, CS_CoordinateSystem crs ) {
        super( crs );

        for ( int i = 0; i < gmp.length; i++ ) {
            aggregate.add( gmp[i] );
        }

    }

    /**
     * adds a GM_Point to the aggregation 
     */
    public void addPoint( GM_Point gmp ) {
        super.add( gmp );
    }

    /**
     * inserts a GM_Point into the aggregation. all elements with an index 
     * equal or larger index will be moved. if index is
     * larger then getSize() - 1 or smaller then 0 or gmp equals null 
     * an exception will be thrown.
     *
     * @param gmp GM_Point to insert.     
     * @param index position where to insert the new GM_Point
     */
    public void insertPointAt( GM_Point gmp, int index ) throws GM_Exception {
        super.insertObjectAt( gmp, index );
    }

    /**
     * sets the submitted GM_Point at the submitted index. the element
     * at the position <code>index</code> will be removed. if index is
     * larger then getSize() - 1 or smaller then 0 or gmp equals null 
     * an exception will be thrown.
     *
     * @param gmp GM_Point to set.     
     * @param index position where to set the new GM_Point
     */
    public void setPointAt( GM_Point gmp, int index ) throws GM_Exception {
        setObjectAt( gmp, index );
    }

    /**
     * removes the submitted GM_Point from the aggregation
     *
     * @return the removed GM_Point
     */
    public GM_Point removePoint( GM_Point gmp ) {
        return (GM_Point)super.removeObject( gmp );
    }

    /**
     * removes the GM_Point at the submitted index from the aggregation.
     * if index is larger then getSize() - 1 or smaller then 0 
     * an exception will be thrown.
     *
     * @return the removed GM_Point
     */
    public GM_Point removePointAt( int index ) throws GM_Exception {
        return (GM_Point)super.removeObjectAt( index );
    }

    /**
     * returns the GM_Point at the submitted index. 
     */
    public GM_Point getPointAt( int index ) {
        return (GM_Point)super.getPrimitiveAt( index );
    }

    /**
     * returns all GM_Points as array
     */
    public GM_Point[] getAllPoints() {
        return (GM_Point[])aggregate.toArray( new GM_Point[getSize()] );
    }

    /**
     * updates the bounding box of the aggregation
     */
    private void calculateEnvelope() {
        GM_Point gmp = getPointAt( 0 );

        double[] min = (double[])gmp.getAsArray().clone();
        double[] max = (double[])min.clone();

        for ( int i = 1; i < getSize(); i++ ) {
            double[] pos = getPointAt( i ).getAsArray();

            for ( int j = 0; j < pos.length; j++ ) {
                if ( pos[j] < min[j] ) {
                    min[j] = pos[j];
                } else if ( pos[j] > max[j] ) {
                    max[j] = pos[j];
                }
            }
        }

        envelope = new GM_Envelope_Impl( new GM_Position_Impl( min ), new GM_Position_Impl( max ) );
    }

    /**
     * calculates the centroid of the surface
     */
    private void calculateCentroid() {
        try {
            GM_Point gmp = getPointAt( 0 );

            double[] cen = new double[gmp.getAsArray().length];

            for ( int i = 0; i < getSize(); i++ ) {
                double[] pos = getPointAt( i ).getAsArray();

                for ( int j = 0; j < pos.length; j++ ) {
                    cen[j] += ( pos[j] / getSize() );
                }
            }

            centroid = new GM_Point_Impl( new GM_Position_Impl( cen ), null );
        } catch ( Exception ex ) {
        }
    }

    /**
     * calculates the centroid and envelope of the aggregation
     */
    protected void calculateParam() {
        calculateCentroid();
        calculateEnvelope();
        setValid( true );
    }

    /**
     * The operation "dimension" shall return the inherent dimension of this
     * GM_Object, which shall be less than or equal to the coordinate dimension.
     * The dimension of a collection of geometric objects shall be the largest
     * dimension of any of its pieces. Points are 0-dimensional, curves are
     * 1-dimensional, surfaces are 2-dimensional, and solids are 3-dimensional.
     */
    public int getDimension() {
        return 0;
    }

    /**
     * The operation "coordinateDimension" shall return the dimension of the
     * coordinates that define this GM_Object, which must be the same as the
     * coordinate dimension of the coordinate reference system for this GM_Object.
     */
    public int getCoordinateDimension() {
        GM_Point sp = null;

        try {
            sp = getPointAt( 0 );
        } catch ( Exception ex ) {
        }

        return sp.getAsArray().length;
    }

    /**
    * returns a shallow copy of the geometry
    */
    public Object clone() {
        GM_MultiPoint mp = null;

        try {
            mp = new GM_MultiPoint_Impl( getCoordinateSystem() );

            for ( int i = 0; i < this.getSize(); i++ ) {
                GM_Point_Impl pi = (GM_Point_Impl)getPointAt( i );
                mp.add( (GM_Point)pi.clone() );
            }
        } catch ( Exception ex ) {
            System.out.println( "GM_MultiPoint_Impl.clone: " + ex );
        }

        return mp;
    }
} 