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
* default implementation of the GM_MultiCurve interface from
* package jago.model. 
*
* ------------------------------------------------------------
* @version 12.6.2001
* @author Andreas Poth
*/
final class GM_MultiCurve_Impl extends GM_MultiPrimitive_Impl implements GM_MultiCurve, Serializable {
    /** Use serialVersionUID for interoperability. */
    private final static long serialVersionUID = 2730942874409216686L;

    /**
     * Creates a new GM_MultiCurve_Impl object.
     *
     * @param crs 
     */
    public GM_MultiCurve_Impl( CS_CoordinateSystem crs ) {
        super( crs );
    }

    /**
     * Creates a new GM_MultiCurve_Impl object.
     *
     * @param gmc 
     */
    public GM_MultiCurve_Impl( GM_Curve[] gmc ) {
        super( null );

        for ( int i = 0; i < gmc.length; i++ ) {
            aggregate.add( gmc[i] );
        }

    }

    /**
     * Creates a new GM_MultiCurve_Impl object.
     *
     * @param gmc 
     * @param crs 
     */
    public GM_MultiCurve_Impl( GM_Curve[] gmc, CS_CoordinateSystem crs ) {
        super( crs );

        for ( int i = 0; i < gmc.length; i++ ) {
            aggregate.add( gmc[i] );
        }

    }

    /**
    * adds a GM_Curve to the aggregation 
    */
    public void addCurve( GM_Curve gmc ) {
        super.add( gmc );
    }

    /**
     * inserts a GM_Curve in the aggregation. all elements with an index 
     * equal or larger index will be moved. if index is
     * larger then getSize() - 1 or smaller then 0 or gmc equals null 
     * an exception will be thrown.
     *
     * @param gmc GM_Curve to insert.     
     * @param index position where to insert the new GM_Curve
     */
    public void insertCurveAt( GM_Curve gmc, int index ) throws GM_Exception {
        super.insertObjectAt( gmc, index );
    }

    /**
     * sets the submitted GM_Curve at the submitted index. the element
     * at the position <code>index</code> will be removed. if index is
     * larger then getSize() - 1 or smaller then 0 or gmc equals null 
     * an exception will be thrown.
     *
     * @param gmc GM_Curve to set.     
     * @param index position where to set the new GM_Curve
     */
    public void setCurveAt( GM_Curve gmc, int index ) throws GM_Exception {
        setObjectAt( gmc, index );
    }

    /**
     * removes the submitted GM_Curve from the aggregation
     *
     * @return the removed GM_Curve
     */
    public GM_Curve removeCurve( GM_Curve gmc ) {
        return (GM_Curve)super.removeObject( gmc );
    }

    /**
     * removes the GM_Curve at the submitted index from the aggregation.
     * if index is larger then getSize() - 1 or smaller then 0 
     * an exception will be thrown.
     *
     * @return the removed GM_Curve
     */
    public GM_Curve removeCurveAt( int index ) throws GM_Exception {
        return (GM_Curve)super.removeObjectAt( index );
    }

    /**
     * removes all GM_Curve from the aggregation. 
     */
    public void removeAll() {
        super.removeAll();
    }

    /**
     * returns the GM_Curve at the submitted index. 
     */
    public GM_Curve getCurveAt( int index ) {
        return (GM_Curve)super.getPrimitiveAt( index );
    }

    /**
     * returns all GM_Curves as array
     */
    public GM_Curve[] getAllCurves() {
        return (GM_Curve[])aggregate.toArray( new GM_Curve[getSize()] );
    }

    /**
     * returns true if the submitted GM_Curve is within the aggregation
     */
    public boolean isMember( GM_Curve gmc ) {
        return super.isMember( gmc );
    }

    /**
     * returns the boundary of the MultiCurve<p>
     * not implemented yet
     */
    public GM_Boundary getBoundary() {
        return null;
    }

    /**
     * calculates the bounding box / envelope of the aggregation
     */
    protected void calculateEnvelope() {
        GM_Envelope bb = getCurveAt( 0 ).getEnvelope();

        double[] min = (double[])bb.getMin().getAsArray().clone();
        double[] max = (double[])bb.getMax().getAsArray().clone();

        for ( int i = 1; i < getSize(); i++ ) {
            double[] pos1 = getCurveAt( i ).getEnvelope().getMin().getAsArray();
            double[] pos2 = getCurveAt( i ).getEnvelope().getMax().getAsArray();

            for ( int j = 0; j < pos1.length; j++ ) {
                if ( pos1[j] < min[j] ) {
                    min[j] = pos1[j];
                } else if ( pos1[j] > max[j] ) {
                    max[j] = pos1[j];
                }

                if ( pos2[j] < min[j] ) {
                    min[j] = pos2[j];
                } else if ( pos2[j] > max[j] ) {
                    max[j] = pos2[j];
                }
            }
        }

        envelope = new GM_Envelope_Impl( new GM_Position_Impl( min ), new GM_Position_Impl( max ) );
    }

    /**
     * calculates the centroid of the aggregation
     */
    protected void calculateCentroid() {
        try {
            double cnt = 0;
            GM_Point gmp = getCurveAt( 0 ).getCentroid();

            double[] cen = new double[gmp.getAsArray().length];

            for ( int i = 0; i < getSize(); i++ ) {
                cnt += getCurveAt( i ).getNumberOfCurveSegments();

                double[] pos = getCurveAt( i ).getCentroid().getAsArray();

                for ( int j = 0; j < getCoordinateDimension(); j++ ) {
                    cen[j] += pos[j];
                }
            }

            for ( int j = 0; j < getCoordinateDimension(); j++ ) {
                cen[j] = cen[j] / cnt / getSize();
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
        return 1;
    }

    /**
     * The operation "coordinateDimension" shall return the dimension of the
     * coordinates that define this GM_Object, which must be the same as the
     * coordinate dimension of the coordinate reference system for this GM_Object.
     */
    public int getCoordinateDimension() {
        GM_CurveSegment sp = null;

        try {
            sp = getCurveAt( 0 ).getCurveSegmentAt( 0 );
        } catch ( Exception ex ) {
        }

        return sp.getPositionAt( 0 ).getAsArray().length;
    }

    /**
    * returns a shallow copy of the geometry
    */
    public Object clone() {
        GM_MultiCurve mc = null;

        try {
            mc = new GM_MultiCurve_Impl( getCoordinateSystem() );

            for ( int i = 0; i < this.getSize(); i++ ) {
                GM_Curve_Impl ci = (GM_Curve_Impl)getCurveAt( i );
                mc.addCurve( (GM_Curve)ci.clone() );
            }
        } catch ( Exception ex ) {
            System.out.println( "GM_MultiCurve_Impl.clone: " + ex );
        }

        return mc;
    }
} 