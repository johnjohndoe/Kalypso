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
import java.util.ArrayList;

import org.deegree.model.geometry.GM_Curve;
import org.deegree.model.geometry.GM_CurveBoundary;
import org.deegree.model.geometry.GM_CurveSegment;
import org.deegree.model.geometry.GM_Exception;
import org.deegree.model.geometry.GM_GenericCurve;
import org.deegree.model.geometry.GM_LineString;
import org.deegree.model.geometry.GM_Object;
import org.deegree.model.geometry.GM_Point;
import org.deegree.model.geometry.GM_Position;


/**
 * default implementation of the GM_Curve interface from
 * package jago.model.
 *
 * ------------------------------------------------------------
 * @version 14.10.2001 
 * @author Andreas Poth
 *
 *
 */
class GM_Curve_Impl extends GM_OrientableCurve_Impl implements GM_Curve, GM_GenericCurve,
                                                               Serializable {
    /** Use serialVersionUID for interoperability. */
    private final static long serialVersionUID = 4060425075179654976L;
    protected ArrayList segments = null;

    /**
     * initialize the curve by submitting a spatial reference system and
     * an array of curve segments. the orientation of the curve is '+'
     *
     * @param segments array of GM_CurveSegment
     */
    public GM_Curve_Impl( GM_CurveSegment[] segments ) throws GM_Exception {
        this( '+', segments );
    }

    /**
     * initialize the curve by submitting a spatial reference system,
     * an array of curve segments and the orientation of the curve
     *
     * @param segments array of GM_CurveSegment
     * @param orientation of the curve
     */
    public GM_Curve_Impl( char orientation, GM_CurveSegment[] segments ) throws GM_Exception {
        super( segments[0].getCoordinateSystem(), orientation );

        this.segments = new ArrayList( segments.length );

        if ( segments != null ) {
            for ( int i = 0; i < segments.length; i++ ) {
                this.segments.add( segments[i] );

                if ( i > 0 ) {
                    if ( !segments[i - 1].getEndPoint().equals( segments[i].getStartPoint() ) ) {
                        throw new GM_Exception( 
                                "end-point of segment[i-1] doesn't match start-point of segment[i]!" );
                    }
                }
            }
        }

        setValid( false );
    }

    /**
     * calculates the envelope of the Curve
     */
    private void calculateEnvelope() {
        try {
            GM_Position[] positions = getAsLineString().getPositions();

            double[] min = (double[])positions[0].getAsArray().clone();
            double[] max = (double[])min.clone();

            for ( int i = 1; i < positions.length; i++ ) {
                double[] pos = positions[i].getAsArray();

                for ( int j = 0; j < pos.length; j++ ) {
                    if ( pos[j] < min[j] ) {
                        min[j] = pos[j];
                    } else if ( pos[j] > max[j] ) {
                        max[j] = pos[j];
                    }
                }
            }

            envelope = new GM_Envelope_Impl( new GM_Position_Impl( min ), 
                                             new GM_Position_Impl( max ) );
        } catch ( GM_Exception e ) {
        }
    }

    /**
     * calculates the boundary of the Curve
     */
    private void calculateBoundary() {
        try {
            boundary = new GM_CurveBoundary_Impl( getCoordinateSystem(), 
                                                  getStartPoint().getPosition(), 
                                                  getEndPoint().getPosition() );
        } catch ( GM_Exception e ) {
        }
    }

    /**
     * calculates the centroid of the Curve
     */
    private void calculateCentroid() {
        try {
            GM_Position[] positions = getAsLineString().getPositions();

            double[] cen = new double[positions[0].getAsArray().length];

            for ( int i = 0; i < positions.length; i++ ) {
                double[] pos = positions[i].getAsArray();

                for ( int j = 0; j < pos.length; j++ ) {
                    cen[j] += ( pos[j] / positions.length );
                }
            }

            centroid = new GM_Point_Impl( new GM_Position_Impl( cen ), null );
        } catch ( Exception e ) {
        }
    }

    /**
     *
     */
    protected void calculateParam() {
        calculateCentroid();
        calculateEnvelope();
        calculateBoundary();
        setValid( true );
    }

    /**
     * returns the boundary of the curve
     */
    public GM_CurveBoundary getCurveBoundary() {
        return (GM_CurveBoundary)boundary;
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
        return getStartPoint().getAsArray().length;
    }

    /**
     * The Boolean valued operation "intersects" shall return TRUE if this GM_Object
     * intersects another GM_Object. Within a GM_Complex, the GM_Primitives do not
     * intersect one another. In general, topologically structured data uses shared
     * geometric objects to capture intersection information.<p></p>
     * dummy implementation
     */
    public boolean intersects( GM_Object gmo ) {
        boolean inter = false;

        try {
            for ( int i = 0; i < segments.size(); i++ ) {
                GM_CurveSegment cs = getCurveSegmentAt( i );

                if ( cs.intersects( gmo ) ) {
                    inter = true;
                    break;
                }
            }
        } catch ( Exception e ) {
        }

        return inter;
    }

    /**
     * returns the length of the curve in units of the related spatial reference
     * system
     */
    public double getLength() {
        return -1;
    }

    /**
     * returns the number of segments building the curve
     */
    public int getNumberOfCurveSegments() {
        return segments.size();
    }

    /**
     * returns the first point of the curve. if the curve  doesn't contain a
     * segment or the first segment doesn't contain a point null will be returned
     */
    public GM_Point getStartPoint() {
        if ( getNumberOfCurveSegments() == 0 ) {
            return null;
        }

        GM_Point gmp = null;

        try {
            gmp = getCurveSegmentAt( 0 ).getStartPoint();
        } catch ( GM_Exception e ) {
        }

        return gmp;
    }

    /**
     * returns the last point of the curve.if the curve doesn't contain a segment
     * or the last segment doesn't contain a point null will be returned
     */
    public GM_Point getEndPoint() {
        if ( getNumberOfCurveSegments() == 0 ) {
            return null;
        }

        GM_Point gmp = null;

        try {
            gmp = getCurveSegmentAt( getNumberOfCurveSegments() - 1 ).getEndPoint();
        } catch ( GM_Exception e ) {
        }

        return gmp;
    }

    /**
     * returns the curve as GM_LineString. if there isn't a curve
     * segment within the curve null will be returned
     */
    public GM_LineString getAsLineString() throws GM_Exception {
        if ( getNumberOfCurveSegments() == 0 ) {
            return null;
        }

        GM_Position[] tmp = null;

        // normal orientaton
        if ( getOrientation() == '+' ) {
            int cnt = 0;

            for ( int i = 0; i < getNumberOfCurveSegments(); i++ ) {
                cnt += getCurveSegmentAt( i ).getNumberOfPoints();
            }

            tmp = new GM_Position[cnt];

            int k = 0;

            for ( int i = 0; i < getNumberOfCurveSegments(); i++ ) {
                GM_Position[] gmps = getCurveSegmentAt( i ).getPositions();

                for ( int j = 0; j < gmps.length; j++ ) {
                    tmp[k++] = gmps[j];
                }
            }
        } else {
            // inverse orientation
            int cnt = 0;

            for ( int i = getNumberOfCurveSegments() - 1; i >= 0; i-- ) {
                cnt += getCurveSegmentAt( i ).getNumberOfPoints();
            }

            tmp = new GM_Position[cnt];

            int k = 0;

            for ( int i = getNumberOfCurveSegments() - 1; i >= 0; i-- ) {
                GM_Position[] gmps = getCurveSegmentAt( i ).getPositions();

                for ( int j = gmps.length - 1; j >= 0; j-- ) {
                    tmp[k++] = gmps[j];
                }
            }
        }

        return new GM_LineString_Impl( tmp, this.crs );
    }

    /**
     * returns the curve segment at the submitted index
     *
     * @param index index of the curve segment that should be returned
     * @exception GM_Exception a exception will be thrown if <tt>index</tt> is smaller
     *             than '0' or larger than <tt>getNumberOfCurveSegments()-1</tt>
     */
    public GM_CurveSegment getCurveSegmentAt( int index ) throws GM_Exception {
        if ( ( index < 0 ) || ( index > getNumberOfCurveSegments() - 1 ) ) {
            throw new GM_Exception( "invalid index/position to get a segment!" );
        }

        return (GM_CurveSegment)segments.get( index );
    }

    /**
     * writes a segment to the curve at submitted position. the old point
     * will be deleted
     *
     * @param segment curve segment that should be set
     * @param index index where to set the curve segment
     * @exception GM_Exception  a exception will be thrown if <tt>index</tt> is smaller
     *             than '0' or larger than <tt>getNumberOfCurveSegments()-1</tt> or
     *            or the starting point of the submitted curve segment isn't equal to
     *             the ending point of segment at <tt>index-1</tt> and/or the ending
     *             point of the submitted segment isn't equals to the curve segment at
     *             <tt>index+1</tt>
     */
    public void setCurveSegmentAt( GM_CurveSegment segment, int index ) throws GM_Exception {
        if ( ( index < 0 ) || ( index > getNumberOfCurveSegments() - 1 ) ) {
            throw new GM_Exception( "invalid index/position to set a segment!" );
        }

        /*
         * checks if the start/endpoint of  the inserted segment is equal
         * to the end/startpoint of the successor/previous segment
         * start segx == start segx-1
         */
        GM_Point p1 = segment.getEndPoint();
        GM_Point p2 = segment.getStartPoint();

        if ( index == 0 ) {
            GM_Point p4 = getCurveSegmentAt( index + 1 ).getStartPoint();

            /*
             * insert segment at beginning of curve
             */
            if ( !p1.equals( p4 ) ) {
                throw new GM_Exception( 
                        "end-point of segment[i-1] doesn't match start-point of segment[i]!" );
            }
        } else if ( ( index > 0 ) && ( index < ( getNumberOfCurveSegments() - 1 ) ) ) {
            GM_Point p4 = getCurveSegmentAt( index + 1 ).getStartPoint();
            GM_Point p5 = getCurveSegmentAt( index - 1 ).getEndPoint();

            /*
             * insert segment anywhere in curve
             */
            if ( !p1.equals( p4 ) || !p2.equals( p5 ) ) {
                throw new GM_Exception( 
                        "end-point of segment[i-1 || i]  doesn't match start-point of segment[i || i+1]!" );
            }
        } else if ( index == ( getNumberOfCurveSegments() - 1 ) ) {
            GM_Point p5 = getCurveSegmentAt( index - 1 ).getEndPoint();

            /*
             * insert segment at end of curve
             */
            if ( !p2.equals( p5 ) ) {
                throw new GM_Exception( 
                        "end-point of segment[i-1 || i]  doesn't match start-point of segment[i || i+1]!" );
            }
        }

        segments.set( index, segment );

        setValid( false );
    }

    /**
     * inserts a segment in the curve at the submitted position. all
     * points with a position that equals index or is higher will
     * be shifted
     *
     * @param segment curve segment that should be inserted
     * @param index index where to insert the curve segment
     * @exception GM_Exception a exception will be thrown if <tt>index</tt> is smaller
     *             than '0' or larger than <tt>getNumberOfCurveSegments()-1</tt> or
     *            or the starting point of the submitted curve segment isn't equal to
     *             the ending point of segment at <tt>index-1</tt> and/or the ending
     *             point of the submitted segment isn't equals to the curve segment at
     *             <tt>index+1</tt>
     */
    public void insertCurveSegmentAt( GM_CurveSegment segment, int index )
                              throws GM_Exception {
        if ( ( index < 0 ) || ( index > getNumberOfCurveSegments() - 1 ) ) {
            throw new GM_Exception( "invalid index/position to insert a segment!" );
        }

        /*
         * checks if the start/endpoint of  the inserted segment is equal
         * to the end/startpoint of the successor/previous segment
         */
        GM_Point p1 = segment.getEndPoint();
        GM_Point p2 = segment.getStartPoint();

        if ( index == 0 ) {
            GM_Point p4 = getCurveSegmentAt( index + 1 ).getStartPoint();

            /*
             * insert segment at beginning of curve
             */
            if ( !p1.equals( p4 ) ) {
                throw new GM_Exception( 
                        "end-point of segment[i] doesn't match start-point of segment[i+1]!" );
            }
        } else if ( ( index > 0 ) && ( index < ( getNumberOfCurveSegments() - 1 ) ) ) {
            GM_Point p4 = getCurveSegmentAt( index + 1 ).getStartPoint();
            GM_Point p5 = getCurveSegmentAt( index - 1 ).getEndPoint();

            /*
             * insert segment anywhere in curve
             */
            if ( !p1.equals( p4 ) || !p2.equals( p5 ) ) {
                throw new GM_Exception( 
                        "end-point of segment[i-1 || i]  doesn't match start-point of segment[i || i+1]!" );
            }
        } else if ( index == ( getNumberOfCurveSegments() - 1 ) ) {
            GM_Point p5 = getCurveSegmentAt( index - 1 ).getEndPoint();

            /*
             * insert segment at end of curve
             */
            if ( !p2.equals( p5 ) ) {
                throw new GM_Exception( 
                        "end-point of segment[i-1]  doesn't match start-point of segment[i]!" );
            }
        }

        segments.add( index, segment );

        setValid( false );
    }

    /**
     * adds a segment at the end of the curve
     *
     * @param segment curve segment that should be set
     * @exception GM_Exception a exception will be thrown if the starting point of the submitted
     *             curve segment isn't equal to the ending point of the last segment.
     */
    public void addCurveSegment( GM_CurveSegment segment ) throws GM_Exception {
        // TODO: Was wenn keine Curve da?
        GM_Point p2 = getEndPoint();

        segments.add( segment );

        GM_Point p1 = segment.getStartPoint();

        if ( !p1.equals( p2 ) ) {
            throw new GM_Exception( 
                    "EndPoint of last segment doesn't match StartPoint of adding segment" );
        }

        setValid( false );
    }

    /**
     * deletes the segment at the submitted index
     *
     * @param index index of the curve segement that should be removed
     *         from the curve.
     * @exception GM_Exception will be thrown if <tt>index</tt> is smaller '0' or
     *             larger <tt>getNumberOfCurveSegments()-1</tt>
     */
    public void deleteCurveSegmentAt( int index ) throws GM_Exception {
        if ( ( index < 0 ) || ( index > getNumberOfCurveSegments() - 1 ) ) {
            throw new GM_Exception( "invalid index/position to remove a segment!" );
        }

        GM_Point p1 = getCurveSegmentAt( index - 1 ).getEndPoint();
        // p1 (index.sp) and p2 (index.ep) not used here!
        GM_Point p4 = getCurveSegmentAt( index + 1 ).getStartPoint();

        if ( index > 0 ) {
            if ( !p1.equals( p4 ) ) {
                throw new GM_Exception( 
                        "end-point of segment[index-1] doesn't match start-point of segment[index+1]!" );
            }
        }

        segments.remove( index );

        setValid( false );
    }

    /**
     * returns true if no segment is within the curve
     */
    public boolean isEmpty() {
        return ( getNumberOfCurveSegments() == 0 );
    }

    /**
     * translate each point of the curve with the values of the submitted
     * double array.
     */
    public void translate( double[] d ) {
        try {
            for ( int i = 0; i < segments.size(); i++ ) {
                GM_Position[] pos = getCurveSegmentAt( i ).getPositions();

                for ( int j = 0; j < pos.length; j++ ) {
                    pos[j].translate( d );
                }
            }
        } catch ( Exception e ) {
        }
        setValid( false );
    }

    /**
     * checks if this curve is completly equal to the submitted geometry
     * @param other object to compare to
     */
    public boolean equals( Object other ) {
        if ( !super.equals( other ) ) {
            return false;
        }

        if ( !( other instanceof GM_Curve_Impl ) ) {
            return false;
        }

        // Bugfix: envelope maybe not yet valid
        if ( !getEnvelope().equals( ( (GM_Object)other ).getEnvelope() ) ) {
            return false;
        }

        if ( getNumberOfCurveSegments() != ( (GM_Curve)other ).getNumberOfCurveSegments() ) {
            return false;
        }

        try {
            for ( int i = 0; i < segments.size(); i++ ) {
                if ( !getCurveSegmentAt( i ).equals( ( (GM_Curve)other ).getCurveSegmentAt( i ) ) ) {
                    return false;
                }
            }
        } catch ( Exception e ) {
            return false;
        }

        return true;
    }

    /**
     * returns a shallow copy of the geometry
     */
    public Object clone() {
        GM_Curve c = null;

        try {
            GM_CurveSegment[] cs = null;
            cs = (GM_CurveSegment[])segments.toArray( 
                           new GM_CurveSegment[getNumberOfCurveSegments()] );
            c = new GM_Curve_Impl( getOrientation(), cs );
        } catch ( Exception ex ) {
            System.out.println( "GM_Curve_Impl.clone: " + ex );
        }

        return c;
    }

    /**
     *
     *
     * @return 
     */
    public String toString() {
        String ret = null;
        ret = "segments = " + segments + "\n";
        ret += ( "envelope = " + envelope + "\n" );
        return ret;
    }
}
