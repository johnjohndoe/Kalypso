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

import org.deegree.model.geometry.GM_Exception;
import org.deegree.model.geometry.GM_GenericSurface;
import org.deegree.model.geometry.GM_Object;
import org.deegree.model.geometry.GM_Position;
import org.deegree.model.geometry.GM_Ring;
import org.deegree.model.geometry.GM_Surface;
import org.deegree.model.geometry.GM_SurfaceBoundary;
import org.deegree.model.geometry.GM_SurfacePatch;


/**

 * default implementation of the GM_Surface interface from
 * package jago.model.<p></p>
 * for simplicity of the implementation it is assumed that a surface is build
 * from just one surface patch. this isn't completly confrom to the ISO 19107
 * and the OGC GAIA specification but sufficient for most applications.<p></p>
 * It will be extended to fullfill the complete specs as soon as possible.
 *
 * <p>-----------------------------------------------------------------------</p>
 * @version 05.04.2002
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 */
class GM_Surface_Impl extends GM_OrientableSurface_Impl implements GM_Surface, GM_GenericSurface,
                                                                   Serializable {
    /** Use serialVersionUID for interoperability. */
    private final static long serialVersionUID = -2148069106391096842L;
    protected GM_SurfacePatch patch = null;
    private double area = 0;

    /**
     * initializes the surface with default orientation submitting one
     * surface patch.
     *
     * @param surfacePatch patches of the surface.
     */
    public GM_Surface_Impl( GM_SurfacePatch surfacePatch ) throws GM_Exception {
        this( '+', surfacePatch );
    }

    /**
     * initializes the surface submitting the orientation and one
     * surface patch.
     *
     * @param surfacePatch patches of the surface.
     */
    public GM_Surface_Impl( char orientation, GM_SurfacePatch surfacePatch )
                    throws GM_Exception {
        super( surfacePatch.getCoordinateSystem(), orientation );

        patch = surfacePatch;

        setValid( false );
    }

    /**
     * initializes the surface with default orientation submitting
     * the surfaces boundary
     *
     * @param boundary boundary of the surface
     */
    public GM_Surface_Impl( GM_SurfaceBoundary boundary ) throws GM_Exception {
        this( '+', boundary );
    }

    /**
     * initializes the surface submitting the orientation and
     * the surfaces boundary.
     *
     * @param boundary boundary of the surface
     */
    public GM_Surface_Impl( char orientation, GM_SurfaceBoundary boundary )
                    throws GM_Exception {
        // todo
        // extracting surface patches from the boundary
        super( boundary.getCoordinateSystem(), orientation );

        this.boundary = boundary;
    }

    /**
     * calculates the centroid and area of the surface
     */
    private void calculateCentroidArea() {
        centroid = patch.getCentroid();
        area = patch.getArea();
    }

    /**
     * calculates the boundary and area of the surface
     */
    private void calculateBoundary() {
        try {
            GM_Ring ext = new GM_Ring_Impl( patch.getExteriorRing(), crs );
            GM_Position[][] inn_ = patch.getInteriorRings();
            GM_Ring[] inn = null;

            if ( inn_ != null ) {
                inn = new GM_Ring_Impl[inn_.length];

                for ( int i = 0; i < inn_.length; i++ ) {
                    inn[i] = new GM_Ring_Impl( inn_[i], crs );
                }
            }

            boundary = new GM_SurfaceBoundary_Impl( ext, inn );
        } catch ( Exception e ) {
            System.out.println( e );
        }
    }

    /**
     * calculates area, centroid and the envelope of the surface
     */
    protected void calculateParam() {
        calculateCentroidArea();
        calculateEnvelope();
        calculateBoundary();
        setValid( true );
    }

    /**
     * calculates the envelope of the surface
     */
    private void calculateEnvelope() {
        envelope = patch.getEnvelope();
    }

    /**
     * returns the length of all boundaries of the surface
     * in a reference system appropriate for measuring distances.
     */
    public double getPerimeter() {
        return -1;
    }

    /**
     * The operation "area" shall return the area of this GM_GenericSurface.
     * The area of a 2 dimensional geometric object shall be a numeric
     * measure of its surface area Since area is an accumulation (integral)
     * of the product of two distances, its return value shall be in a unit
     * of measure appropriate for measuring distances squared.
     */
    public double getArea() {
        if ( !isValid() ) {
            calculateParam();
        }
        return area;
    }

    /**
     * returns the boundary of the surface as surface boundary
     */
    public GM_SurfaceBoundary getSurfaceBoundary() {
        if ( !isValid() ) {
            calculateParam();
        }
        return (GM_SurfaceBoundary)boundary;
    }

    /**
     * returns the number of patches building the surface
     */
    public int getNumberOfSurfacePatches() {
        return 1;
    }

    /**
     * returns the surface patch at the submitted index
     */
    public GM_SurfacePatch getSurfacePatchAt( int index ) throws GM_Exception {
        if ( index != 0 ) {
            throw new GM_Exception( "invalid index/position to get a patch!" );
        }

        return patch;
    }

    /**
     * writes a surface patch to the surface at submitted position. the old patch
     * will be deleted
     */
    public void setSurfacePatchAt( GM_SurfacePatch patch, int index ) throws GM_Exception {
        if ( index != 0 ) {
            throw new GM_Exception( "invalid index/position to set a patch!" );
        }

        this.patch = patch;

        setValid( false );
    }

    /**
     * inserts a surface patch in the curve at the submitted position. all
     * points with a position that equals index or is higher will
     * be shifted
     */
    public void insertSurfacePatchAt( GM_SurfacePatch patch, int index ) throws GM_Exception {
        if ( index != 0 ) {
            throw new GM_Exception( "invalid index/position to insert a patch!" );
        }

        this.patch = patch;

        setValid( false );
    }

    /**
     * adds a surface patch at the end of the curve
     */
    public void addSurfacePatch( GM_SurfacePatch patch ) {
        throw new NoSuchMethodError( "Surfaces made of more then one surface patch " + 
                                     "are not supported at the moment." );
    }

    /**
     * deletes the surface patch at the submitted index
     */
    public void deleteSurfacePatchAt( int index ) throws GM_Exception {
        throw new NoSuchMethodError( "Surfaces made of more then one surface patch " + 
                                     "are not supported at the moment. Because " + 
                                     "empty surface are not allowed you can't delete " + 
                                     "the only existing patch." );
    }

    /**
     * checks if this surface is completly equal to the submitted geometry
     * @param other object to compare to
     */
    public boolean equals( Object other ) {
	if(other==this)
	    return true;
	
	if ( !super.equals( other ) ) {
            return false;
        }

        if ( !( other instanceof GM_Surface_Impl ) ) {
            return false;
        }

        if ( envelope == null ) {
        	calculateEnvelope();
        }
        if ( !envelope.equals( ( (GM_Object)other ).getEnvelope() ) ) {
            return false;
        }

        try {
            if ( !patch.equals( ( (GM_Surface)other ).getSurfacePatchAt( 0 ) ) ) {
                return false;
            }
        } catch ( Exception e ) {
	    e.printStackTrace();
           return false;
        }

        return true;
    }

    /**
     * The operation "dimension" shall return the inherent dimension of this
     * GM_Object, which shall be less than or equal to the coordinate dimension.
     * The dimension of a collection of geometric objects shall be the largest
     * dimension of any of its pieces. Points are 0-dimensional, curves are
     * 1-dimensional, surfaces are 2-dimensional, and solids are 3-dimensional.
     */
    public int getDimension() {
        return 2;
    }

    /**
     * The operation "coordinateDimension" shall return the dimension of the
     * coordinates that define this GM_Object, which must be the same as the
     * coordinate dimension of the coordinate reference system for this GM_Object.
     */
    public int getCoordinateDimension() {
        return patch.getExteriorRing()[0].getAsArray().length;
    }

    /**
     * returns a shallow copy of the geometry
     */
    public Object clone() {
        GM_Surface s = null;

        try {
            s = new GM_Surface_Impl( getOrientation(), patch );
        } catch ( Exception ex ) {
        }

        return s;
    }

    /**
     * translate each point of the surface with the values of the submitted
     * double array.
     */
    public void translate( double[] d ) {
        GM_Position[] ext = patch.getExteriorRing();
        GM_Position[][] inn = patch.getInteriorRings();

        for ( int j = 0; j < ext.length; j++ ) {
            ext[j].translate( d );
        }

        if ( inn != null ) {
            for ( int j = 0; j < inn.length; j++ ) {
                for ( int k = 0; k < inn[j].length; k++ ) {
                    inn[j][k].translate( d );
                }
            }
        }
        setValid( false );
    }

    /**
     * The boolean valued operation "intersects" shall return TRUE if this
     * <tt>GM_Surface_Impl</tt> intersects with the given <tt>GM_Object</t>.
     * Within a <tt>GM_Complex</tt>, the <tt>GM_Primitives</tt> do not
     * intersect one another. In general, topologically structured data uses
     * shared geometric objects to capture intersection information.
     * @param gmo the <tt>GM_Object</tt> to test for intersection
     * @return true if the <tt>GM_Object</tt> intersects with this
     */
    public boolean intersects( GM_Object gmo ) {
        if ( !isValid() ) {
            calculateParam();
        }
        return patch.contains( gmo ) || boundary.intersects( gmo );
    }

    /**
     * The Boolean valued operation "contains" shall return TRUE if this GM_Object
     * contains a single point given by a coordinate.<p></p>
     */
    public boolean contains( GM_Position position ) {
        return contains( new GM_Point_Impl( position, null ) );
    }

    /**
     * The Boolean valued operation "contains" shall return TRUE if this GM_Object
     * contains another GM_Object.<p></p>
     */
    public boolean contains( GM_Object gmo ) {
        if ( !isValid() ) {
            calculateParam();
        }
        return boundary.contains( gmo );
    }

    /**
     *
     *
     * @return 
     */
    public String toString() {
        String ret = getClass().getName() + ":\n";
        ret += ( "envelope = " + envelope + "\n" );
        ret += ( "patch = " + patch + "\n" );
        return ret;
    }
}
