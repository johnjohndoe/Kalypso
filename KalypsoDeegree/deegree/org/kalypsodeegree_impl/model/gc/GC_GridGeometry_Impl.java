/*
 * OpenGIS® Grid Coverage Implementation Specification
 *
 * This Java profile is derived from OpenGIS's specification
 * available on their public web site:
 *
 *     http://www.opengis.org/techno/implementation.htm
 *
 * You can redistribute it, but should not modify it unless
 * for greater OpenGIS compliance.
 */
package org.deegree_impl.model.gc;

import java.io.Serializable;

// CSS dependencies
import org.opengis.ct.CT_MathTransform;
import org.opengis.gc.GC_GridRange;
import org.opengis.gc.GC_GridGeometry;


/**
 * Describes the geometry and georeferencing information of the grid coverage.
 * The grid range attribute determines the valid grid coordinates and allows
 * for calculation of grid size. A grid coverage may or may not have georeferencing.
 *
 * @version 1.00
 * @since   1.00
 */
class GC_GridGeometry_Impl implements GC_GridGeometry, Serializable
{
    
    private GC_GridRange gridRange          = null;
    private CT_MathTransform mathTransform  = null;
    
    GC_GridGeometry_Impl(GC_GridRange gridRange)
    {
        this( gridRange, null );
    }
    
    GC_GridGeometry_Impl(GC_GridRange gridRange, CT_MathTransform mathTransform)
    {
        this.gridRange = gridRange;
        this.mathTransform = mathTransform;
    }
 
    /** The valid coordinate range of a grid coverage.
     * The lowest valid grid coordinate is zero.
     * A grid with 512 cells can have a minimum coordinate of 0 and maximum of 512,
     * with 511 as the highest valid index.
     *
     */
    public GC_GridRange getGridRange() {
        return gridRange;
    }
    
    /** The math transform allows for the transformations from grid coordinates to real
     * world earth coordinates. The transform is often an affine transformation. The
     * coordinate system of the real world coordinates is given by the
     * {@link org.opengis.cv.CV_Coverage#getCoordinateSystem} method.
     * If no math transform is given, this attribute will be <code>null</code>.
     *
     */
    public CT_MathTransform getGridToCoordinateSystem() {
        return mathTransform;
    }
    
}
