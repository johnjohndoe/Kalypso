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
package org.deegree_impl.model.ct;

import org.deegree.model.geometry.GM_Curve;
import org.deegree.model.geometry.GM_CurveSegment;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_MultiCurve;
import org.deegree.model.geometry.GM_MultiPoint;
import org.deegree.model.geometry.GM_MultiSurface;
import org.deegree.model.geometry.GM_Object;
import org.deegree.model.geometry.GM_Point;
import org.deegree.model.geometry.GM_Position;
import org.deegree.model.geometry.GM_Surface;
import org.deegree.model.geometry.GM_SurfacePatch;
import org.deegree_impl.model.cs.ConvenienceCSFactory;
import org.deegree_impl.model.cs.CoordinateSystem;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.deegree_impl.model.pt.CoordinatePoint;
import org.deegree_impl.tools.Debug;
import org.opengis.cs.CS_CoordinateSystem;


/**
 * class for transforming deegree geometries to new coordinate
 * reference systems.
 *
 * <p>------------------------------------------------------------</p>
 *
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version $Revision$ $Date$
 */
final public class GeoTransformer {
    private CS_CoordinateSystem targetOGCCS = null;
    private ConvenienceCSFactory csFactory = null;
    private CoordinateSystem targetCS = null;
   
    
    /**
     * Creates a new GeoTransformer object.
     *
     * @param targetCS 
     *
     * @throws Exception 
     */
    public GeoTransformer( String targetCS ) throws Exception {
        csFactory = ConvenienceCSFactory.getInstance();
        this.targetCS = csFactory.getCSByName( targetCS );
        setTargetCS( this.targetCS );
    }

    /**
     * Creates a new GeoTransformer object.
     *
     * @param targetCS 
     *
     * @throws Exception 
     */
    public GeoTransformer( CoordinateSystem targetCS ) throws Exception {
        setTargetCS( targetCS );
        csFactory = ConvenienceCSFactory.getInstance();
    }

    /**
     * Creates a new GeoTransformer object.
     *
     * @param targetCS 
     *
     * @throws Exception 
     */
    public GeoTransformer( CS_CoordinateSystem targetCS ) throws Exception {
        setTargetCS( targetCS );
        csFactory = ConvenienceCSFactory.getInstance();
    }

    /**
     * sets the target coordinate reference system of the Transformer
     */
    public void setTargetCS( CS_CoordinateSystem targetCS ) throws Exception {
        targetOGCCS = targetCS;
        this.targetCS = org.deegree_impl.model.cs.Adapters.getDefault().wrap( targetCS );
    }

    /**
     * sets the target coordinate reference system of the Transformer
     */
    public void setTargetCS( CoordinateSystem targetCS ) throws Exception {
        this.targetCS = targetCS;
        targetOGCCS = org.deegree_impl.model.cs.Adapters.getDefault().export( targetCS );
    }
    
    /**
     * returns the target CRS of the <tt>GeoTransformer</tt>
     */
    public CS_CoordinateSystem getTargetCS() {
        return targetOGCCS;
    }

    /**
     * transforms the coodinates of a deegree geometry to the target
     * coordinate reference system.
     */
    public GM_Object transform( GM_Object geo ) throws Exception {
        return transform(geo,geo.getCoordinateSystem());
    }
    
    public GM_Object transform( GM_Object geo,CS_CoordinateSystem srcCS ) throws Exception {
	CoordinateSystem cs= org.deegree_impl.model.cs.Adapters.getDefault().wrap(srcCS);

        ConvenienceTransformFactory ctf = null;
        ctf = ConvenienceTransformFactory.getInstance();

        MathTransform trans = ctf.getTransform(cs, targetCS );

        if ( geo instanceof GM_Point ) {
            geo = transformPoint( (GM_Point)geo, trans );
        } else if ( geo instanceof GM_Curve ) {
            geo = transformCurve( (GM_Curve)geo, trans );
        } else if ( geo instanceof GM_Surface ) {
            geo = transformSurface( (GM_Surface)geo, trans );
        } else if ( geo instanceof GM_MultiPoint ) {
            geo = transformMultiPoint( (GM_MultiPoint)geo, trans );
        } else if ( geo instanceof GM_MultiCurve ) {
            geo = transformMultiCurve( (GM_MultiCurve)geo, trans );
        } else if ( geo instanceof GM_MultiSurface ) {
            geo = transformMultiSurface( (GM_MultiSurface)geo, trans );
        }

        return geo;
    }

    /**
     * transforms the submitted point to the target coordinate reference system
     */
    private GM_Object transformPoint( GM_Point geo, MathTransform trans ) throws Exception {
        Debug.debugMethodBegin( this, "transformPoint" );

        double[] din = geo.getAsArray();
        
	/*
	// wozu das ??
        if ( geo.getCoordinateSystem().getName().equalsIgnoreCase( "EPSG:4326" ) ) 
	    {
		if ( din[0] <= -180 ) din[0] = -179.999;
		else if ( din[0] >= 180 ) din[0] = 179.999;        
		if ( din[1] <= -90 ) din[1] = -89.999;
		else if ( din[1] >= 90 ) din[1] = 89.999;
	    }
	 */
        double[] dout = new double[din.length];
        trans.transform( din, 0, dout, 0, din.length - 1 );

        geo = GeometryFactory.createGM_Point( dout[0], dout[1], targetOGCCS );

        Debug.debugMethodEnd();
        return geo;
    }

    /**
     * transforms the submitted curve to the target coordinate reference system
     */
    private GM_Object transformCurve( GM_Curve geo, MathTransform trans ) throws Exception {
        Debug.debugMethodBegin( this, "transformCurve" );        

        GM_CurveSegment[] newcus = new GM_CurveSegment[geo.getNumberOfCurveSegments()];

        for ( int i = 0; i < geo.getNumberOfCurveSegments(); i++ ) {
            GM_CurveSegment cus = geo.getCurveSegmentAt( i );
            GM_Position[] pos = cus.getPositions();
            pos = transformPositions( pos, trans );
            newcus[i] = GeometryFactory.createGM_CurveSegment( pos, targetOGCCS );
        }

        geo = GeometryFactory.createGM_Curve( newcus );

        Debug.debugMethodEnd();
        return geo;
    }

    /**
     * transforms the submitted surface to the target coordinate reference system
     */
    private GM_Object transformSurface( GM_Surface geo, MathTransform trans )
                                throws Exception {
        Debug.debugMethodBegin( this, "transformSurface" );
        
        int cnt = geo.getNumberOfSurfacePatches();
        GM_SurfacePatch[] patches = new GM_SurfacePatch[cnt];

        for ( int i = 0; i < cnt; i++ ) {
            GM_SurfacePatch p = geo.getSurfacePatchAt( i );
            GM_Position[] ex = p.getExteriorRing();
            ex = transformPositions( ex, trans );

            GM_Position[][] in = p.getInteriorRings();
            GM_Position[][] inn = null;

            if ( in != null ) {
                inn = new GM_Position[in.length][];

                for ( int k = 0; k < in.length; k++ ) {
                    inn[k] = transformPositions( in[i], trans );
                }
            }

            patches[i] = GeometryFactory.createGM_SurfacePatch( ex, inn, p.getInterpolation(), targetOGCCS );
        }

        // at the moment only polygons made of one patch are supported
        geo = GeometryFactory.createGM_Surface( patches[0] );

        Debug.debugMethodEnd();
        return geo;
    }

    /**
     * transforms the submitted multi point to the target coordinate reference
     * system
     */
    private GM_Object transformMultiPoint( GM_MultiPoint geo, MathTransform trans )
                                   throws Exception {
        Debug.debugMethodBegin( this, "transformMultiPoint" );

        GM_Point[] points = new GM_Point[geo.getSize()];

        for ( int i = 0; i < geo.getSize(); i++ ) {
            CoordinatePoint point = new CoordinatePoint( geo.getPointAt( i ).getAsArray() );
            point = trans.transform( point, point );
            points[i] = GeometryFactory.createGM_Point( point.ord[0], point.ord[1], targetOGCCS );
        }

        geo = GeometryFactory.createGM_MultiPoint( points );

        Debug.debugMethodEnd();
        return geo;
    }

    /**
     * transforms the submitted multi curve to the target coordinate reference
     * system
     */
    private GM_Object transformMultiCurve( GM_MultiCurve geo, MathTransform trans )
                                   throws Exception {
        Debug.debugMethodBegin( this, "transformMultiPoint" );

        GM_Curve[] curves = new GM_Curve[geo.getSize()];

        for ( int i = 0; i < geo.getSize(); i++ ) {
            curves[i] = (GM_Curve)transformCurve( geo.getCurveAt( i ), trans );
        }

        geo = GeometryFactory.createGM_MultiCurve( curves );

        Debug.debugMethodEnd();
        return geo;
    }

    /**
     * transforms the submitted multi surface to the target coordinate reference
     * system
     */
    private GM_Object transformMultiSurface( GM_MultiSurface geo, MathTransform trans )
                                     throws Exception {
        Debug.debugMethodBegin( this, "transformMultiPoint" );

        GM_Surface[] surfaces = new GM_Surface[geo.getSize()];

        for ( int i = 0; i < geo.getSize(); i++ ) {
            surfaces[i] = (GM_Surface)transformSurface( geo.getSurfaceAt( i ), trans );
        }

        geo = GeometryFactory.createGM_MultiSurface( surfaces );

        Debug.debugMethodEnd();
        return geo;
    }

    /**
     * transfroms an array of GM_Positions to the target coordinate reference
     * system
     */
    private GM_Position[] transformPositions( GM_Position[] pos, MathTransform trans ) 
																	throws Exception {
        Debug.debugMethodBegin( this, "transformPositions" );

        GM_Position[] newpos = new GM_Position[pos.length];

        for ( int k = 0; k < pos.length; k++ ) {
            double[] din = pos[k].getAsArray();
            double[] dout = new double[din.length];
            trans.transform( din, 0, dout, 0, din.length - 1 );
            newpos[k] = GeometryFactory.createGM_Position( dout );
        }

        Debug.debugMethodEnd();
        return newpos;
    }

    /**
     * transfroms a <tt>GM_Envelope</tt> to the target crs of the 
     * <tt>GeoTransformer</tt> instance
     *
     * @param envelope 
     * @param sourceCRS CRS of the envelope
     *
     * @return 
     *
     * @throws Exception 
     */
    public GM_Envelope transformEnvelope( GM_Envelope envelope, String sourceCRS )
                                  throws Exception {
        Debug.debugMethodBegin( this, "transformPositions" );

        CoordinateSystem cs = csFactory.getCSByName( sourceCRS );

        envelope = transformEnvelope( envelope, cs );

        Debug.debugMethodEnd();
        return envelope;
    }

    /**
     * transfroms a <tt>GM_Envelope</tt> to the target crs of the 
     * <tt>GeoTransformer</tt> instance
     *
     * @param envelope 
     * @param sourceCRS CRS of the envelope 
     *
     * @return 
     *
     * @throws Exception 
     */
    public GM_Envelope transformEnvelope( GM_Envelope envelope, CoordinateSystem sourceCRS )
                                  throws Exception {
        Debug.debugMethodBegin( this, "transformPositions" );

        CS_CoordinateSystem cs = org.deegree_impl.model.cs.Adapters.getDefault()
                                                                   .export( sourceCRS );
        envelope = transformEnvelope( envelope, cs );

        Debug.debugMethodEnd();
        return envelope;
    }

    /**
     * transfroms a <tt>GM_Envelope</tt> to the target crs of the 
     * <tt>GeoTransformer</tt> instance
     *
     * @param envelope 
     * @param sourceCRS CRS of the envelope 
     *
     * @return 
     *
     * @throws Exception 
     */
    public GM_Envelope transformEnvelope( GM_Envelope envelope, CS_CoordinateSystem sourceCRS )
                                  throws Exception {
        Debug.debugMethodBegin( this, "transformPositions" );
        
        GM_Point min = GeometryFactory.createGM_Point( envelope.getMin().getX(), envelope.getMin().getY(), 
                                               sourceCRS );
        GM_Point max = GeometryFactory.createGM_Point( envelope.getMax().getX(), envelope.getMax().getY(), 
                                               sourceCRS );
        min = (GM_Point)transform( min );
        max = (GM_Point)transform( max );
        // create bounding box with coordinates in the EPSG:4326 reference system
        envelope = GeometryFactory.createGM_Envelope( min.getX(), min.getY(), max.getX(), max.getY() );

        Debug.debugMethodEnd();
        return envelope;
    }
}
