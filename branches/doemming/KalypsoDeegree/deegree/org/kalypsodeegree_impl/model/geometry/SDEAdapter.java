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

import java.util.ArrayList;

import org.deegree.model.geometry.GM_Curve;
import org.deegree.model.geometry.GM_Exception;
import org.deegree.model.geometry.GM_MultiCurve;
import org.deegree.model.geometry.GM_MultiPoint;
import org.deegree.model.geometry.GM_MultiSurface;
import org.deegree.model.geometry.GM_Object;
import org.deegree.model.geometry.GM_Point;
import org.deegree.model.geometry.GM_Position;
import org.deegree.model.geometry.GM_Ring;
import org.deegree.model.geometry.GM_Surface;
import org.deegree.model.geometry.GM_SurfaceBoundary;
import org.deegree_impl.tools.Debug;

import com.esri.sde.sdk.client.SDEPoint;
import com.esri.sde.sdk.client.SeCoordinateReference;
import com.esri.sde.sdk.client.SeException;
import com.esri.sde.sdk.client.SeShape;


/**
 * Adapter class for exporting deegree geometries to WKT and to wrap WKT code
 * geometries to deegree geometries.
 *
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 */
public class SDEAdapter {

    /**
             
     *
     * @param shape
     * @return the corresponding <tt>GM_Object</tt>
     * @throws GM_Exception if type unsupported or conversion failed
     */
    public static GM_Object wrap( SeShape shape ) throws GM_Exception, SeException {
        Debug.debugMethodBegin( );

        GM_Object geo = null;

        if ( shape == null ) {
            return null;
        }

        switch ( shape.getType() ) {
            case SeShape.TYPE_POINT: {
                geo = wrapPoint( shape );
                break;
            }
            case SeShape.TYPE_SIMPLE_LINE:
            case SeShape.TYPE_LINE: {
                geo = wrapCurve( shape );
                break;
            }
            case SeShape.TYPE_POLYGON: {
                geo = wrapSurface( shape );
                break;
            }
            case SeShape.TYPE_MULTI_POINT: {
                geo = wrapMultiPoint( shape );
                break;
            }
            case SeShape.TYPE_MULTI_SIMPLE_LINE:
            case SeShape.TYPE_MULTI_LINE: {
                geo = wrapMultiCurve( shape );
                break;
            }
            case SeShape.TYPE_MULTI_POLYGON: {
                geo = wrapMultiSurface( shape );
                break;
            }
        }

        Debug.debugMethodEnd();
        return geo;
    }

    /**
     * @param geom geometry
     *
     * @return 
     */
    public static SeShape export( GM_Object geom, SeCoordinateReference crs ) throws GM_Exception, SeException {
        Debug.debugMethodBegin( );

        SeShape sb = null;

        if ( geom instanceof GM_Point ) {
            sb = export( (GM_Point)geom, crs );
        } else if ( geom instanceof GM_Curve ) {
            sb = export( (GM_Curve)geom, crs );
        } else if ( geom instanceof GM_Surface ) {
            sb = export( (GM_Surface)geom, crs );
        } else if ( geom instanceof GM_MultiPoint ) {
            sb = export( (GM_MultiPoint)geom, crs );
        } else if ( geom instanceof GM_MultiCurve ) {
            sb = export( (GM_MultiCurve)geom, crs );
        } else if ( geom instanceof GM_MultiSurface ) {
            sb = export( (GM_MultiSurface)geom, crs );
        }

        Debug.debugMethodEnd();

        return sb;
    }

    /**
     * @param point point geometry
     *
     * @return 
     */
    private static SeShape export( GM_Point point, SeCoordinateReference crs  ) throws SeException {
        Debug.debugMethodBegin( );
        
        SDEPoint pt = new SDEPoint( point.getX(), point.getY() );
        SeShape shp = new SeShape( crs );
        shp.generatePoint( 1, new SDEPoint[] { pt } );
        
        Debug.debugMethodEnd();

        return shp;
    }

    /**
     *
     * @param cur curve geometry
     *
     * @return 
     *
     * @throws GM_Exception 
     */
    private static SeShape export( GM_Curve cur, SeCoordinateReference crs  ) throws GM_Exception, SeException {
        Debug.debugMethodBegin( );
        
        GM_Position[] pos = cur.getAsLineString().getPositions();
        SDEPoint[] ptArray = new SDEPoint[pos.length];

        for ( int i = 0; i < pos.length; i++ ) {
            ptArray[i] = new SDEPoint( pos[i].getX(), pos[i].getY() );
        }
        int numParts = 1;
        int[] partOffSets = new int[numParts];
        partOffSets[0] = 0;

        SeShape line = new SeShape( crs );
        line.generateSimpleLine( pos.length, numParts, partOffSets, ptArray );

        Debug.debugMethodEnd();

        return line;
    }

    /**
     * 
     *
     * @param sur
     *
     * @return 
     *
     * @throws SeException 
     */
    private static SeShape export( GM_Surface sur, SeCoordinateReference crs  ) throws SeException {
        Debug.debugMethodBegin( );
        
        
        
        int numParts = 1;
        GM_SurfaceBoundary sbo= sur.getSurfaceBoundary();
        GM_Ring ex = sbo.getExteriorRing();
        GM_Ring[] rings = sbo.getInteriorRings();
         
        int[] partOffsets = new int[numParts];
        partOffsets[0] = 0;
        int numPts = sbo.getExteriorRing().getPositions().length;                
        if ( rings != null ) {  
            for (int i = 0; i < rings.length; i++) {
                numPts += rings[i].getPositions().length;
            }
        }        
        
        SDEPoint[] ptArray = new SDEPoint[numPts];

        int cnt = 0;
        for (int i = 0; i < ex.getPositions().length; i++) {
            ptArray[cnt++] = new SDEPoint( ex.getPositions()[i].getX(), 
                                           ex.getPositions()[i].getY() );
        }
        
        if ( rings != null ) {  
            for (int k = 0; k < numParts; k++) {
                for (int i = 0; i < rings[k].getPositions().length; i++) {
                    ptArray[cnt++] = new SDEPoint( rings[k].getPositions()[i].getX(), 
                                                   rings[k].getPositions()[i].getY() );
                }
            }
        }
     
        SeShape polygon = new SeShape( crs );
        polygon.generatePolygon( numPts, numParts, partOffsets, ptArray );

        Debug.debugMethodEnd();

        return polygon;
    }

    
    /**
     * @param mp
     * @param crs
     * @return
     * @throws SeException
     */
    private static SeShape export( GM_MultiPoint mp, SeCoordinateReference crs  ) throws SeException {
        Debug.debugMethodBegin( );
        
        SDEPoint[] pt = new SDEPoint[ mp.getSize() ];
        
        for (int i = 0; i < pt.length; i++) {
            pt[i] = new SDEPoint( mp.getPointAt(i).getX(), mp.getPointAt(i).getY() );
        }
        SeShape shp = new SeShape( crs );
        shp.generatePoint( pt.length, pt );

        Debug.debugMethodEnd();

        return shp;
    }

    /**
     *
     *
     * @param mc
     *
     * @return 
     *
     * @throws GM_Exception 
     */
    private static SeShape export( GM_MultiCurve mc, SeCoordinateReference crs  ) throws GM_Exception, SeException {
        Debug.debugMethodBegin( );
              
        int numParts = mc.getSize();
        int[] partOffSets = new int[numParts];
        int numPts = 0;
        for (int i = 0; i < numParts; i++) {
            partOffSets[i] = numPts;
            numPts += mc.getCurveAt( i ).getAsLineString().getNumberOfPoints();
        } 
        SDEPoint[] ptArray = new SDEPoint[numPts];
        int cnt = 0;
        for (int k = 0; k < numParts; k++) {
            GM_Position[] pos = mc.getCurveAt( k ).getAsLineString().getPositions();
            for ( int i = 0; i < pos.length; i++ ) {
                ptArray[cnt++] = new SDEPoint( pos[i].getX(), pos[i].getY() );
            }
        }

        SeShape line = new SeShape( crs );
        line.generateSimpleLine( numPts, numParts, partOffSets, ptArray );

        Debug.debugMethodEnd();

        return line;
    }

    /**
     *
     *     
     * @param ms
     *
     * @return 
     *
     * @throws SeException 
     */
    private static SeShape export( GM_MultiSurface ms, SeCoordinateReference crs ) throws SeException {
        Debug.debugMethodBegin( );
        
        int numParts = ms.getSize();
        int[] partOffSets = new int[numParts];
        int numPts = 0;
        for (int i = 0; i < numParts; i++) {
            partOffSets[i] = numPts;
            GM_SurfaceBoundary sbo = ms.getSurfaceAt( i ).getSurfaceBoundary();
            GM_Ring ex = sbo.getExteriorRing();
            GM_Ring[] inner = sbo.getInteriorRings();
            numPts += ex.getPositions().length;
            if ( inner != null ) {
                for (int j = 0; j < inner.length; j++) {
                    numPts += inner[j].getPositions().length;
                }
            }
        } 
        SDEPoint[] ptArray = new SDEPoint[numPts];
        int cnt = 0;
        for (int k = 0; k < numParts; k++) {
            GM_SurfaceBoundary sbo = ms.getSurfaceAt( k ).getSurfaceBoundary();
            GM_Ring ex = sbo.getExteriorRing();
            GM_Ring[] inner = sbo.getInteriorRings();
            GM_Position[] pos = ex.getPositions();
            for ( int i = 0; i < pos.length; i++ ) {
                ptArray[cnt++] = new SDEPoint( pos[i].getX(), pos[i].getY() );
            }
            if ( inner != null ) {
                for (int j = 0; j < inner.length; j++) {
                    pos = inner[j].getPositions();
                    for ( int i = 0; i < pos.length; i++ ) {
                        ptArray[cnt++] = new SDEPoint( pos[i].getX(), pos[i].getY() );
                    }
                }
            }
        }
        
        SeShape polygon = new SeShape( crs );
        polygon.generatePolygon( numPts, numParts, partOffSets, ptArray );

        Debug.debugMethodEnd();

        return polygon;
    }

    /**
     * creates a GM_Point from a SeShape
     *
     * @param shape
     */
    private static GM_Point wrapPoint( SeShape shape ) throws GM_Exception, SeException {
        Debug.debugMethodBegin( );

        ArrayList al = shape.getAllPoints( SeShape.TURN_DEFAULT, true );
        // Retrieve the array of SDEPoints
        SDEPoint[] points = (SDEPoint[])al.get( 0 );

        GM_Point point = GeometryFactory.createGM_Point( points[0].getX(), points[0].getY(), null );

        Debug.debugMethodEnd();
        return point;
    }

    /**
     * creates a GM_Curve from a SeShape
     *
     * @param shape
     */
    private static GM_Curve wrapCurve( SeShape shape ) throws GM_Exception, SeException {
        Debug.debugMethodBegin();

        ArrayList al = shape.getAllPoints( SeShape.TURN_DEFAULT, true );
        // Retrieve the array of SDEPoints
        SDEPoint[] points = (SDEPoint[])al.get( 0 );
//        // Retrieve the part offsets array.
//        int[] partOffset = (int[])al.get( 1 );
//        // Retrieve the sub-part offsets array.
//        int[] subPartOffset = (int[])al.get( 2 );

        int numPoints = shape.getNumOfPoints();

        GM_Position[] gmSimpleLinePosition = new GM_Position[numPoints];

        for ( int pt = 0; pt < numPoints; pt++ ) {
            gmSimpleLinePosition[pt] = GeometryFactory.createGM_Position( points[pt].getX(), 
                                                                  points[pt].getY() );
        }

        GM_Curve curve = GeometryFactory.createGM_Curve( gmSimpleLinePosition, null );

        Debug.debugMethodEnd();
        return curve;
    }

    /**
     * creates a GM_Surface
     *
     * @param shape
     */
    private static GM_Surface wrapSurface( SeShape shape ) throws GM_Exception, SeException {
        Debug.debugMethodBegin();

        ArrayList al = shape.getAllPoints( SeShape.TURN_DEFAULT, true );
        // Retrieve the array of SDEPoints
        SDEPoint[] points = (SDEPoint[])al.get( 0 );
        // Retrieve the part offsets array.
//        int[] partOffset = (int[])al.get( 1 );
        // Retrieve the sub-part offsets array.
        int[] subPartOffset = (int[])al.get( 2 );

        int numSubParts = shape.getNumSubParts( 1 );
        //System.out.println("\tIt has " + (numSubParts-1) + " sub-parts");
        GM_Position[] gmPolygonExteriorRing = new GM_Position[shape.getNumPoints( 1, 1 )];

        for ( int pt = 0; pt < shape.getNumPoints( 1, 1 ); pt++ ) {
            gmPolygonExteriorRing[pt] = GeometryFactory.createGM_Position( points[pt].getX(), 
                                                                   points[pt].getY() );
        }

        GM_Position[][] gmPolygonInteriorRings = null;

        // if it is a donut create inner rings
        if ( numSubParts > 1 ) {
            gmPolygonInteriorRings = new GM_Position[numSubParts - 1][];

            int j = 0;

            for ( int subPartNo = 1; subPartNo < numSubParts; subPartNo++ ) {
                int lastPoint = shape.getNumPoints( 1, subPartNo + 1 ) + 
                                subPartOffset[subPartNo];
                GM_Position[] gmPolygonPosition = new GM_Position[shape.getNumPoints( 1, 
                                                                                      subPartNo + 1 )];
                int i = 0;

                for ( int pt = subPartOffset[subPartNo]; pt < lastPoint; pt++ ) {
                    gmPolygonPosition[i] = GeometryFactory.createGM_Position( points[pt].getX(), 
                                                                      points[pt].getY() );
                    i++;
                }

                gmPolygonInteriorRings[j] = gmPolygonPosition;
                j++;
            }
        }

        GM_Surface sur = GeometryFactory.createGM_Surface( gmPolygonExteriorRing, gmPolygonInteriorRings, 
                                                   new GM_SurfaceInterpolation_Impl(), null );

        Debug.debugMethodEnd();
        return sur;
    }

    /**
     * creates a GM_MultiPoint from a WKT
     *
     * @param shape
     */
    private static GM_MultiPoint wrapMultiPoint( SeShape shape ) throws GM_Exception, SeException {
        Debug.debugMethodBegin();

        ArrayList al = shape.getAllPoints( SeShape.TURN_DEFAULT, true );
        // Retrieve the array of SDEPoints
        SDEPoint[] points = (SDEPoint[])al.get( 0 );

        int numPoints = shape.getNumOfPoints();

        GM_Point[] gmPoints = new GM_Point[numPoints];

        for ( int pt = 0; pt < numPoints; pt++ ) {
            gmPoints[pt] = GeometryFactory.createGM_Point( points[pt].getX(), points[pt].getY(), null );
        }

        GM_MultiPoint gmMultiPoint = GeometryFactory.createGM_MultiPoint( gmPoints );

        Debug.debugMethodEnd();
        return gmMultiPoint;
    }

    /**
     * creates a GM_MultiCurve from a WKT
     *
     * @param shape
     */
    private static GM_MultiCurve wrapMultiCurve( SeShape shape ) throws GM_Exception, SeException {
        Debug.debugMethodBegin();

        ArrayList al = shape.getAllPoints( SeShape.TURN_DEFAULT, true );
        // Retrieve the array of SDEPoints
        SDEPoint[] points = (SDEPoint[])al.get( 0 );

        // Retrieve the part offsets array.
        int[] partOffset = (int[])al.get( 1 );

        int numParts = shape.getNumParts();

        GM_Curve[] gmCurves = new GM_Curve[numParts];

        for ( int partNo = 0; partNo < numParts; partNo++ ) {
            int lastPoint = shape.getNumPoints( partNo + 1, 1 ) + partOffset[partNo];
            GM_Position[] gmMultiSimpleLinePosition = new GM_Position[shape.getNumPoints( partNo + 1, 1 )];
            int i = 0;

            for ( int pt = partOffset[partNo]; pt < lastPoint; pt++ ) {
                //System.out.println("X: " + points[pt].getX() + "\tY: " + points[pt].getY() );
                gmMultiSimpleLinePosition[i] = GeometryFactory.createGM_Position( points[pt].getX(), 
                                                                          points[pt].getY() );
                i++;
            }

            gmCurves[partNo] = GeometryFactory.createGM_Curve( gmMultiSimpleLinePosition, null );
        }

        GM_MultiCurve gmMultiCurve = GeometryFactory.createGM_MultiCurve( gmCurves );

        Debug.debugMethodEnd();
        return gmMultiCurve;
    }

    /**
     * creates a GM_MultiSurface from a WKT
     *
     * @param shape
     */
    private static GM_MultiSurface wrapMultiSurface( SeShape shape ) throws GM_Exception, SeException {
        Debug.debugMethodBegin();

        ArrayList al = shape.getAllPoints( SeShape.TURN_DEFAULT, true );
        // Retrieve the array of SDEPoints
        SDEPoint[] points = (SDEPoint[])al.get( 0 );
        // Retrieve the part offsets array.
        int[] partOffset = (int[])al.get( 1 );
        // Retrieve the sub-part offsets array.
        int[] subPartOffset = (int[])al.get( 2 );

        int numParts = shape.getNumParts();

        GM_Surface[] gmMultiPolygonSurface = new GM_Surface[numParts];
        boolean subParts = false;

        if ( partOffset.length < subPartOffset.length ) {
            subParts = true;
        }

        for ( int partNo = 0, partEnd = 0; partNo < partOffset.length; partNo++ ) {
            GM_Position[] gmMultiPolygonExteriorRing = new GM_Position[shape.getNumPoints( partNo + 1, 1 )];
            GM_Position[][] gmMultiPolygonInteriorRings = null;
            int nSubParts = shape.getNumSubParts( partNo + 1 );

            if ( nSubParts > 1 ) {
                gmMultiPolygonInteriorRings = new GM_Position[( nSubParts - 1 )][];
            }

            if ( ( partOffset.length - partNo ) == 1 ) {
                partEnd = points.length; //If this is the last part, scan through to points.length
            } else {
                partEnd = subPartOffset[partOffset[partNo + 1]]; //Otherwise scan to the offset of next part
            }

            int subPartNo = partOffset[partNo];
            int pointNo = subPartOffset[partOffset[partNo]];
            boolean exterior = true;
            int i = 0;
            int subPartIndex = -1;

            for ( ; ( pointNo < points.length ) && ( pointNo < partEnd ); pointNo++ ) {
                if ( subParts ) {
                    if ( ( subPartNo < subPartOffset.length ) && 
                             ( pointNo == subPartOffset[subPartNo] ) ) {
                        subPartNo++;
                        i = 0;
                    }
                }

                if ( exterior ) {
                    gmMultiPolygonExteriorRing[i] = GeometryFactory.createGM_Position( 
                                                            points[pointNo].getX(), 
                                                            points[pointNo].getY() );

                    i++;

                    if ( ( subPartNo < subPartOffset.length ) && 
                             ( pointNo == ( subPartOffset[subPartNo] - 1 ) ) ) {
                        exterior = false;
                    }
                } else {
                    // When i=0 we are starting a new subPart. I compute 
                    // and assign the size of the second dimension of gmMultiPolygonInteriorRings
                    if ( i == 0 ) {
                        subPartIndex++; //Used to address each interior ring

                        gmMultiPolygonInteriorRings[subPartIndex] = new GM_Position[subPartOffset[subPartNo] - 
                                                                    subPartOffset[subPartNo - 1]];
                    }

                    gmMultiPolygonInteriorRings[subPartIndex][i] = GeometryFactory.createGM_Position( 
                                                                           points[pointNo].getX(), 
                                                                           points[pointNo].getY() );

                    i++;
                }
            } // End for

            gmMultiPolygonSurface[partNo] = GeometryFactory.createGM_Surface( gmMultiPolygonExteriorRing, 
                                                                      gmMultiPolygonInteriorRings, 
                                                                      new GM_SurfaceInterpolation_Impl(), 
                                                                      null );
        } // End for

        GM_MultiSurface gmMultiSurface = GeometryFactory.createGM_MultiSurface( gmMultiPolygonSurface );

        Debug.debugMethodEnd();
        return gmMultiSurface;
    }
}