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

import org.deegree.model.geometry.*;


/**
 * 
 *
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 */
class LinearContains {
    /**
     * the operations returns true if two the submitted points contains
     */
    public static boolean contains( GM_Position point1, GM_Position point2 ) throws Exception {
        throw new NoSuchMethodException( "contains(GM_Position, GM_Position)" + 
                                         " not supported at the moment." );
    }

    /**
     * the operations returns true if the submitted point contains
     * the submitted curve segment
     */
    public static boolean contains( GM_CurveSegment curve, GM_Position point ) throws Exception {
        throw new NoSuchMethodException( "contains(GM_CurveSegment, GM_Position)" + 
                                         " not supported at the moment." );
    }

    /**
     * the operation returns true if the submitted point contains
     * the submitted surface patch
     */
    public static boolean contains( GM_SurfacePatch surface, GM_Position point ) {
        boolean con = false;
        GM_Position[] ex = surface.getExteriorRing();
        con = contains( ex, point );

        if ( con ) {
            GM_Position[][] inner = surface.getInteriorRings();

            if ( inner != null ) {
                for ( int i = 0; i < inner.length; i++ ) {
                    if ( contains( inner[i], point ) ) {
                        con = false;
                        break;
                    }
                }
            }
        }

        return con;
    }

    /**
     * the operation returns true if the two submitted curves segments contains
     */
    public static boolean contains( GM_CurveSegment curve1, GM_CurveSegment curve2 )
                     throws Exception {
        throw new NoSuchMethodException( "contains(GM_CurveSegment, GM_CurveSegment)" + 
                                         " not supported at the moment." );
    }

    /**
     * the operation returns true if the submitted curve segment contains
     * the submitted surface patch
     */
    public static boolean contains( GM_SurfacePatch surface, GM_CurveSegment curve ) {
        boolean con = true;
        GM_Position[] ex = surface.getExteriorRing();
        GM_Position[] cu = curve.getPositions();

        for ( int i = 0; i < cu.length; i++ ) {
            if ( !contains( ex, cu[i] ) ) {
                con = false;
                break;
            }
        }

        if ( con ) {
            GM_Position[][] inner = surface.getInteriorRings();

            if ( inner != null ) {
                for ( int i = 0; i < inner.length; i++ ) {
                    for ( int j = 0; j < cu.length; j++ ) {
                        if ( contains( inner[i], cu[j] ) ) {
                            con = false;
                            break;
                        }
                    }

                    if ( !con ) {
                        break;
                    }
                }
            }
        }

        return con;
    }

    /**
     * the operation returns true if the first surface patches contains
     * the second one
     */
    public static boolean contains( GM_SurfacePatch surface1, GM_SurfacePatch surface2 ) {
        boolean con = true;
        GM_Position[] ex = surface1.getExteriorRing();
        GM_Position[] ex_ = surface2.getExteriorRing();

        for ( int i = 0; i < ex_.length; i++ ) {
            if ( !contains( ex, ex_[i] ) ) {
                con = false;
                break;
            }
        }

        if ( con ) {
            GM_Position[][] inner = surface1.getInteriorRings();
            GM_Position[][] inner_ = surface2.getInteriorRings();

            if ( inner != null ) {
                for ( int i = 0; i < inner.length; i++ ) {
                    // a point of the second exterior is not allowed to be
                    // within a inner ring of the first
                    for ( int j = 0; j < ex_.length; j++ ) {
                        if ( contains( inner[i], ex_[j] ) ) {
                            con = false;
                            break;
                        }
                    }

                    if ( !con ) {
                        break;
                    }

                    // a point of the inner rings of the second is not allowed
                    // to be within a inner ring of the first
                    if ( inner_ != null ) {
                        for ( int k = 0; k < inner_.length; k++ ) {
                            for ( int j = 0; j < inner_[k].length; j++ ) {
                                if ( contains( inner[i], inner_[k][j] ) ) {
                                    con = false;
                                    break;
                                }
                            }

                            if ( !con ) {
                                break;
                            }
                        }
                    }

                    // a point of the inner rings of the first is not allowed
                    // to be within the second surface
                    for ( int j = 0; j < inner[i].length; j++ ) {
                        if ( contains( surface2, inner[i][j] ) ) {
                            con = false;
                            break;
                        }
                    }

                    if ( !con ) {
                        break;
                    }
                }
            }
        }

        // surface2 is not allowed to contain one point of surface1
        if ( con ) {
            for ( int i = 0; i < ex.length; i++ ) {
                if ( contains( surface2, ex[i] ) ) {
                    con = false;
                    break;
                }
            }
        }

        return con;
    }

    /**
     * the operations returns true if two the submitted points contains
     */
    public static boolean contains( GM_Point point1, GM_Point point2 ) throws Exception {
        throw new NoSuchMethodException( "contains(GM_Point, GM_Point)" + 
                                         " not supported at the moment." );
    }

    /**
     * the operations returns true if the submitted point contains
     * the submitted curve
     */
    public static boolean contains( GM_Curve curve, GM_Point point ) throws Exception {
        throw new NoSuchMethodException( "contains(GM_Curve, GM_Point)" + 
                                         " not supported at the moment." );
    }

    /**
     * the operation returns true if the submitted point contains
     * the submitted surface
     */
    public static boolean contains( GM_Surface surface, GM_Point point ) throws Exception {
        boolean contain = false;
        int cnt = surface.getNumberOfSurfacePatches();

        for ( int i = 0; i < cnt; i++ ) {
            if ( contains( surface.getSurfacePatchAt( i ), point.getPosition() ) ) {
                contain = true;
                break;
            }
        }

        return contain;
    }

    /**
     * the operation returns true if the two submitted curves contains
     */
    public static boolean contains( GM_Curve curve1, GM_Curve curve2 ) throws Exception {
        throw new NoSuchMethodException( "contains(GM_Curve, GM_Curve)" + 
                                         " not supported at the moment." );
    }

    /**
     * Convenience method to extract all <tt>GM_Position</tt>s from a
     * <tt>GM_Curve</tt>.
     */
    private static GM_Position[] getPositions( GM_Curve curve ) throws GM_Exception {
        ArrayList positions = new ArrayList(1000);

        for ( int i = 0; i < curve.getNumberOfCurveSegments(); i++ ) {
            GM_CurveSegment segment = curve.getCurveSegmentAt( i );
            GM_Position[] segmentPos = segment.getPositions();

            for ( int j = 0; j < segmentPos.length; j++ )
                positions.add( segmentPos[j] );
        }

        return (GM_Position[])positions.toArray();
    }

    /**
     * the operation returns true if the submitted curve contains
     * the submitted surface
     */
    public static boolean contains( GM_Surface surface, GM_Curve curve ) throws GM_Exception {
        // gather the positions of the crings (exterior and interior) and 
        // the curve as arrays of GM_Positions
        GM_SurfaceBoundary boundary = (GM_SurfaceBoundary)surface.getBoundary();
        GM_Ring extRing = boundary.getExteriorRing();
        GM_Ring[] intRings = boundary.getInteriorRings();

        GM_Position[] curvePos = getPositions( curve );
        GM_Position[] extRingPos = extRing.getPositions();
        GM_Position[][] intRingsPos = new GM_Position[intRings.length][];

        for ( int i = 0; i < intRings.length; i++ )
            intRingsPos[i] = intRings[i].getPositions();

        // necessary condition: all points of the curve have to be inside
        // of the surface's exterior ring and none must be inside of one
        // of the interior rings
        for ( int i = 0; i < curvePos.length; i++ ) {
            if ( !contains( extRingPos, curvePos[i] ) ) {
                return false;
            }

            for ( int j = 0; j < intRings.length; j++ ) {
                if ( contains( intRingsPos[j], curvePos[i] ) ) {
                    return false;
                }
            }
        }

        return true;
    }

    /**
     * the operation returns true if the two submitted surfaces contains
     */
    public static boolean contains( GM_Surface surface2, GM_Surface surface1 ) throws Exception {
        return contains( surface2.getSurfacePatchAt( 0 ), surface1.getSurfacePatchAt( 0 ) );
    }

    /**
     * the operation returns true if polygon defined by an array of GM_Position
     * contains the submitted point.
     */
    protected static boolean contains( GM_Position[] positions, GM_Position point ) {
        if ( positions.length <= 2 ) {
            return false;
        }

        int hits = 0;

        double lastx = positions[positions.length - 1].getX();
        double lasty = positions[positions.length - 1].getY();
        double curx;
        double cury;

        // Walk the edges of the polygon
        for ( int i = 0; i < positions.length; lastx = curx, lasty = cury, i++ ) {
            curx = positions[i].getX();
            cury = positions[i].getY();

            if ( cury == lasty ) {
                continue;
            }

            double leftx;

            if ( curx < lastx ) {
                if ( point.getX() >= lastx ) {
                    continue;
                }

                leftx = curx;
            } else {
                if ( point.getX() >= curx ) {
                    continue;
                }

                leftx = lastx;
            }

            double test1;
            double test2;

            if ( cury < lasty ) {
                if ( ( point.getY() < cury ) || ( point.getY() >= lasty ) ) {
                    continue;
                }

                if ( point.getX() < leftx ) {
                    hits++;
                    continue;
                }

                test1 = point.getX() - curx;
                test2 = point.getY() - cury;
            } else {
                if ( ( point.getY() < lasty ) || ( point.getY() >= cury ) ) {
                    continue;
                }

                if ( point.getX() < leftx ) {
                    hits++;
                    continue;
                }

                test1 = point.getX() - lastx;
                test2 = point.getY() - lasty;
            }

            if ( test1 < ( test2 / ( lasty - cury ) * ( lastx - curx ) ) ) {
                hits++;
            }
        }

        return ( ( hits & 1 ) != 0 );
    }
}