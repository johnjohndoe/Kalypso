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
package org.deegree_impl.io.shpapi;

import org.deegree.model.geometry.ByteUtils;
import org.deegree.model.geometry.GM_Curve;
import org.deegree.model.geometry.GM_Position;
import org.deegree.model.geometry.GM_Surface;
import org.deegree_impl.model.cs.Adapters;
import org.deegree_impl.model.cs.ConvenienceCSFactory;
import org.deegree_impl.model.cs.CoordinateSystem;
import org.opengis.cs.CS_CoordinateSystem;


/**
 * 
 *
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 */
public class SHP2WKB {
    /**
     * constructor:<BR>
     */
    public SHP2WKB() {
    }

    /**
     * method: byte[] transformPoint(CS_CoordinateSystem srs,<BR> 
     *                               SHPPoint shppoint))<BR>
     * transforms a SHPPoint into a byte array using sf-WKB specifications<BR>
     * gets a point that should be transformed to a WKBGeometry<BR>
     */
    public byte[] transformPoint( SHPPoint shppoint_ ) {
        byte[] wkbPoint = new byte[21];

        // big endian coding
        wkbPoint[0] = 0;

        // write wkbtype 
        ByteUtils.writeBEInt( wkbPoint, 1, 1 );

        // write point coordinates
        ByteUtils.writeBEDouble( wkbPoint, 5, shppoint_.x );
        ByteUtils.writeBEDouble( wkbPoint, 13, shppoint_.y );

        return wkbPoint;
    }

    /**
     * method: byte[] transformMultiPoint(CS_CoordinateSystem srs,<BR> 
     *                                    SHPMultiPoint shpmultipoint))<BR>
     * transforms a SHPMultiPoint into a byte array using sf-WKB specifications<BR>
     * gets a multipoint that should be transformed to a WKBGeometry<BR>
     */
    public byte[] transformMultiPoint( SHPMultiPoint shpmultipoint_ ) {
        byte[] wkbPoint = new byte[9 + ( shpmultipoint_.numPoints * 21 )];

        // big endian coding
        wkbPoint[0] = 0;

        // write wkbtype 
        ByteUtils.writeBEInt( wkbPoint, 1, 1 );

        // write number of points 
        ByteUtils.writeBEInt( wkbPoint, 5, shpmultipoint_.numPoints );

        int offset = 9;

        for ( int i = 0; i < shpmultipoint_.numPoints; i++ ) {
            // big endian coding
            wkbPoint[offset] = 0;

            // write wkbtype 
            ByteUtils.writeBEInt( wkbPoint, offset + 1, 1 );

            // write point coordinates
            ByteUtils.writeBEDouble( wkbPoint, offset + 5, shpmultipoint_.points[i].x );
            ByteUtils.writeBEDouble( wkbPoint, offset + 13, shpmultipoint_.points[i].y );

            // increment offset with the size of a WKBPoint
            offset += 21;
        }

        return wkbPoint;
    }

    /**
     * method: byte[] transformPolyLine(CS_CoordinateSystem srs,<BR> 
     *                                      SHPPolyLine shppolyline))<BR>
     * transforms a SHPPolyLine into a byte array using sf-WKB specifications<BR>
     * gets a multipoint that should be transformed<BR>
     */
    public byte[] transformPolyLine( SHPPolyLine shppolyline ) {
        byte[] wkbLineString = null;
        int offset = 0;

        SHP2WKS shp2wks = new SHP2WKS();
        CoordinateSystem cs = ConvenienceCSFactory.getInstance().getCSByName( "EPSG:4326" );        
        CS_CoordinateSystem srs = Adapters.getDefault().export( cs );
        GM_Curve[] points = shp2wks.transformPolyLine( srs, shppolyline );

        /*
                // it's a single LineString
                if (points.length == 1) {
        
                    wkbLineString = new byte[points[0].length*21 + 9];
        
                    // big endian coding
                    wkbLineString[0] = 0;
        
                    // write wkbtype 
                    ByteUtils.writeBEInt(wkbLineString,1,2);
        
                    // write number of points 
                    ByteUtils.writeBEInt(wkbLineString,5,points[0].length);
        
                    offset = 9;
        
                    for (int i = 0; i < points[0].length; i++) {
        
                        // big endian coding
                        wkbLineString[offset] = 0;
                        offset++;
        
                        // write wkbtype 
                        ByteUtils.writeBEInt(wkbLineString,offset + 1,1);
                        offset += 4;
        
                        // write point coordinates
                        ByteUtils.writeBEDouble(wkbLineString,offset + 5 ,points[0][i].getX());
                        offset += 8;
                        ByteUtils.writeBEDouble(wkbLineString,offset + 13,points[0][i].getY());
                        offset += 8;                    
        
                    }
        
                }
        
                // it's a multi LineString
               else {
        
                    // get size to be allocated form wkbstructure
                    int size = 9;
                
                    for (int j = 0; j < points.length; j++) size += points[j].length*21 + 9;
        
                    wkbLineString = new byte[size];
        
                    // big endian coding
                    wkbLineString[0] = 0;
        
                    // write wkbtype 
                    ByteUtils.writeBEInt(wkbLineString,1,5);
        
                    // write number of linestrings 
                    ByteUtils.writeBEInt(wkbLineString,5,points.length);
        
                    offset = 9;
                
                    // for every linestring
                    for (int j = 0; j < points.length; j++) {
        
                        // big endian coding
                        wkbLineString[offset] = 0;
        
                        // write wkbtype 
                        ByteUtils.writeBEInt(wkbLineString,offset + 1,2);
                
                        // write number of points 
                        ByteUtils.writeBEInt(wkbLineString,offset + 5,points[j].length);
        
                        offset += 9;
        
                        for (int i = 0; i < points[j].length; i++) {
        
                            // big endian coding
                            wkbLineString[offset] = 0;
        
                            // write wkbtype 
                            ByteUtils.writeBEInt(wkbLineString,offset + 1,1);
        
                            // write point coordinates
                            ByteUtils.writeBEDouble(wkbLineString,offset + 5 ,points[j][i].getX());
                            ByteUtils.writeBEDouble(wkbLineString,offset + 13,points[j][i].getY());
        
                            // increment offset with the size of a WKBPoint
                            offset += 21;
        
                        }
        
                    }
        
                }
        */
        return wkbLineString;
    }

    /**
     * method: byte[] transformPolygon(CS_CoordinateSystem srs,<BR> 
     *                                 SHPPolygon shppolygon))<BR>
     * transforms the SHPPolygon into a byte array using sf-WKB specifications<BR>
     * gets the polygon that should be transformed to a WKSGeometry<BR>
     */
    public byte[] transformPolygon( SHPPolygon shppolygon_ ) throws Exception {
        int wkbtype = 0;
        int N = 0;

        SHP2WKS shp2wks = new SHP2WKS();
        CoordinateSystem cs = ConvenienceCSFactory.getInstance().getCSByName( "EPSG:4326" );        
        CS_CoordinateSystem srs = Adapters.getDefault().export( cs );
        GM_Surface[] wkslp = shp2wks.transformPolygon( srs, shppolygon_ );

        byte[] buffer = null;

        /*
         * calculate buffer size
         */

        // byte order (1 byte)
        int bufsize = 1;

        // wkb type (4 bytes)
        bufsize += 4;

        // if a multipolygon calculate required memory for every polygon
        if ( wkslp.length > 1 ) {
            wkbtype = 6;

            // number of polygons (4 bytes)
            bufsize += 4;

            for ( int i = 0; i < wkslp.length; i++ ) {
                // byte order (1 byte) (of the i-th polygon)
                bufsize += 1;

                // wkb type (4 bytes) (of the i-th polygon)
                bufsize += 4;

                // number of rings (4 bytes) (of the i-th polygon)
                bufsize += 4;

                // number of points of the external boundary 
                bufsize += 4;

                // add 16 byte (2*double) for every point of the external boundary 
                bufsize += ( wkslp[i].getSurfacePatchAt( 0 ).getExteriorRing().length * 16 );

                if ( wkslp[i].getSurfacePatchAt( 0 ).getInteriorRings() != null ) {
                    for ( int j = 0;
                          j < wkslp[i].getSurfacePatchAt( 0 ).getInteriorRings().length;
                          j++ ) {
                        // number of points of the j-th internal boundary 
                        bufsize += 4;

                        // add 16 byte (2*double) for every point of the j-th internal boundary 
                        bufsize += ( wkslp[i].getSurfacePatchAt( 0 ).getInteriorRings()[j].length * 16 );
                    }
                }
            }
        }// if a single polygon 
        else {
            wkbtype = 3;

            // number of rings (4 bytes)
            bufsize += 4;

            // number of points of the external boundary 
            bufsize += 4;

            // add 16 byte (2*double) for every point of the external boundary 
            bufsize += ( wkslp[0].getSurfacePatchAt( 0 ).getExteriorRing().length * 16 );

            if ( wkslp[0].getSurfacePatchAt( 0 ).getInteriorRings() != null ) {
                for ( int j = 0; j < wkslp[0].getSurfacePatchAt( 0 ).getInteriorRings().length; j++ ) {
                    // number of points of the j-th internal boundary 
                    bufsize += 4;

                    // add 16 byte (2*double) for every point of every internal boundary 
                    bufsize += ( wkslp[0].getSurfacePatchAt( 0 ).getInteriorRings()[j].length * 16 );
                }
            }
        }

        // allocate memory for buffering the geometry
        buffer = new byte[bufsize];

        // big endian wkb type
        buffer[0] = 0;

        int offset = 1;

        // write wkb type to buffer
        ByteUtils.writeBEInt( buffer, offset, wkbtype );
        offset += 4;

        // a multipolygon
        if ( wkbtype == 6 ) {
            // write number of polygons to buffer
            ByteUtils.writeBEInt( buffer, offset, wkslp.length );
            offset += 4;

            for ( int i = 0; i < wkslp.length; i++ ) {
                // big endian wkb type
                buffer[offset] = 0;
                ++offset;

                // write wkb type (polygon = 3) to buffer
                ByteUtils.writeBEInt( buffer, offset, 3 );
                offset += 4;

                // write number of rings to buffer
                if ( wkslp[i].getSurfacePatchAt( 0 ).getInteriorRings() != null ) {
                    N = wkslp[i].getSurfacePatchAt( 0 ).getInteriorRings().length;
                } else {
                    N = 0;
                }

                ByteUtils.writeBEInt( buffer, offset, N + 1 );
                offset += 4;

                // write number of points of the external boundary to buffer
                ByteUtils.writeBEInt( buffer, offset, 
                                      wkslp[i].getSurfacePatchAt( 0 ).getExteriorRing().length );
                offset += 4;

                GM_Position[] ls = wkslp[i].getSurfacePatchAt( 0 ).getExteriorRing();

                // write points of the external boundary to buffer
                for ( int j = 0; j < ls.length; j++ ) {
                    ByteUtils.writeBEDouble( buffer, offset, ls[j].getX() );
                    offset += 8;

                    ByteUtils.writeBEDouble( buffer, offset, ls[j].getY() );
                    offset += 8;
                }

                if ( wkslp[i].getSurfacePatchAt( 0 ).getInteriorRings() != null ) {
                    GM_Position[][] gcu = wkslp[i].getSurfacePatchAt( 0 ).getInteriorRings();

                    // for every internal boundary of the i-th polygon
                    for ( int j = 0; j < gcu.length; j++ ) {
                        // write number of points of the j-th internal boundary to buffer
                        ByteUtils.writeBEInt( buffer, offset, gcu[j].length );
                        offset += 4;

                        // write points of the j-th internal boundary to buffer
                        for ( int k = 0; k < gcu[j].length; k++ ) {
                            ByteUtils.writeBEDouble( buffer, offset, gcu[j][k].getX() );
                            offset += 8;

                            ByteUtils.writeBEDouble( buffer, offset, gcu[j][k].getY() );
                            offset += 8;
                        }
                    }
                }
            }
        } else {
            if ( wkslp[0].getSurfacePatchAt( 0 ).getInteriorRings() != null ) {
                N = wkslp[0].getSurfacePatchAt( 0 ).getInteriorRings().length;
            } else {
                N = 0;
            }

            // write number of rings to buffer
            ByteUtils.writeBEInt( buffer, offset, N + 1 );
            offset += 4;

            // write number of points of the external boundary to buffer
            GM_Position[] ls = wkslp[0].getSurfacePatchAt( 0 ).getExteriorRing();
            ByteUtils.writeBEInt( buffer, offset, ls.length );
            offset += 4;

            // write points of the external boundary to buffer
            for ( int j = 0; j < ls.length; j++ ) {
                ByteUtils.writeBEDouble( buffer, offset, ls[j].getX() );
                offset += 8;

                ByteUtils.writeBEDouble( buffer, offset, ls[j].getY() );
                offset += 8;
            }

            if ( wkslp[0].getSurfacePatchAt( 0 ).getInteriorRings() != null ) {
                GM_Position[][] gcu = wkslp[0].getSurfacePatchAt( 0 ).getInteriorRings();

                // for every internal boundary
                for ( int j = 0; j < gcu.length; j++ ) {
                    // write number of points of the j-th internal boundary to buffer
                    ByteUtils.writeBEInt( buffer, offset, gcu[j].length );
                    offset += 4;

                    // write points of the j-th internal boundary to buffer
                    for ( int k = 0; k < gcu[j].length; k++ ) {
                        ByteUtils.writeBEDouble( buffer, offset, gcu[j][k].getX() );
                        offset += 8;

                        ByteUtils.writeBEDouble( buffer, offset, gcu[j][k].getY() );
                        offset += 8;
                    }
                }
            }
        }

        return buffer;
    }
} // end of class SHP2WKB
