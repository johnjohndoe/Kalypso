/*----------------    FILE HEADER  ------------------------------------------

This file has been provided to deegree by
Emanuele Tajariol e.tajariol@libero.it

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
package org.deegree_impl.io.bnaapi;

import java.util.Vector;

import org.deegree.model.geometry.GM_Envelope;

import org.deegree_impl.model.geometry.GeometryFactory;


/**
 * A feature read from a BNA File. It may hold more than one Geometry.
 * <p>
 * @version 2003.08.04
 * @author Emanuele Tajariol
 *
 */
public class BNAFeature {
    /** The bounding box for all the geometries of this feature*/
    private GM_Envelope _envelope = null;

    /** The BNAGeometry'es of this feature*/
    private Vector _geometries;

    /** The headers' names */
    private String[] _headers;

    /**
     * Creates a new BNAFeature object.
     *
     * @param headers 
     */
    public BNAFeature( String[] headers ) {
        _headers = headers;
        _geometries = new Vector();
    }

    /**
     * @return the i-th header as a String
     */
    public String getHeader( int i ) {
        return _headers[i];
    }

    /**
     * @return the number of headers
     */
    public int getHeaderSize() {
        return _headers.length;
    }

    /**
     * @return   the number of geometries of this Feature.
     */
    public int size() {
        return _geometries.size();
    }

    /**
     * Returns the bounding box containing all the geometris of this feature.
     * @return   a GM_Envelope that holds all the geometries of this feature.
     */
    public GM_Envelope getEnvelope() {
        return _envelope;
    }

    /**
     * Method getGeometry
     *
     * @return   the i-th geometry
     */
    public BNAGeometry getGeometry( int i ) {
        return (BNAGeometry)_geometries.get( i );
    }

    /**
     * Adds a geometry to this BNAFeature
     *
     * @param    geom                The BNAGeometry to be added
     *
     */
    public void addGeometry( BNAGeometry geom ) {
        _geometries.add( geom );

        if ( _envelope == null ) {
            _envelope = geom.getEnvelope();
        } else {
            _envelope = merge( _envelope, geom.getEnvelope() );
        }
    }

    /**
     * Merges two envelopes, computing the Envelope that contains both.
     *
     * @return   the smallest GM_Envelope that contains both input envelopes.
     */
    private static GM_Envelope merge( GM_Envelope a, GM_Envelope b ) {
        double minx = Math.min( a.getMin().getX(), b.getMin().getX() );
        double maxx = Math.max( a.getMax().getX(), b.getMax().getX() );
        double miny = Math.min( a.getMin().getY(), b.getMin().getY() );
        double maxy = Math.max( a.getMax().getY(), b.getMax().getY() );

        return GeometryFactory.createGM_Envelope( minx, miny, maxx, maxy );
    }

    /**
     *
     *
     * @return 
     */
    public String toString() {
        StringBuffer h = new StringBuffer();

        for ( int i = 0; i < _headers.length; i++ )
            h.append( "FIELD" ).append( i ).append( "='" ).append( _headers[i] ).append( "', " );

        return "BNAFeature[" + h + "#geom=" + _geometries.size() + ", env=" + "(" + 
               _envelope.getMin().getX() + "," + _envelope.getMin().getY() + ")->(" + 
               _envelope.getMax().getX() + "," + _envelope.getMax().getY() + ")" + "]";
    }

    /** @return a short identifier for this feature*/
    public String getHeads() {
        StringBuffer h = new StringBuffer( "[" );

        for ( int i = 0; i < _headers.length; i++ ) {
            h.append( "FIELD" ).append( i ).append( "='" ).append( _headers[i] ).append( "'" );

            if ( i < ( _headers.length - 1 ) ) {
                h.append( "', " );
            } else {
                h.append( "]" );
            }
        }

        return h.toString();
    }
}