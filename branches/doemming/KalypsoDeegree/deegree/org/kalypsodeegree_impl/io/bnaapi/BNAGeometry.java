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
import org.deegree.model.geometry.GM_Position;

import org.deegree_impl.model.geometry.GeometryFactory;


/**
 * A BNA geometry is a point or an ordered sequence of points. Points are stored as GM_Positions.
 * Ordered sequence can be a ring if the last point has the same coords of the first one.
 * If a ring has _bLine = false, then it's a surface, else it's a polygon.
 * <p>
 * @version 2003.08.04
 * @author Emanuele Tajariol
 */
public class BNAGeometry {

    /** The sequence of points*/
    private Vector _points;

    /** Tells if it's a polygon or a surface */
    private boolean _bLine = false;

    /* The bounding box of this geometry */
    private double _maxX = 0;
    private double _maxY = 0;

    /* The bounding box of this geometry */
    private double _minX = 0;
    private double _minY = 0;

    /**
     * Constructor
     *
     * @param    isLine  a  boolean telling if this is a line or a surface.
     */
    protected BNAGeometry( boolean isLine ) {
        _points = new Vector();
        _bLine = isLine;
    }

    /**
     * Construct a surface or a point
     */
    protected BNAGeometry() {
        this( false );
    }

    /** @return the number of point of this geometry*/
    public int size() {
        return _points.size();
    }

    /** @return the index-th point*/
    public GM_Position getPoint( int index ) {
        return (GM_Position)_points.get( index );
    }

    /**
     * @return   this geometry as a GM_Position[]
     */
    public GM_Position[] getPoints() {
        return (GM_Position[])_points.toArray( new GM_Position[0] );
    }

    /** Adds a new point and recompute the envelope */
    public void addPoint( GM_Position point ) {
        _points.add( point );

        if ( point.getX() < _minX ) {
            _minX = point.getX();
        }

        if ( point.getX() > _maxX ) {
            _maxX = point.getX();
        }

        if ( point.getY() < _minY ) {
            _minY = point.getY();
        }

        if ( point.getY() > _maxY ) {
            _maxY = point.getY();
        }
    }

    /**
     *
     *
     * @return 
     */
    public GM_Envelope getEnvelope() {
        return GeometryFactory.createGM_Envelope( _minX, _minY, _maxX, _maxY );
    }

    /**
     * @return   a boolean telling if this geometry is a surface.
     */
    public boolean isSurface() {
        return ( _points.size() > 1 ) && !_bLine;
    }

    /**
     * @return   a boolean telling if this geometry is a point.
     */
    public boolean isPoint() {
        return _points.size() == 1;
    }

    /**
     * @return   a boolean telling if this geometry is a closed line.
     */
    public boolean isPoly() {
        return ( _points.size() > 1 ) && _bLine && 
               ( (GM_Position)_points.get( 0 ) ).equals( 
                       _points.get( _points.size() - 1 ) );
    }

    /**
     * @return   a boolean telling if this geometry is an open line.
     */
    public boolean isLine() {
        return ( _points.size() > 1 ) && _bLine && 
               !( (GM_Position)_points.get( 0 ) ).equals( 
                        _points.get( _points.size() - 1 ) );
    }
}