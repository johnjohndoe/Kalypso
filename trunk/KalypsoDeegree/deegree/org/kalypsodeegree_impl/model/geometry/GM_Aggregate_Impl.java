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

import java.util.*;

import org.deegree.model.geometry.*;
import org.deegree_impl.tools.Debug;

import org.opengis.cs.*;



/**
* default implementierung of the GM_Aggregate interface 
*
* ------------------------------------------------------------
* @version 8.6.2001
* @author Andreas Poth href="mailto:poth@lat-lon.de"
*/
abstract class GM_Aggregate_Impl extends GM_Object_Impl implements GM_Aggregate, Serializable {
    /** Use serialVersionUID for interoperability. */
    private final static long serialVersionUID = 1161164609227432958L;
    protected ArrayList aggregate = new ArrayList( 500 );
    

    /**
     * Creates a new GM_Aggregate_Impl object.
     *
     * @param crs 
     */
    public GM_Aggregate_Impl( CS_CoordinateSystem crs ) {
        super( crs );
    }

    /**
     * Creates a new GM_Aggregate_Impl object.
     */
    private GM_Aggregate_Impl() {
        super( null );
    }        

    /**
     * returns the number of GM_Object within the aggregation
     */
    public int getSize() {
        return aggregate.size();
    }

    /**
     * merges this aggregation with another one 
     *
     * @exception GM_Exception a GM_Exception will be thrown if the submitted
     *             isn't the same type as the recieving one.
     */
    public void merge( GM_Aggregate aggregate ) throws GM_Exception {
        if ( !this.getClass().getName().equals( aggregate.getClass().getName() ) ) {
            throw new GM_Exception( "Aggregations are not of the same type!" );
        }

        for ( int i = 0; i < this.getSize(); i++ ) {
            this.add( aggregate.getObjectAt( i ) );
        }

        setValid( false );
    }

    /**
     * adds an GM_Object to the aggregation 
     */
    public void add( GM_Object gmo ) {
        aggregate.add( gmo );

        setValid( false );
    }

    /**
     * inserts a GM_Object in the aggregation. all elements with an index 
     * equal or larger index will be moved. if index is
     * larger then getSize() - 1 or smaller then 0 or gmo equals null 
     * an exception will be thrown.
     *
     * @param gmo GM_Object to insert.     
     * @param index position where to insert the new GM_Object
     */
    public void insertObjectAt( GM_Object gmo, int index ) throws GM_Exception {
        if ( ( index < 0 ) || ( index > this.getSize() - 1 ) ) {
        	throw new GM_Exception( "invalid index/position: " + index +
        							" to insert a geometry!" );
        } 

        if ( gmo == null ) {
            throw new GM_Exception( "gmo == null. it isn't possible to insert a value" + 
                                    " that equals null!" );
        }

        aggregate.add( index, gmo );

        setValid( false );
    }

    /**
     * sets the submitted GM_Object at the submitted index. the element
     * at the position <code>index</code> will be removed. if index is
     * larger then getSize() - 1 or smaller then 0 or gmo equals null 
     * an exception will be thrown.
     *
     * @param gmo GM_Object to set.     
     * @param index position where to set the new GM_Object
     */
    public void setObjectAt( GM_Object gmo, int index ) throws GM_Exception {
        if ( ( index < 0 ) || ( index > this.getSize() - 1 ) ) {
        	throw new GM_Exception( "invalid index/position: " + index +
        							" to set a geometry!" );
        }

        if ( gmo == null ) {
            throw new GM_Exception( "gmo == null. it isn't possible to set a value" + 
                                    " that equals null!" );
        }

        aggregate.set( index, gmo );

        setValid( false );
    }

    /**
     * removes the submitted GM_Object from the aggregation
     *
     * @return the removed GM_Object
     */
    public GM_Object removeObject( GM_Object gmo ) {
        if ( gmo == null ) {
            return null;
        }

        int i = aggregate.indexOf( gmo );

        GM_Object gmo_ = null;

        try {
            gmo_ = removeObjectAt( i );
        } catch ( GM_Exception e ) {
            Debug.debugException( e, "" );
        }

        setValid( false );

        return gmo_;
    }

    /**
     * removes the GM_Object at the submitted index from the aggregation.
     * if index is larger then getSize() - 1 or smaller then 0 
     * an exception will be thrown.
     *
     * @return the removed GM_Object
     */
    public GM_Object removeObjectAt( int index ) throws GM_Exception {
        if ( index < 0 ) {
            return null;
        }

        if ( index > ( this.getSize() - 1 ) ) {
            throw new GM_Exception( "invalid index/position: " + index +
            						" to remove a geometry!" );
        }

        GM_Object gmo = (GM_Object)aggregate.remove( index );

        setValid( false );

        return gmo;
    }

    /**
     * removes all GM_Object from the aggregation. 
     */
    public void removeAll() {
        aggregate.clear();
        envelope = null;
        setValid( false );
    }

    /**
     * returns the GM_Object at the submitted index. if index is
     * larger then getSize() - 1 or smaller then 0 
     * an exception will be thrown.
     */
    public GM_Object getObjectAt( int index ) {
        return (GM_Object)aggregate.get( index );
    }

    /**
     * returns all GM_Objects as array
     */
    public GM_Object[] getAll() {
        GM_Object[] gmos = new GM_Object[this.getSize()];

        return (GM_Object[])aggregate.toArray( gmos );
    }

    /**
     * returns true if the submitted GM_Object is within the aggregation
     */
    public boolean isMember( GM_Object gmo ) {
        return aggregate.contains( gmo );
    }

    /**
     * returns the aggregation as an iterator
     */
    public Iterator getIterator() {
        return aggregate.iterator();
    }

    /**
     * returns true if no geometry stored
     * within the collection.
     */
    public boolean isEmpty() {
        return ( getSize() == 0 );
    }

    /**
     * sets the spatial reference system
     *
     * @param crs new spatial reference system
     */
    public void setCoordinateSystem( CS_CoordinateSystem crs ) {
        super.setCoordinateSystem( crs );

        if ( aggregate != null ) {
            for ( int i = 0; i < aggregate.size(); i++ ) {
                ( (GM_Object_Impl)getObjectAt( i ) ).setCoordinateSystem( crs );
            }
            setValid( false );
        }
    }    

    /**
     * translate the point by the submitted values. the <code>dz</code>-
     * value will be ignored.
     */
    public void translate( double[] d ) {
        try {
            for ( int i = 0; i < getSize(); i++ ) {
                GM_Object gmo = getObjectAt( i );
                gmo.translate( d );
            }
            setValid( false );
        } catch ( Exception e ) {
            Debug.debugException( e, "" );
        }
        setValid( false );
    }

    /**
     *
     *
     * @param other 
     *
     * @return 
     */
    public boolean equals( Object other ) {
      // envelope was not valid 
      if ( !super.equals( other ) || !( other instanceof GM_Aggregate_Impl ) || 
                 !getEnvelope().equals( ( (GM_Object)other ).getEnvelope() ) || 
                 ( getSize() != ( (GM_Aggregate)other ).getSize() ) ) {
            return false;
        }

        try {
            for ( int i = 0; i < getSize(); i++ ) {
                Object o1 = getObjectAt( i );
                Object o2 = ( (GM_Aggregate)other ).getObjectAt( i );

                if ( !o1.equals( o2 ) ) {
                    return false;
                }
            }
        } catch ( Exception ex ) {
            return false;
        }

        return true;
    }

    /**
     * The Boolean valued operation "intersects" shall return TRUE if this GM_Object
     * intersects another GM_Object. Within a GM_Complex, the GM_Primitives do not
     * intersect one another. In general, topologically structured data uses shared
     * geometric objects to capture intersection information.
     */
    public boolean intersects( GM_Object gmo ) {
        boolean inter = false;

        try {
            for ( int i = 0; i < aggregate.size(); i++ ) {
                if ( this.getObjectAt( i ).intersects( gmo ) ) {
                    inter = true;
                    break;
                }
            }
        } catch ( Exception e ) {
        }

        return inter;
    }

    /**
     *
     *
     * @return 
     */
    public String toString() {
        String ret = null;
        ret = "aggregate = " + aggregate + "\n";
        ret += ( "envelope = " + envelope + "\n" );
        return ret;
    }
}