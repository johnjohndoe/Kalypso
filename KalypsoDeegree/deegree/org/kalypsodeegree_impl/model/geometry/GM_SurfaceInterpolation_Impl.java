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
import org.deegree.model.geometry.GM_SurfaceInterpolation;


/**
* default implementation of the GM_SurfaceInterpolation interface from the
* package jago.model. 
*
* ------------------------------------------------------------
* @version 11.6.2001
* @author Andreas Poth
*/
public class GM_SurfaceInterpolation_Impl implements GM_SurfaceInterpolation, Serializable {
    /** Use serialVersionUID for interoperability. */
    private final static long serialVersionUID = -3728721225837686088L;
    private int surfaceInterpolation = NONE;

    /**
     * Creates a new GM_SurfaceInterpolation_Impl object.
     */
    public GM_SurfaceInterpolation_Impl() {
    }

    /**
     * Creates a new GM_SurfaceInterpolation_Impl object.
     *
     * @param surfaceInterpolation 
     *
     * @throws GM_Exception 
     */
    public GM_SurfaceInterpolation_Impl( int surfaceInterpolation ) throws GM_Exception {
        if ( ( surfaceInterpolation > TRIANGULATEDSOLINE ) || ( surfaceInterpolation < NONE ) ) {
            throw new GM_Exception( "invalid surface interpolation" );
        }
    }

    /**
     *
     *
     * @return 
     */
    public int getValue() {
        return surfaceInterpolation;
    }

    /**
    * returns a deep copy of the geometry
    */
    public Object clone() {
        GM_SurfaceInterpolation si = null;

        try {
            si = new GM_SurfaceInterpolation_Impl( getValue() );
        } catch ( Exception ex ) {
            System.out.println( "GM_SurfaceInterpolation_Impl.clone: " + ex );
        }

        return si;
    }

    /**
    * checks if this surface is completly equal to the submitted geometry.
    */
    public boolean equals( Object other ) {
        return ( other instanceof GM_SurfaceInterpolation_Impl ) && 
               ( ( (GM_SurfaceInterpolation)other ).getValue() == surfaceInterpolation );
    }
} 