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

copyright Emanuele Tajariol / itworks
                 
 ---------------------------------------------------------------------------*/
package org.deegree.services;



/**
 * DoubleExtent implements the ISO extended time format
 * The syntax for expressing Time constraints and date / time values is
 * specified in WMS 1.1 Annexes C
 *
 * @author Emanuele Tajariol
 */
public interface DoubleExtent extends Extent {
    
    /** Used for single valued extents*/
    public Double getSingleValue();
    
    /** Used for listed extents*/
    public Double getListItem( int i );

    /** Used for periodic extents*/
    public Double getLower();

    /** Used for periodic extents*/
    public Double getHigher();

    /** Used for periodic extents*/
    public Double getPeriod();
}