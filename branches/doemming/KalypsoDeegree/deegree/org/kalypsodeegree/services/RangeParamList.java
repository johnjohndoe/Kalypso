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
package org.deegree.services;

import java.util.Set;

import org.deegree.tools.Parameter;


/**
 * The list of ranges in a URL request.
 *
 * @author ETj
 */
public interface RangeParamList {
    /**
     * returns the parameter that matches the submitted name. if no parameter can
     * be found <tt>null</tt> will be returned.
     */
    Parameter getParameter( String name );

    /**
     * adds a new <tt>Parameter</tt> to the list
     */
    void addParameter( Parameter param );

    /**
     *
     *
     * @param name 
     * @param value 
     */
    void addParameter( String name, String value );

    /**
     *
     *
     * @return 
     */
    /**
     * returns all <tt>Parameter</tt>s contained within the list as array.
     * it is guarenteered that the arrays isn't <tt>null</tt>
     */
    Parameter[] getParameters();

    /**
     *
     *
     * @return 
     */
    Set getNames();
}