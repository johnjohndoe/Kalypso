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
package org.deegree.services.wfs.filterencoding;

import org.deegree.model.feature.Feature;

/**
 * Abstract superclass representing expr-entities (as defined in the Expression DTD). 
 * @author Markus Schneider
 * @version 06.08.2002
 */
public interface Expression {
  
    /** Returns the name of the expression. */
    String getExpressionName ();
 
    /**
     * Returns the expression's id.
     */
    int getExpressionId ();

    /**
     * Calculates the <tt>Expression</tt>'s value based on the certain property
     * values of the given feature.
     * <p>
     * @param feature that determines the values of <tt>PropertyNames</tt>
     *                in the expression
     * @return the resulting Object
     * @throws FilterEvaluationException if the evaluation fails
     */
    public Object evaluate (Feature feature) throws FilterEvaluationException;
    
    /** Produces an indented XML representation of this object. */
    StringBuffer toXML ();
}
