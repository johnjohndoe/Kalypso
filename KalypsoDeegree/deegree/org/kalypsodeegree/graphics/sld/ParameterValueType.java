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
package org.deegree.graphics.sld;

import org.deegree.model.feature.Feature;
import org.deegree.services.wfs.filterencoding.FilterEvaluationException;

/**
 * The ParameterValueType element is a combination of text nodes and 
 * Filter-Expression element nodes.
 * <p>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider</a>
 * @version $Revision$ $Date$
 */
public interface ParameterValueType {

    /**
     * Returns the contents (mix of <tt>String</tt>/<tt>Expression</tt>-objects)
     * of this <tt>ParameterValueType</tt>.
     * <p>
     * @return mix of <tt>String</tt> and <tt>Expression</tt>-objects
     */
    Object [] getComponents ();    
    
    /**
     * Sets the contents (mix of <tt>String</tt>/<tt>Expression</tt>-objects)
     * of this <tt>ParameterValueType</tt>.
     * <p>
     * @param components mix of <tt>String</tt>/<tt>Expression</tt>-objects
     */
    void setComponents (Object [] components);
   
    /**
     * Concatenates a component (a <tt>String</tt> or an 
     * <tt>Expression</tt>-object)
     * to this <tt>ParameterValueType</tt>.
     * <p>
     * @param component either a <tt>String</tt> or an
     *        <tt>Expression</tt>-object
     */
    void addComponent (Object component); 
    
    /**
     * Removes a component (a <tt>String</tt> or an 
     * <tt>Expression</tt>-object)
     * from this <tt>ParameterValueType</tt>.
     * <p>
     * @param component either a <tt>String</tt> or an
     *        <tt>Expression</tt>-object
     */
    void removeComponent (Object component); 
    
    /**
     * Returns the actual <tt>String</tt> value of this object.
     * Expressions are evaluated according to the given
     * <tt>Feature</tt>-instance.
     * <p>
     * @param feature used for the evaluation of the underlying
     *        'wfs:Expression'-elements
     * @return the evaluated extual representation, ready to be printed
     * @throws FilterEvaluationException if the evaluation fails
     */
    String evaluate (Feature feature) throws FilterEvaluationException;
}
