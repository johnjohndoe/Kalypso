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
package org.deegree.model.coverage;

import java.net.URL;
import java.util.ArrayList;
import org.deegree.tools.ParameterList;

/**
 *
 * Interface for describing a coverage layer with a multi level resolution splitted
 * into several <tt>Tile</tt>s.
 *
 * <p>----------------------------------------------------------------------</p>
 *
 * @author Andreas Poth
 * @version $Revision$ $Date$
 * <p>
 */
public interface CVDescriptor {
    
    /**
     * returns a description of the coverage layer as defined in the capabilities
     * section of the OGC WCS sppecification 0.7
     */
    CoverageLayer getCoverageLayer();
    
    /**
     * returns the resource where to access a preview of the coverage
     */
    URL getPreviewResource();
    
    /**
     * returns the top <tt>Level</tt> of the Layer
     */
    Level getLevel();
    
    /**
     * returns a list of properties assigned to the <tt>Level</tt>. The properties
     * may be overwritten by the embeded <tt>Level</tt>s.
     */
    ParameterList getProperties();

    /**
     * @return a list of the first-level ETJRanges.
	 * @author ETj
     */
	ArrayList getRanges();
	
}

