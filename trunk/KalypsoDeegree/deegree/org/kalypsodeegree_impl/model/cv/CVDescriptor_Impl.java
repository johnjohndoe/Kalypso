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
package org.deegree_impl.model.cv;

import java.net.URL;
import java.util.ArrayList;
import org.deegree.model.coverage.CVDescriptor;
import org.deegree.model.coverage.CoverageLayer;
import org.deegree.model.coverage.Level;
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
class CVDescriptor_Impl implements CVDescriptor {
    
    private CoverageLayer coverageLayer = null;
    private URL previewResource         = null;
    private Level level                 = null;
    private ParameterList parameterList = null;
	private ArrayList ranges 			= null;
    
    CVDescriptor_Impl(CoverageLayer coverageLayer, URL previewResource,
                      Level level, ArrayList ranges, ParameterList parameterList) {
                     
        this.coverageLayer 		= coverageLayer;
        this.previewResource 	= previewResource;
        this.level 				= level;
        this.parameterList 		= parameterList;
		this.ranges 			= ranges;
    }
    
    /**
     * returns a description of the coverage layer as defined in the capabilities
     * section of the OGC WCS sppecification 0.7
     */
    public CoverageLayer getCoverageLayer()
    {
        return coverageLayer;
    }
    
    /**
     * returns the resource where to access a preview of the coverage
     */
    public URL getPreviewResource()
    {
        return previewResource;
    }
    
    /**
     * returns the top <tt>Level</tt> of the Layer
     */
    public Level getLevel()
    {
        return level;
    }
    
    /** returns a list of properties assigned to the <tt>Level</tt>. The properties
     * may be overwritten by the embeded <tt>Level</tt>s.
     *
     */
    public ParameterList getProperties() {
        return parameterList;
    }

	
	/**
	 * @return   an ArrayList of Range
	 * @author ETj
	 */
	public ArrayList getRanges() {
		return ranges;
	}
    
}

