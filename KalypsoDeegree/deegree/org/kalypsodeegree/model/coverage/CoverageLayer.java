/*----------------    FILE HEADER  ------------------------------------------
 
This file is part of deegree (Java Framework for Geospatial Solutions).
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


/**
 * In its Capabilities XML response, a Web Coverage Server announces that it can
 * return coverages (that is, values or properties of spatio-temporal locations)
 * culled from various collections of data. Each logical collection from which
 * coverages may be requested is called a coverage layer. The different kinds of
 * coverage layers available all share several common elements, which comprise
 * the CoverageLayer.
 *
 * <p>-----------------------------------------------------------------------</p>
 *
 * @author Andreas Poth
 * @version $Revision$ $Date$
 * <p>
 */
public interface CoverageLayer extends Layer {
    
    /**
     * returns the DomainSetExtentDescription of the <tt>CoverageLayer</tt>. The
     * <tt>DomainSetExtentDescription</tt> is contrected by for each coverage type.
     */
    DomainSetExtentDescription getDomainSetExtentDescription();
    
    /**
     * returns a list of  supported interpolations that states whether and how 
     * the server will interpolate a CoverageLayer’s values over the spatial 
     * domain when a request requires resampling, reprojection, or other 
     * generalization. The optional, repeatable InterpolationMethod sub-element 
     * may have one of 8 different values: “nearest neighbor”, “linear”, 
     * “bilinear”, “bicubic”, “lost area”, “barycentric”, “piecewise constant”, 
     * and “none”.
     */
    String[] getSupportedInterpolationList();
    
    /**
     * returns the URL where to access the descriptor file for this layer
     */
    URL getDescriptorResource();
}
