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

import org.deegree.model.coverage.Format;
import org.deegree.model.coverage.GridCoverageLayer;
import org.deegree.model.coverage.GridExtentDescription;
import org.deegree.model.coverage.RangeSetDescription;
import org.deegree.model.geometry.GM_Envelope;

/**
 * defines the additional fields for a <tt>GridCoverageLayer</tt>.
 *
 * <p>-----------------------------------------------------------------------</p>
 *
 * @author Andreas Poth
 * @version $Revision$ $Date$
 * <p>
 */
class GridCoverageLayer_Impl extends CoverageLayer_Impl implements GridCoverageLayer  {
    
    private RangeSetDescription rangeSetDescription = null;
    
    GridCoverageLayer_Impl(String layerID, String title, String abstract_, String[] keywordList,
                       GM_Envelope latLonBoundingBox, String[] crs, String nativeCRS,
                       URL[] metadataURLs, Format[] supportedFormatList,
                       String[] supportedInterpolationList,
                       GridExtentDescription gridExtentDescription,
                       URL descriptorResource, RangeSetDescription rangeSetDescription)
    {
        super( layerID, title, abstract_, keywordList, latLonBoundingBox, crs, 
               nativeCRS, metadataURLs, supportedFormatList, supportedInterpolationList,
               gridExtentDescription, descriptorResource);
        this.rangeSetDescription = rangeSetDescription;
    }
    
    GridCoverageLayer_Impl(String layerID, String title, String abstract_, String[] keywordList,
                       GM_Envelope latLonBoundingBox, String[] queryCRS, String[] responseCRS, 
                       String nativeCRS, URL[] metadataURLs, Format[] supportedFormatList,
                       String[] supportedInterpolationList,
                       GridExtentDescription gridExtentDescription,
                       RangeSetDescription rangeSetDescription)
    {
        super( layerID, title, abstract_, keywordList, latLonBoundingBox,  
               queryCRS, responseCRS, nativeCRS, metadataURLs, supportedFormatList,
               supportedInterpolationList, gridExtentDescription);
        this.rangeSetDescription = rangeSetDescription;
    }
    
    /** returns the range description for a <tt>GridCoverageLayer</tt>
     *
     */
    public RangeSetDescription getRangeSetDescription() {
        return rangeSetDescription;
    }    
    
    
}
