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
package org.deegree_impl.services.wcts.protocol;

import java.util.HashMap;

import org.deegree.services.wcts.protocol.DescribeTransformationRequest;
import org.deegree_impl.services.OGCWebServiceRequest_Impl;
import org.opengis.cs.CS_CoordinateSystem;


/**
 * The only request not defined as mandatory supplies the description 
 * of the transformation of coordinates from a spatial reference system to 
 * another. The structure of the requests essentially corresponds to that of 
 * the IsTransformable Requests. The transformation steps, which 
 * are necessary, are inquired in order to transform a coordinate from the 
 * SourceCRS into the TargetCRS. 
 * <p>----------------------------------------------------------------------</p>
* @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version 2002-07-10
 */
public class DescribeTransformationRequest_Impl extends OGCWebServiceRequest_Impl
    implements DescribeTransformationRequest {
    private CS_CoordinateSystem destinationCRS = null;
    private CS_CoordinateSystem sourceCRS = null;
    private String format = null;

    /**
     * Creates a new DescribeTransformationRequest_Impl object.
     *
     * @param version 
     * @param id 
     * @param verndorSpecificParameter 
     * @param format 
     * @param sourceCRS 
     * @param destinationCRS 
     */
    DescribeTransformationRequest_Impl( String version, String id, HashMap verndorSpecificParameter, 
                                        String format, CS_CoordinateSystem sourceCRS, 
                                        CS_CoordinateSystem destinationCRS ) {
        super( "DescribeTransformation", "WCTS", version, id, verndorSpecificParameter );
        setFormat( format );
        setSourceCRS( sourceCRS );
        setDestinationCRS( destinationCRS );
    }

    /**
     * gets the format
     */
    public String getFormat() {
        return format;
    }

    /**
     * sets the format
     */
    public void setFormat( String format ) {
        this.format = format;
    }

    /**
     * gets the SourceCRS
     */
    public CS_CoordinateSystem getSourceCRS() {
        return sourceCRS;
    }

    /**
     * @see getSourceCRS
     */
    public void setSourceCRS( CS_CoordinateSystem sourceCRS ) {
        this.sourceCRS = sourceCRS;
    }

    /**
     * gets the DestinationCRS
     */
    public CS_CoordinateSystem getDestinationCRS() {
        return destinationCRS;
    }

    /**
     * @see getDestinationCRS
     */
    public void setDestinationCRS( CS_CoordinateSystem destinationCRS ) {
        this.destinationCRS = destinationCRS;
    }
}