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

import java.util.ArrayList;

import org.deegree.services.OGCWebServiceRequest;
import org.deegree.services.wcts.protocol.DescribeTransformationResponse;
import org.deegree_impl.services.OGCWebServiceResponse_Impl;
import org.opengis.ct.CT_MathTransform;
import org.w3c.dom.Document;


/**
* <p> As an answer to a DescribeTransformationRequest the service answers with one 
* list including all single transformations which can be accomplished, in 
* order to convert a coordinate from a sourcecoordinatensystem into a 
* target system. As the attribute the number of necessary transformations is 
* returned. If the transformation described is not 
* possible, no transformation is returned and the numberOfTransformations is ' 0 '.
* Reasons for the missing possibility to perform a transformation can be that 
* the service does not know/implement the necessary transformation steps, or that a 
* transformation not is not possible in principle (e.g. 3d to 2d systems). An 
* exception is returned only if an error aroses during processing the request .
* </p>
* <p>----------------------------------------------------------------------</p>
* @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
* @version 2002-07-10
*/
public class DescribeTransformationResponse_Impl extends OGCWebServiceResponse_Impl
    implements DescribeTransformationResponse {
    private ArrayList parameterizedTransformation = null;
    private int numberOfTransformations = 0;

    /**
     * constructor initializing the class 
     */
    DescribeTransformationResponse_Impl( OGCWebServiceRequest request, Document exception, 
                                         CT_MathTransform[] parameterizedTransformation, 
                                         int numberOfTransformations ) {
        super( request, exception );
        this.parameterizedTransformation = new ArrayList();
        setParameterizedTransformation( parameterizedTransformation );
        setNumberOfTransformations( numberOfTransformations );
    }

    /**
     * gets the ParameterizedTransformation 
     */
    public CT_MathTransform[] getParameterizedTransformation() {
        return (CT_MathTransform[])parameterizedTransformation.toArray( 
                         new CT_MathTransform[parameterizedTransformation.size()] );
    }

    /**
    * adds the ParameterizedTransformation;
    */
    public void addParameterizedTransformation( CT_MathTransform parameterizedTransformation ) {
        this.parameterizedTransformation.add( parameterizedTransformation );
    }

    /**
    * sets the &lt;VendorSpecificCapabilities&gt;
    */
    public void setParameterizedTransformation( CT_MathTransform[] parameterizedTransformation ) {
        this.parameterizedTransformation.clear();

        if ( parameterizedTransformation != null ) {
            for ( int i = 0; i < parameterizedTransformation.length; i++ ) {
                this.parameterizedTransformation.add( parameterizedTransformation[i] );
            }
        }
    }

    /**
     * gets the NumberOfTransformations
     */
    public int getNumberOfTransformations() {
        return numberOfTransformations;
    }

    /**
     * sets the NumberOfTransformations
     */
    public void setNumberOfTransformations( int numberOfTransformations ) {
        this.numberOfTransformations = numberOfTransformations;
    }

    /**
     * creates the DescribeTransformationResponse as XML
     */
    public String exportAsXML() {        
        return null;
    }


}