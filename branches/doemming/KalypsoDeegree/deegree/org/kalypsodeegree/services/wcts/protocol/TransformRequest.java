/*----------------    FILE HEADER  ------------------------------------------

This file is part of Deegree.
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

Markus Mueller
Department of Geography
University of Bonn
Meckenheimer Allee 166
53115 Bonn
Germany
E-Mail: mm@giub.uni-bonn.de

 ---------------------------------------------------------------------------*/

package org.deegree.services.wcts.protocol;

import org.opengis.cs.*;

import org.deegree.gml.*;
import org.deegree.services.*;


/**
 * <p>The transform-schema describes the only mandatory request, which is to
 * adapt from the Transformation-Web-Service
 * <p>The basic elememts of the reqest are the coordinate-system of the data
 * (SourceCRS), the coordinate-system in which the data should be transformed
 * (DestinationCRS) and the data itself. Additional to this the format of the
 * incoming data and the results are declared.
 * <p>The determination of a transformation-sequence is optional. Normally
 * several single-transformations are needed for the transformation of
 * coordinates from one system into another. With the &lt;TransformationSequence&gt;-
 * element it is possible to define (a) in which order (b) which transformations
 * should be executed.
 * <p>It is to notice, that the transformationservice is based on the OpenGIS
 * implemantation specification &quot;Coordinate Transformation Service 1.0.0&quot;.
 * I. e. that the transformation of a geometry results from the transformation of
 * its particular coordinates. Splines and other not-linear interpolations get
 * not explicit transformed. So, for the the incoming data only point-geometries
 * and linear interpolated geometries are acceptable. In addition to the
 * GML-coded geometries a coding of &quot;Well Known Text&quot; (WKT) is possible.
 * A WKT-geometry is transported as a string.
 * <p>----------------------------------------------------------------------</p>
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version 2002-07-19
 */

public interface TransformRequest extends OGCWebServiceRequest {
	
	/**
     * gets the version-attribute.
     */
	public String getVersion();
	
	/**
     * gets InputFormat-element
     */
    public String getInputFormat();
    
    /**
     * gets OutputFormat-element
     */
    public String getOutputFormat();
	
	/**
     * gets SourceCRS-element.
     * Unique description of the Source coordinate-system
     */
    public CS_CoordinateSystem getSourceCRS();
    
    /**
     * gets DestinationCRS-element.
     * Unique description of the Destination coordinate-system
     */
    public CS_CoordinateSystem getDestinationCRS();
    
    /**
     * gets the TransformationSequence-element.
     * Sequence of the transformation steps. Each transformation step 
     * is to be described completely by means of XML.
     */
    public TransformationSequence[] getTransformationSequence();
    
    /**
     * gets the Data
     */
    public GMLGeometry[] getGeometries();
	
}