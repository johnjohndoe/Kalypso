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

import java.util.*;

import org.deegree.gml.*;
import org.deegree.services.wcts.protocol.*;

import org.deegree_impl.services.*;

import org.opengis.cs.*;


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
public class TransformRequest_Impl extends OGCWebServiceRequest_Impl implements TransformRequest {
    private ArrayList data = null;
    private ArrayList transformationSequence = null;
    private CS_CoordinateSystem destinationCRS = null;
    private CS_CoordinateSystem sourceCRS = null;
    private String inputFormat = null;
    private String outputFormat = null;

    /**
     * constructor
     */
    TransformRequest_Impl( String version, String id, HashMap vendorSpecificParameter, 
                           String inputFormat, String outputFormat, CS_CoordinateSystem sourceCRS, 
                           CS_CoordinateSystem destinationCRS, 
                           TransformationSequence[] transformationSequence, GMLGeometry[] data ) {
        super( "TransformRequest", "WCTS", version, id, vendorSpecificParameter );
        this.transformationSequence = new ArrayList();
        this.data = new ArrayList();
        setInputFormat( inputFormat );
        setOutputFormat( outputFormat );
        setSourceCRS( sourceCRS );
        setDestinationCRS( destinationCRS );
        setTransformationSequence( transformationSequence );
        setGeometry( data );
    }

    /**
     * gets InputFormat-element
     */
    public String getInputFormat() {
        return inputFormat;
    }

    /**
     * @see getInputFormat
     */
    public void setInputFormat( String inputFormat ) {
        this.inputFormat = inputFormat;
    }

    /**
     * gets OutputFormat-element
     */
    public String getOutputFormat() {
        return outputFormat;
    }

    /**
     * @see getOutputFormat
     */
    public void setOutputFormat( String outputFormat ) {
        this.outputFormat = outputFormat;
    }

    /**
     * gets SourceCRS-element.
     * Unique description of the Source coordinate-system
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
     * gets DestinationCRS-element.
     * Unique description of the Destination coordinate-system
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

    /**
     * gets the TransformationSequence-element.
     * Sequence of the transformation steps. Each transformation step 
     * is to be described completely by means of XML.
     */
    public TransformationSequence[] getTransformationSequence() {
        return (TransformationSequence[])transformationSequence.toArray( 
                         new TransformationSequence[transformationSequence.size()] );
    }

    /**
     * @see getTransformationSequence
     */
    public void addTransformationSequence( TransformationSequence transformationSequence ) {
        this.transformationSequence.add( transformationSequence );
    }

    /**
     * @see getTransformationSequence
     */
    public void setTransformationSequence( TransformationSequence[] transformationSequence ) {
        this.transformationSequence.clear();

        if ( transformationSequence != null ) {
            for ( int i = 0; i < transformationSequence.length; i++ ) {
                this.transformationSequence.add( transformationSequence[i] );
            }
        }
    }

    /**
     * gets the data
     */
    public GMLGeometry[] getGeometries() {
        return (GMLGeometry[])data.toArray( new GMLGeometry[data.size()] );
    }

    /**
     * @see getData
     */
    public void addGeometry( GMLGeometry data ) {
        this.data.add( data );
    }

    /**
     * @see getData
     */
    public void setGeometry( GMLGeometry[] data ) {
        this.data.clear();

        if ( data != null ) {
            for ( int i = 0; i < data.length; i++ ) {
                this.data.add( data[i] );
            }
        }
    }
}