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
package org.deegree_impl.graphics.sld;

import org.deegree.xml.DOMPrinter;
import org.deegree.xml.Marshallable;
import org.deegree_impl.tools.Debug;
import org.deegree.gml.GMLGeometry;
import org.deegree_impl.gml.GMLGeometry_Impl;
import org.deegree.graphics.sld.*;


/**
 * The Geometry element is optional and if it is absent then the default geometry
 * property of the feature type that is used in the  containing FeatureStyleType
 * is used. The precise meaning of default geometry property is system-dependent.
 * Most frequently, feature types will have only a single geometry property.
 * <p>----------------------------------------------------------------------</p>
 *
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp</a>
 * @version $Revision$ $Date$
 */
class Geometry_Impl implements Geometry, Marshallable {
    private GMLGeometry geometryAsGML = null;
    private String propertyName = null;

    /**
     * constructor initializing the class with the <Geometry>
     */
    Geometry_Impl( String propertyName, GMLGeometry geometryAsGML ) {
        setPropertyName( propertyName );
        setGeometryAsGML( geometryAsGML );
    }

    /**
     * returns the name of the geometry property
     * @return the name of the geometry property
     */
    public String getPropertyName() {
        return propertyName;
    }

    /**
     * sets the name of the geometry property
     * @param propertyName the name of the geometry property
     */
    public void setPropertyName( String propertyName ) {
        this.propertyName = propertyName;
    }

    /**
     * In principle, a fixed geometry could be defined using GML or operators
     * could be defined for computing a geometry from references or literals.
     * This enbales the calling client to submitt the geometry to be rendered
     * by the WMS directly. (This is not part of the SLD XML-schema)
     * @return the GMLGeometry 
     */
    public GMLGeometry getGeometryAsGML() {
        return geometryAsGML;
    }

    /**
     * sets the <GMLGeometry>
     * @param geometryAsGML the GMLGeometry
     */
    public void setGeometryAsGML( GMLGeometry geometryAsGML ) {
        this.geometryAsGML = geometryAsGML;
    }
    
    /**
     * exports the content of the Geometry as XML formated String
     *
     * @return xml representation of the Geometry
     */
    public String exportAsXML() {
        Debug.debugMethodBegin();
        
        StringBuffer sb = new StringBuffer(1000);
        sb.append( "<Geometry>" );
        if ( propertyName != null && !propertyName.equals( "" ) ) {
            sb.append( "<ogc:PropertyName>" ).append( propertyName );
            sb.append( "</ogc:PropertyName>" );
        } else {
            String s = DOMPrinter.nodeToString( ((GMLGeometry_Impl)geometryAsGML).getAsElement(), "UTF-8" );
            sb.append( s );
        }
        
        sb.append( "</Geometry>" );
        
        Debug.debugMethodEnd();
        return sb.toString();
    }
}