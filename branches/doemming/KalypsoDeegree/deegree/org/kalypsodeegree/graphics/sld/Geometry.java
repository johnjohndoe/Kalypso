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

import org.deegree.gml.*;

/**
 * The Geometry element is optional and if it is absent then the default geometry
 * property of the feature type that is used in the  containing FeatureStyleType
 * is used. The precise meaning of default geometry property is system-dependent.
 * Most frequently, feature types will have only a single geometry property.
 * <p>----------------------------------------------------------------------</p>
 *
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version $Revision$ $Date$
 */
public interface Geometry {

   /**
    * returns the name of the geometry property
    * @return the name of the geometry property
    */
    String getPropertyName();

   /**
    * sets the name of the geometry property
    * @param propertyName the name of the geometry property
    */
    void setPropertyName(String propertyName);
    
   /**
    * In principle, a fixed geometry could be defined using GML or operators
    * could be defined for computing a geometry from references or literals.
    * This enbales the calling client to submitt the geometry to be rendered
    * by the WMS directly. (This is not part of the SLD XML-schema)
    * @return the GMLGeometry 
    */
    GMLGeometry getGeometryAsGML();
    
   /**
    * sets the <GMLGeometry>
    * @param geometry the GMLGeometry 
    */
    void setGeometryAsGML(GMLGeometry geometry);
}
