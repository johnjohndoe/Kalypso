/*----------------    FILE HEADER  ------------------------------------------

This file is part of deegree.
Copyright (C) 2001 by:
EXSE, Department of Geography, University of Bonn
http://www.giub.uni-bonn.de/exse/
lat/lon Fitzke/Fretter/Poth GbR
http://www.lat-lon.de

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General 
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General  License for more details.

You should have received a copy of the GNU Lesser General 
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
package org.deegree.model.feature;

import org.deegree.model.geometry.*;

/**
* Features are, according to the Abstract Specification, digital representations
* of real world entities. Feature Identity thus refers to mechanisms to identify
* such representations: not to identify the real world entities that are the
* subject of a representation. Thus two different representations of a real world
* entity (say the Mississippi River) will be two different features with distinct
* identities. Real world identification systems, such as title numbers, while
* possibly forming a sound basis for an implementation of a feature identity
* mechanism, are not of themselves such a mechanism.
*
* <p>-----------------------------------------------------------------------</p>
*
* @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
* @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider</a>
* @version $Revision$ $Date$
*/
public interface Feature {
    
    /**
     * returns the id of the Feature. the id has to be a name space
     * that must be unique for each feature. use the adress of the
     * datasource in addition to a number for example .
     */ 
     String getId();

    /**
     * returns the FeatureType of this Feature
     */
     FeatureType getFeatureType();

   /**
    * returns the properties of the feature as array of Objects
    */
     Object[] getProperties();

    /**
     * returns the property of the feature that matches the submitted name
     */
     Object getProperty(String name);
    
    /**
     * returns the property of the feature that matches the submitted index
     */
     Object getProperty(int index);
    
   /**
    * returns all geometry properties of the feature. If no geometry could 
    * be found an <tt>GM_Object[]</tt> with zero length will be returned.
    */
     GM_Object[] getGeometryProperties();

    /**
     * Returns the default geometry of the <tt>Feature</tt>.
     * @return default geometry or null, if the <tt>Feature</tt> has none
     */
     GM_Object getDefaultGeometryProperty ();
    
   /**
    * sets the value for the submitted property.
    * if no property with the submitted exists the property
    * will be added
    */
     void setProperty(FeatureProperty property);    
     
     /**
      * returns the envelope / boundingbox of the feature
      *
      * @return
      */
     GM_Envelope getEnvelope();

}
