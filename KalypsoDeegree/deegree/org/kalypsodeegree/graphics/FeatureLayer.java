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
package org.deegree.graphics;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureCollection;


/**
* A Layer is a collection of <tt>Feature</tt>s building a thematic
* 'unit' waterways or country borders for example. <tt>Feature</tt>s 
* can be added or removed from the layer. A <tt>Feature</tt> can e changed
* by a modul of the application using the layer because only references to
* <tt>Feature</tt>s are stored within a layer.
*
* <p>------------------------------------------------------------------------</p>
*
* @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
* @version $Revision$ $Date$
*/
public interface FeatureLayer extends Layer {
   
    /**
     * returns the feature that matches the submitted id
     */
    Feature getFeatureById( String id );

    /**
     * returns all features that matches the submitted ids
     */
    Feature[] getFeaturesById( String[] id );

    /**
     * returns the feature that matches the submitted id
     */
    Feature getFeature( int index );

    /**
     * returns all features
     */
    Feature[] getAllFeatures();

    /**
     * adds a feature to the layer
     */
    void addFeature( Feature feature ) throws Exception;

    /**
     * adds a feature collection to the layer
     */
    void addFeatureCollection( FeatureCollection featureCollection ) throws Exception;

    /**
     * removes a display Element from the layer
     */
    void removeFeature( Feature feature ) throws Exception;
    void removeFeatureCollection( FeatureCollection featureCollection ) throws Exception;

    /**
     * removes the display Element from the layer that matches the submitted id
     */
    void removeFeature( int id ) throws Exception;
    
     /**
     * returns the amount of features within the layer.
     */
    int getSize();


 
}
