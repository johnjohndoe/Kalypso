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

import org.deegree.services.wfs.filterencoding.*;

/**
 * A FeatureTypeConstraint element is used to identify a feature type by
 * well-known name, using the FeatureTypeName element.
 * <p>----------------------------------------------------------------------</p>
 *
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version $Revision$ $Date$
 */
public interface FeatureTypeConstraint {

   /**
    * returns the name of the feature type
    * @return the name of the feature type
    */
    String getFeatureTypeName();
    
   /**
    * sets the name of the feature type
    * @param featureTypeName the name of the feature type
    */
    void setFeatureTypeName(String featureTypeName);
    
   /**
    * returns a feature-filter as defined in WFS specifications.
    * @see org.deegree.sevices.wfs.filterencoding.Filter
    * @return the filter of the FeatureTypeConstraints
    */
    Filter getFilter();
    
   /**
    * sets a feature-filter as defined in WFS specifications.
    * @see org.deegree.sevices.wfs.filterencoding.Filter
    * @param filter the filter of the FeatureTypeConstraints
    */
    void setFilter(Filter filter);

   /**
    * returns the extent for filtering the feature type
    * @return the extent for filtering the feature type
    */
    Extent[] getExtents();
    
   /**
    * sets the extent for filtering the feature type
    * @param extents extents for filtering the feature type     
    */
    void setExtents(Extent[] extents);
    
    /**
    * adds an extent 
    * @param extent an extent to add     
    */
    void addExtent(Extent extent);
    
   /**
    * removes an extent 
    * @param extent an extent to remove     
    */
    void removeExtent(Extent extent);
}
