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
package org.deegree_impl.services.wcas.capabilities;

import java.util.HashMap;
import java.util.Iterator;

import org.deegree.services.wcas.capabilities.TaxonomyTypeList;
import org.deegree.services.wcas.capabilities.TaxonomyType;

/**
 * 
 * 
 * <p>---------------------------------------------------------------------</p>
 *
 * @author Andreas Poth
 * @version $Revision$ $Date$
 * <p>
 */
final class TaxonomyTypeList_Impl implements TaxonomyTypeList {
    
    private HashMap taxonomyTypes = null;

    /**
     * @param taxonomyTypes
     */
    TaxonomyTypeList_Impl(TaxonomyType[] taxonomyTypes)
    {
        setTaxonomyTypes( taxonomyTypes );
    }
      
    
    /** returns a list of all taxonomies known by a catalog
     *
     */
    public TaxonomyType[] getAllTaxonomies() {
        TaxonomyType[] tt = new TaxonomyType[taxonomyTypes.size()];
        Iterator iterator = taxonomyTypes.values().iterator();
        int i =0;
        while ( iterator.hasNext() ) {
            tt[i++] = (TaxonomyType)iterator.next();
        }
        return tt;
    }    
    
    /**
     * @see TaxonomyTypeList_Impl#getAllTaxonomies()
     */
    public void setTaxonomyTypes(TaxonomyType[] taxonomyTypes)
    {
        this.taxonomyTypes.clear();
        if ( taxonomyTypes != null ) {
            for (int i = 0; i < taxonomyTypes.length; i++) {
                addTaxonomyType( taxonomyTypes[i] );
            }
        }
    }
    
    /**
     * @see TaxonomyTypeList_Impl#getTaxonomy(String)
     */
    public void addTaxonomyType(TaxonomyType taxonomyType)
    {
        taxonomyTypes.put( taxonomyType.getName(), taxonomyType );
    }
    
    /** returns one specific taxonomy identfied by its name. If no taxonomy with
     * the submitted name is known, <tt>null</tt> will be returned.
     *
     */
    public TaxonomyType getTaxonomy(String name) {
        return (TaxonomyType)taxonomyTypes.get( name );
    }    
    
}
