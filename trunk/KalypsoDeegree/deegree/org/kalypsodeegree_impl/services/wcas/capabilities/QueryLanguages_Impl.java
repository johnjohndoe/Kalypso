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

import java.util.ArrayList;

import org.deegree.services.wcas.capabilities.QueryLanguages;


/**
 * 
 * <p>------------------------------------------------------------</p>
 *
 * @author <a href="mailto:uzs6tr@uni-bonn.de">Axel Schaefer</a>
 * @version $Revision$ $Date$
 */

final class QueryLanguages_Impl implements QueryLanguages
{
    private ArrayList queryLanguages = null;
    
    /** Creates a new instance of QueryLanguages_Impl */
    QueryLanguages_Impl(String[] queryLanguages)
    {
        this.queryLanguages = new ArrayList();
        setQueryLanguages( queryLanguages );
    }
    
    /** returns the name of all query languages known by a catalog
     * @return names of all query languages known by a catalog
     *
     */
    public String[] getQueryLanguages()
    {
        String[] tmp = new String[ queryLanguages.size() ];
        return (String[])queryLanguages.toArray(tmp);
    }
    
    /**
     * sets the query languages known by a catalog
     * @param queryLanguages list of query languages to add
     */
    public void setQueryLanguages(String[] queryLanguages)
    {
        this.queryLanguages.clear();
        if ( queryLanguages != null ) {
            for (int i = 0; i < queryLanguages.length; i++) {
                addQueryLanguage( queryLanguages[i] );
            }
        }
    }
    
    /**
     * adds a query language to list of query languages known by a catalog
     * @param queryLanguage query language to add
     */
    public void addQueryLanguage(String queryLanguage)
    {
        this.queryLanguages.add( queryLanguage );
    }
    
    /** returns true if the submitted query language is known by the catalog
     * @param queryLanguage query language to be checked
     * @return true if submitted query language is known
     *
     */
    public boolean isQueryLanguageKnown(String queryLanguage)
    {
        return queryLanguages.contains( queryLanguage );
    }
    
}
