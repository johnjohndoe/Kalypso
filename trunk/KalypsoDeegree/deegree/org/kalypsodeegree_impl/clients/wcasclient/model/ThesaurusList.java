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
package org.deegree_impl.clients.wcasclient.model;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;


/**
 * 
 *
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 */
public class ThesaurusList {
    
    private ArrayList list = null;
    
    /** Creates a new instance of EntryList 
     *
     */
    public ThesaurusList() {
        this( 100 );
    }

    /**
     * Creates a new CatalogEntryList object with an initial size
     *
     * @param initialSize 
     */
    public ThesaurusList(int initialSize) {
        list = new ArrayList( initialSize );
    }

    /**
     * adds an thesaurus entry to the list
     *
     * @param entry a thesaurus term
     */
    public void addEntry( ThesaurusEntry entry ) {
        list.add( entry );
    }

    

    /**
     * returns all base MD_Metadata objects
     *
     * @return all base MD_Metadata objects
     */
    public ThesaurusEntry[] getAll() {
        return (ThesaurusEntry[])list.toArray( new ThesaurusEntry[ list.size() ] );
    }

    /**
     * returns all base MD_Metadata objects sorted by their names
     *
     * @return 
     */
    public ThesaurusEntry[] getAllSortedByName() {
        ThesaurusEntry[] entries = getAll();
        Arrays.sort( entries, new ComparatorImpl() );

        return entries;
    }


    /**
     * 
     *
     * @version $Revision$
     * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
     */
    protected class ComparatorImpl implements Comparator {
        /**
         *
         *
         * @param o1 
         * @param o2 
         *
         * @return 
         */
        public int compare( Object o1, Object o2 ) {
            String s1 = ( (ThesaurusEntry)o1 ).getTerm();
            String s2 = ( (ThesaurusEntry)o2 ).getTerm();
            String st1 = ( (ThesaurusEntry)o1 ).getThesaurus();
            String st2 = ( (ThesaurusEntry)o2 ).getThesaurus();
            int c = st1.compareTo( st2 );
            if ( c == 0 ) {
                return s1.compareTo( s2 );
            } else {
                return c;
            }
        }
    }
}