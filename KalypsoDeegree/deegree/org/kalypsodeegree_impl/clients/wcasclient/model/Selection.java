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

import java.util.Arrays;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Comparator;


/**
 * 
 *
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 */
public class Selection extends ModelList {
    
    /** Creates a new instance of EntryList 
     *
     */
    public Selection() {
        this( 100 );
    }

    /**
     * Creates a new CatalogEntryList object with an initial size
     *
     * @param initialSize 
     */
    public Selection(int initialSize) {
        super( initialSize );
    }

    /**
     * adds an entry (MD_Metadata) to the list
     *
     * @param entry a base MD_Metadata description
     */
    public void addEntry( SelectionEntry entry ) {
        list.put( entry.getCatalog() + ":" + entry.getId(), entry );
    }

    /**
     * returns an entry (MD_Metadata object) identified by the passed fileIdentifier
     *     
     * @param fileIdentifier 
     * @param catalog
     *
     * @return base MD_Metadata object
     */
    public SelectionEntry getSelectionEntry( String fileIdentifier, String catalog ) {
        return (SelectionEntry)getEntry( catalog + ":" + fileIdentifier );
    }
    
    /**
     * sets an entry of the selection identified by its ID and the catalog it
     * belongs to as selected or not
     *
     * @param fileIdentifier 
     * @param catalog
     * @param selected
     */
    public void setEntryToSelected(String fileIdentifier, String catalog, boolean selected) {
        SelectionEntry entry = (SelectionEntry)getEntry( catalog + ":" + fileIdentifier );
        if ( entry != null ) {
            entry.setSelected( selected );
        }
    }
    
    /**
     * marks all entries of the selection as unselected
     */
    public void unselectAll() {        
        Iterator iterator = list.values().iterator();
        while( iterator.hasNext() ) {
            SelectionEntry entry = (SelectionEntry)iterator.next();
            entry.setSelected( false );
        }
    }
    
    /**
     * returns all entries of the selection that are marked as selected
     *
     * @return all entries of the selection that are marked as selected
     */
    public SelectionEntry[] getSelectedEntries() {
        ArrayList sl = new ArrayList();
        Iterator iterator = list.values().iterator();
        while( iterator.hasNext() ) {
            SelectionEntry entry = (SelectionEntry)iterator.next();
            if ( entry.isSelected() ) {
                sl.add( entry );
            }
        }
        return (SelectionEntry[])sl.toArray( new SelectionEntry[ sl.size() ] );
    }

    /**
     * returns all base MD_Metadata objects
     *
     * @return all base MD_Metadata objects
     */
    public SelectionEntry[] getAll() {  
        return (SelectionEntry[])getAll( new SelectionEntry[list.size()] );
    }

    /**
     * returns all base MD_Metadata objects sorted by their names
     *
     * @return 
     */
    public SelectionEntry[] getAllSortedByName() {
        SelectionEntry[] entries = getAll();
        if ( entries != null && entries.length > 1 ) {
            Arrays.sort( entries, new SelComparatorImpl() );
        }

        return entries;
    }
    
    

    /**
     * removes an entry identified by its id from the list
     *
     * @param fileIdentifier
     * @param catalog
     *
     * @return 
     */
    public SelectionEntry removeSelectionEntry( String fileIdentifier, String catalog  ) {
        return (SelectionEntry)removeEntry( catalog + ":" + fileIdentifier );
    }

    /**
     * 
     *
     * @version $Revision$
     * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
     */
    protected class SelComparatorImpl implements Comparator {
        /**
         *
         *
         * @param o1 
         * @param o2 
         *
         * @return 
         */
        public int compare( Object o1, Object o2 ) {
            String s1 = ( (SelectionEntry)o1 ).getTitle();
            String s2 = ( (SelectionEntry)o2 ).getTitle();
            return s1.compareTo( s2 );
        }
    }
}