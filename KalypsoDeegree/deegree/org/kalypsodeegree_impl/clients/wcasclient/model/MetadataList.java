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
import java.util.Comparator;

import org.deegree.model.geometry.GM_Envelope;


/**
 * 
 *
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 */
public class MetadataList extends ModelList {
    
    protected GM_Envelope bbox = null;
    protected String catalog = null;

    /** Creates a new instance of EntryList 
     *
     * @param bbox boundingbox that had limited the spatial extent of the
     *             request the list bases on
     * @param catalog name of the catalog the list of MD_Metadata objects came from
     */
    public MetadataList(GM_Envelope bbox, String catalog) {
        this( bbox, catalog, 100 );
    }

    /**
     * Creates a new CatalogEntryList object with an initial size
     *
     * @param initialSize 
     * @param bbox boundingbox that had limited the spatial extent of the
     *             request the list bases on
     * @param catalog name of the catalog the list of MD_Metadata objects came from
     */
    public MetadataList(GM_Envelope bbox, String catalog, int initialSize) {
        super( initialSize );
        this.bbox = bbox;
        this.catalog = catalog;
    }

    /**
     * adds an entry (MD_Metadata) to the list
     *
     * @param entry a base MD_Metadata description
     */
    public void addEntry( BaseMetadata entry ) {
        list.put( entry.getFileIdentifier(), entry );
    }

    /**
     * returns an entry (MD_Metadata object) identified by the passed fileIdentifier
     *     
     * @param fileIdentifier 
     *
     * @return base MD_Metadata object
     */
    public BaseMetadata getMetadataEntry( String fileIdentifier ) {
        return (BaseMetadata)getEntry( fileIdentifier );
    }

    /**
     * returns all base MD_Metadata objects
     *
     * @return all base MD_Metadata objects
     */
    public BaseMetadata[] getAll() {        
        return (BaseMetadata[])getAll( new BaseMetadata[1] );
    }

    /**
     * returns all base MD_Metadata objects sorted by their names
     *
     * @return 
     */
    public BaseMetadata[] getAllSortedByName() {
        BaseMetadata[] entries = getAll();
        Arrays.sort( entries, new BMComparatorImpl() );

        return entries;
    }

    /**
     * removes an entry identified by its fileIdentifier from the list
     *
     * @param fileIdentifier 
     *
     * @return 
     */
    public BaseMetadata removeMetadataEntry( String fileIdentifier ) {
        return (BaseMetadata)removeEntry( fileIdentifier );
    }
    
    /**
     * returns the name of the catalog the list of MD_Metadata objects came from
     *
     * @return name of the catalog the list of MD_Metadata objects came from
     */
    public String getCatalog() {
        return catalog;
    }
    
    /**
     * returns the boundingbox that had limited the spatial extent of the
     * request the list bases on
     *
     * @return boundingbox that had limited the spatial extent of the
     * request the list bases on
     */
    public GM_Envelope getBoundingBox() {
        return bbox;
    }

    /**
     * 
     *
     * @version $Revision$
     * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
     */
    protected class BMComparatorImpl implements Comparator {
        /**
         *
         *
         * @param o1 
         * @param o2 
         *
         * @return 
         */
        public int compare( Object o1, Object o2 ) {
            String s1 = ( (BaseMetadata)o1 ).getTitle();
            String s2 = ( (BaseMetadata)o2 ).getTitle();
            return s1.compareTo( s2 );
        }
    }
}