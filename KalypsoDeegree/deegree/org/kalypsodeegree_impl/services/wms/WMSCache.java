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
package org.deegree_impl.services.wms;

import java.awt.Image;

import org.deegree.services.OGCWebServiceRequest;
import org.deegree.services.OGCWebServiceResponse;
import org.deegree.services.wms.protocol.WMSFeatureInfoRequest;
import org.deegree.services.wms.protocol.WMSFeatureInfoResponse;
import org.deegree.services.wms.protocol.WMSGetMapRequest;
import org.deegree.services.wms.protocol.WMSGetMapResponse;
import org.deegree_impl.services.wms.protocol.WMSProtocolFactory;
import org.deegree_impl.tools.Cache_Impl;
import org.deegree_impl.tools.Debug;


/**
 * class for caching the results of GetMap and GetFeatureInfo requests. GetMap
 * requests containing a reference to a SLD document won't be written into
 * the cache.
 *
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 */
public class WMSCache extends Cache_Impl {
    private static WMSCache cache = null;    
    private int actualSize = 0;
    private int maxSize = 0;

    /** 
     * Creates a new instance of WMSCache 
     * @param maxSize maximum cache size in KB
     */
    private WMSCache( int maxSize ) {
        super( 5000 );
        this.maxSize = maxSize;
    }

    /**
     * realizes the singelton pattern to access/create an instance of a
     * <tt>WMSCache</tt>
     *
     * @param maxSize maximum cache size in KB
     *
     * @return instance of <tt>WMSCache</tt>
     */
    public synchronized static WMSCache getInstance( int maxSize ) {
        if ( cache == null ) {
            cache = new WMSCache( maxSize );
        }

        return cache;
    }

    /** gets an entry from the cache. The entry that shall be returned will be
     * identified by the passed identifier. If no entry for a passed identifier 
     * can be found within the cache the method shall return <tt>null</tt>.
     * @param identifier key of a entry of the cache
     */
    public OGCWebServiceResponse get( WMSGetMapRequest identifier ) {
        Debug.debugMethodBegin( this, "get" );

        String s = "SLD=";
        try {
            s = identifier.getRequestParameter();
        } catch (Exception e) {}

        // if the request contains a SLD reference avoid accessing data from
        // the cache
        if ( s.toUpperCase().indexOf( "SLD=" ) >= 0 ) {
            return null;
        }
        Object o = super.get( s );
        WMSGetMapResponse res = null;
        if ( o != null ) {  
            res = WMSProtocolFactory.createWMSGetMapResponse( identifier, null, o );        
        }

        Debug.debugMethodEnd();
        return res;
    }

    /** gets an entry from the cache. The entry that shall be returned will be
     * identified by the passed identifier. If no entry for a passed identifier 
     * can be found within the cache the method shall return <tt>null</tt>.
     * @param identifier key of a entry of the cache
     */
    public OGCWebServiceResponse get( WMSFeatureInfoRequest identifier ) {
        Debug.debugMethodBegin( this, "get" );

        Object o = super.get( identifier.toString() );
        WMSFeatureInfoResponse res = null;
        if ( o != null ) {            
            res = WMSProtocolFactory.createWMSFeatureInfoResponse( identifier, null, (String)o );
        }
        
        Debug.debugMethodEnd();
        return res;
    }

    /** pushes a new entry to the cache. Each entry is marked by an unique
     * identifier. The passed data object shall be a <tt>Image<tt> or a
     * <tt>String</tt>.
     * @param identifier identifier key of a entry of the cache
     * @param data data object to be stored in the cache
     */
    public void push( OGCWebServiceRequest identifier, Object data ) {
        Debug.debugMethodBegin( this, "put" );

        String s = "SLD=";
        try {
            s = identifier.getRequestParameter();
        } catch (Exception e) {}
        // if the request contains a SLD reference avoid writing data to the
        // the cache
        if ( s.toUpperCase().indexOf( "SLD=" ) >= 0 ) {
            return;
        }

        // calculate size of the new object
        int sz = 10;

        if ( data instanceof Image ) {
            Image im = (Image)data;
            sz = ( im.getWidth( null ) * im.getHeight( null ) * 4 ) / 1024;
        } else if ( data instanceof String ) {
            sz = ( data.toString().length() * 2 ) / 1024;
        }

        // remove oldest object from the cache if the cache maximum size is
        // reached
        while ( ( ( sz + actualSize ) > maxSize ) && ( actualSize > 0 ) ) {
            deleteOldest();
            // calculate the size of the object removed from the cache
            int sz_ = 10;

            if ( data instanceof Image ) {
                Image im = (Image)data;
                sz_ = ( im.getWidth( null ) * im.getHeight( null ) * 4 ) / 1024;
            } else if ( data instanceof String ) {
                sz_ = ( data.toString().length() * 2 ) / 1024;
            }

            // recalculate actual cache size after removing the oldest object
            actualSize -= sz_;
        }

        if ( actualSize < 0 ) {
            actualSize = 0;
        }

        super.push( s, data );

        Debug.debugMethodEnd();
    }
}