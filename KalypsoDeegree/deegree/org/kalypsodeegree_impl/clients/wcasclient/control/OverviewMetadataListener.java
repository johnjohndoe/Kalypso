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
package org.deegree_impl.clients.wcasclient.control;

import java.util.HashMap;
import java.util.Iterator;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.deegree.enterprise.control.FormEvent;
import org.deegree.enterprise.control.RPCMethodCall;
import org.deegree.enterprise.control.RPCParameter;
import org.deegree.enterprise.control.RPCStruct;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree_impl.clients.wcasclient.CatalogClientException;
import org.deegree_impl.clients.wcasclient.Constants;
import org.deegree_impl.clients.wcasclient.model.DetailedMetadata;
import org.deegree_impl.clients.wcasclient.model.MetadataFactory;
import org.deegree_impl.clients.wcasclient.model.MetadataList;
import org.deegree_impl.enterprise.control.RPCWebEvent;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.deegree_impl.services.wfs.filterencoding.FeatureFilter;
import org.deegree_impl.services.wfs.filterencoding.FeatureId;
import org.deegree_impl.tools.Debug;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;


/**
 * Listener for accessing a full metadata description
 *
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version $Revision$ $Date$
 */
public class OverviewMetadataListener extends FreeSearchListener {
    
    private GM_Envelope bbox = null;
    
    /**
     * This method is called to get one full metadata description
     */
    public void actionPerformed( FormEvent event ) {
        Debug.debugMethodBegin();  
        
        RPCWebEvent e = (RPCWebEvent)event;
        RPCMethodCall mc = e.getRPCMethodCall();
        RPCParameter[] params = mc.getParameters();
        if ( params == null || params.length == 0 ) {
            // get Metadata (DetailedMetadata) from the users sesssion
            HttpSession session = ( (HttpServletRequest)this.getRequest() ).getSession( true );
            Object o = session.getAttribute( Constants.SESSION_METADATA );
            if ( o != null ) {
                getRequest().setAttribute( Constants.RESULT_SEARCH, o );
            } 
        }else {
            super.actionPerformed( event );
        }
        
        Debug.debugMethodEnd();
    }
    
    /**
     * validates the request to be performed.
     *
     * @param event event object containing the request to be performed
     */
    protected void validateRequest( RPCWebEvent event ) throws CatalogClientException {
        Debug.debugMethodBegin();  
        
        RPCMethodCall mc = event.getRPCMethodCall();
        RPCParameter[] para = mc.getParameters();   
        RPCStruct struct = (RPCStruct)para[0].getValue();
        RPCStruct bboxStruct = (RPCStruct)struct.getMember( Constants.RPC_BBOX ).getValue();    
        Double minx = (Double)bboxStruct.getMember( Constants.RPC_BBOXMINX ).getValue();
        Double miny = (Double)bboxStruct.getMember( Constants.RPC_BBOXMINY ).getValue();
        Double maxx = (Double)bboxStruct.getMember( Constants.RPC_BBOXMAXX ).getValue();
        Double maxy = (Double)bboxStruct.getMember( Constants.RPC_BBOXMAXY ).getValue();

        bbox = GeometryFactory.createGM_Envelope( minx.doubleValue(), miny.doubleValue(),
        										  maxx.doubleValue(), maxy.doubleValue() );
                
        Debug.debugMethodEnd();
    }
    
    /**
     * creates a GetRecord request from the <tt>RPCWebEvent</tt> passed to this
     * listener
     *
     * @param rpcEvent event object containing the request to be performed
     * @return HashMap containing a string representation of a GetRecord request
     *                 for each targeted catalog
     * @exception CatalogClientException will be throwns if it is impossible 
     * 									 to create a GetRecord request
     */
    protected HashMap createRequest(RPCWebEvent rpcEvent) throws CatalogClientException {
        Debug.debugMethodBegin();
        
        RPCMethodCall mc = rpcEvent.getRPCMethodCall();
        RPCParameter[] para = mc.getParameters();       
        HashMap request = new HashMap();
        
        // create catalog request from the rpc
        RPCStruct struct = (RPCStruct)para[0].getValue();                
        String id = (String)struct.getMember( "ID" ).getValue();    
        
        FeatureFilter filter = new FeatureFilter();
        filter.addFeatureId( new FeatureId( id ) );
        
        StringBuffer req = new StringBuffer( 5000 );
        req.append( "<?xml version='1.0' encoding='UTF-8'?>" );
        req.append( "<GetRecord " );
        req.append( " xmlns:wfs=\"http://www.opengis.net/wfs\"" );
        req.append( " xmlns:ogc=\"http://www.opengis.net/ogc\"" ); 
        req.append( " maxRecords='-1'" );
        req.append( " outputFormat='XML'" );
        req.append( " outputRecType='ISO19115'" );
        req.append( " queryScope='1'" );
        req.append( " startPosition='0'>" );
        req.append( "<Query typeName='" + Constants.PRODUCT + "'>" );
        req.append( "<PropertySet setName='Full'/>" );
        req.append( filter.toXML() );
        req.append( "</Query>" );
        req.append( "</GetRecord>" );        
        
        // get names of the catalogs that shall be asked
        String catalog = (String)struct.getMember( Constants.RPC_CATALOG ).getValue();    
        request.put( catalog, req.toString() );
                
        Debug.debugMethodEnd();
        return request;
    }
        
    /**
     * validates the result of the full get record request and returns an <tt>Object</tt>
     * depending on the results content.
     *
     * @param result result to a full GetRecord request
     * @return validated result to a full GetRecord
     * @exception CatalogClientException
     */ 
    protected Object validateResult(HashMap result) throws CatalogClientException {
        Debug.debugMethodBegin();
     
        MetadataList mlist = null;
        try {
            Iterator iterator = result.keySet().iterator();
            String catalog = (String)iterator.next();
            Document doc = (Document)result.get( catalog );
            // get Metadata object from the query result
            NodeList nl = doc.getElementsByTagName( "MD_Metadata" );
            // initialize a list that will gathering the result (MD_Metadata)
            // objects of a query   
            mlist = new MetadataList( bbox, catalog);
            DetailedMetadata mdMetadata = 
                 MetadataFactory.createISO19115Full( (Element)nl.item( 0 ), null );
            mlist.addEntry( mdMetadata );
                        
        } catch (Exception e) {
            Debug.debugException( e, "" );
            throw new CatalogClientException( "couldn' create full metadata object"+
                                              " from the catalog response", e );
        }
        
        Debug.debugMethodEnd();
        return mlist;
    }
    
    /**
     * handles the result of a 'FULL' catalog query 
     *
     * @param result result to a GetRecord request
     */
    protected void handleResult( Object result ) throws CatalogClientException {
        Debug.debugMethodBegin();
        
        HttpSession session = ( (HttpServletRequest)this.getRequest() ).getSession( true );
        session.setAttribute( Constants.SESSION_METADATA, result );
        getRequest().setAttribute( Constants.RESULT_SEARCH, result );
        
        Debug.debugMethodEnd();
    }

}