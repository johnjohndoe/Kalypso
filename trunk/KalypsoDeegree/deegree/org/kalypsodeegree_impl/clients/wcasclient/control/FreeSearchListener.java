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

import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.net.URL;
import java.util.HashMap;
import java.util.Iterator;

import org.deegree.enterprise.control.RPCMethodCall;
import org.deegree.enterprise.control.RPCParameter;
import org.deegree.enterprise.control.RPCStruct;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.xml.XMLTools;
import org.deegree_impl.clients.wcasclient.CatalogClientException;
import org.deegree_impl.clients.wcasclient.Constants;
import org.deegree_impl.clients.wcasclient.configuration.CSWClientConfiguration;
import org.deegree_impl.clients.wcasclient.model.BaseMetadata;
import org.deegree_impl.clients.wcasclient.model.MetadataFactory;
import org.deegree_impl.clients.wcasclient.model.MetadataList;
import org.deegree_impl.enterprise.control.*;
import org.deegree_impl.enterprise.control.RPCWebEvent;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.NetWorker;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;



/**
 * Listener for search request just containing free search terms
 *
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version $Revision$ $Date$
 */
public class FreeSearchListener extends AbstractDBListener {
    
    /**
     * validates the request to be performed.
     *
     * @param event event object containing the request to be performed
     */
    protected void validateRequest( RPCWebEvent event ) throws CatalogClientException {
        Debug.debugMethodBegin();
        
        RPCMethodCall mc = event.getRPCMethodCall();        
        RPCParameter[] para = mc.getParameters();     
        
        if ( para.length != 2 ) {
            throw new CatalogClientException( "Request / method call must contain " +
                                              "two parameters " );
        }
        
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
     * 			  to create a GetRecord request
     */
    protected HashMap createRequest(RPCWebEvent rpcEvent) throws CatalogClientException {
        Debug.debugMethodBegin();
        
        RPCMethodCall mc = rpcEvent.getRPCMethodCall();
        RPCParameter[] para = mc.getParameters();       
        HashMap request = new HashMap();
        
        // create catalog request from the rpc
        RPCStruct struct = (RPCStruct)para[1].getValue();                
        String tmp = ISO19115RequestFactory.createRequest( struct );
        
        // get names of the catalogs that shall be asked
        RPCParameter[] catalogs = (RPCParameter[])para[0].getValue();        
        for (int i = 0; i < catalogs.length; i++) {            
            request.put( catalogs[i].getValue(),  tmp );
        }        
        
        Debug.debugMethodEnd();
        return request;
    }
    
    /**
     * performs the request contained in the passed <tt>RPCWebEvent</tt>
     * 
     * @param event event object containing the request to be performed
     * @return result of the GetRecord request
     * @throws CatalogClientException
     */
    protected HashMap performRequest(RPCWebEvent event) throws CatalogClientException {
        Debug.debugMethodBegin( ) ;
        
        HashMap req = createRequest( event );
        
        CSWClientConfiguration conf = CSWClientConfiguration.getInstance();
        
        Iterator iterator = req.keySet().iterator();
        HashMap result = new HashMap();

        while ( iterator.hasNext() ) {
            String catalog = (String)iterator.next();           
            URL url = conf.getCatalogServerAddress( catalog );          
            NetWorker nw = new NetWorker( url, (String)req.get( catalog ) );                              
            try {
                Reader reader = new InputStreamReader( nw.getInputStream(), "UTF-8" );
                int c = 0;
                StringBuffer sb = new StringBuffer();
                while ( ( c = reader.read()) > -1 ) {
                    sb.append( (char)c );                    
                }
                reader.close();
                reader = new StringReader( sb.toString() );       
                result.put( catalog, XMLTools.parse( reader ) );
            } catch ( Exception e ) {
                e.printStackTrace();
                throw new CatalogClientException( e.toString() );
            }
        }
        
        Debug.debugMethodEnd();
        return result;
    }
        
    /**
     * validates the result of the catalog request and returns an <tt>Object</tt>
     * depending on the results content.
     *
     * @param result result to a GetRecord request
     * @return validated result to a GetRecord
     * @exception CatalogClientException
     */ 
    protected Object validateResult(HashMap result) throws CatalogClientException {
        Debug.debugMethodBegin();       

        MetadataList[] list = new MetadataList[ result.size() ];
        int k = 0;
        try {
            Iterator iterator = result.keySet().iterator();
            while ( iterator.hasNext() ) {
                String catalog = (String)iterator.next();
                Document doc = (Document)result.get( catalog );
                NodeList nl = doc.getElementsByTagName( "MD_Metadata" );

                // initialize a list that will gathering the result (MD_Metadata)
                // objects of a query   
                GM_Envelope bbox = CSWClientConfiguration.getInstance().getRootBoundingBox();
                list[k] = new MetadataList( bbox, catalog, nl.getLength() );
                for (int i = 0; i < nl.getLength(); i++) {
                    BaseMetadata mdMetadata = 
                        MetadataFactory.createISO19115Brief( (Element)nl.item( i ), null );
                    list[k].addEntry( mdMetadata );
                }
                k++;
            }
                        
        } catch (Exception e) {
            Debug.debugException( e, "" );
            throw new CatalogClientException( "could not parse research result", e );
        }
        
        Debug.debugMethodEnd();
        return list;
    }
    
    /**
     * handles the result of a catalog query 
     *
     * @param result result to a GetRecord request
     */
    protected void handleResult( Object result ) throws CatalogClientException {
        Debug.debugMethodBegin();
        
        this.getRequest().setAttribute( Constants.RESULT_SEARCH, result );
        
        Debug.debugMethodEnd();
    }

}