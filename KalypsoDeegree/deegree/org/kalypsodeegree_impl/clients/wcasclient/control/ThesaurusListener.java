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

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.deegree.enterprise.control.RPCMethodCall;
import org.deegree.enterprise.control.RPCParameter;
import org.deegree.enterprise.control.RPCException;
import org.deegree.enterprise.control.RPCMethodResponse;
import org.deegree.services.wfs.protocol.WFSGetFeatureRequest;
import org.deegree.xml.Marshallable;
import org.deegree.xml.XMLTools;
import org.deegree_impl.clients.wcasclient.CatalogClientException;
import org.deegree_impl.clients.wcasclient.Constants;
import org.deegree_impl.clients.wcasclient.configuration.CSWClientConfiguration;
import org.deegree_impl.clients.wcasclient.model.ThesaurusEntry;
import org.deegree_impl.clients.wcasclient.model.ThesaurusList;
import org.deegree_impl.enterprise.control.*;
import org.deegree_impl.enterprise.control.RPCWebEvent;
import org.deegree_impl.enterprise.control.RPCFactory;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.NetWorker;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;

/**
 *
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version $Revision$ $Date$
 */
public class ThesaurusListener extends AbstractDBListener {
      
    
    /**
     * validates the request to be performed.
     *
     * @param event event object containing the request to be performed
     */
    protected void validateRequest( RPCWebEvent event ) throws CatalogClientException {
        Debug.debugMethodBegin();
                

        Debug.debugMethodEnd();
    }
    
    /**
     * creates a GetFeature request from the <tt>RPCWebEvent</tt> passed to this
     * listener
     *
     * @param rpcEvent event object containing the request to be performed
     * @return string representation of a GetFeature request
     * @exception CatalogClientException will be throwns if it is impossible 
     * 				to create a GetFeature request
     */
    protected HashMap createRequest(RPCWebEvent rpcEvent) throws CatalogClientException {
        Debug.debugMethodBegin();
        
        RPCMethodCall mc = rpcEvent.getRPCMethodCall();
        RPCParameter[] para = mc.getParameters();       
        HashMap request = new HashMap();
        
        // create catalog request from the rpc
        String pattern = (String)para[1].getValue();                
        WFSGetFeatureRequest req = ThesaurusRequestFactory.createRequest( pattern );
        
        // get names of the catalogs that shall be asked
        RPCParameter[] catalogs = (RPCParameter[])para[0].getValue();        
        for (int i = 0; i < catalogs.length; i++) {            
            request.put( catalogs[i].getValue(),  ((Marshallable)req).exportAsXML() );
        } 
        
        // write request parameter into session to reconstruct the search form
        HttpSession session = ( (HttpServletRequest)this.getRequest() ).getSession( true );
        session.setAttribute( Constants.SESSION_DETAILEDSEARCHPARAM, para );
        
        try  {
            RPCMethodResponse resp = RPCFactory.createRPCMethodResponse( para );
            this.getRequest().setAttribute( Constants.SESSION_DETAILEDSEARCHPARAM, resp ); 
        } catch (RPCException e) {
            e.printStackTrace();
        }     

        Debug.debugMethodEnd();
        return request;
    }   
    
    /**
     * performs the request contained in the passed <tt>RPCWebEvent</tt>
     * 
     * @param event event object containing the request to be performed
     * @return result of the GetRecord request
     * @exception CatalogClientException
     */
    protected HashMap performRequest(RPCWebEvent event) throws CatalogClientException {
        Debug.debugMethodBegin( this, "performRequest" ) ;
        
        HashMap req = createRequest( event );
        
        CSWClientConfiguration conf = CSWClientConfiguration.getInstance();
        
        Iterator iterator = req.keySet().iterator();
        HashMap result = new HashMap();
        while ( iterator.hasNext() ) {
            String thesaurus = (String)iterator.next();        
            URL url = conf.getThesaurusAddress( thesaurus );
            NetWorker nw = new NetWorker( url, (String)req.get( thesaurus ) );                              
            try {
                Reader reader = new InputStreamReader( nw.getInputStream(), "UTF-8" );
                int c = 0;
                StringBuffer sb = new StringBuffer();
                while ( ( c = reader.read()) > -1 ) {
                    sb.append( (char)c );                    
                }
                reader.close();
                reader = new StringReader( sb.toString() );       
                result.put( thesaurus, XMLTools.parse( reader ) );
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
        
        ThesaurusList list = new ThesaurusList();
        Iterator iterator = result.keySet().iterator();
        while ( iterator.hasNext() ) {
            String thesaurus = (String)iterator.next();
            Document doc = (Document)result.get( thesaurus );
            NodeList nl = doc.getElementsByTagName( "TH_TERM" );
            for (int i = 0; i < nl.getLength(); i++) {
                String term = XMLTools.getStringValue( nl.item(i) );
                ThesaurusEntry te = new ThesaurusEntry( term, thesaurus );
                list.addEntry( te );
            }
        }
        
        Debug.debugMethodEnd();
        return list;
    }
    
    /**
     * handles the result of a 'FULL' catalog query 
     *
     * @param result result to a GetRecord request
     */
    protected void handleResult( Object result ) throws CatalogClientException {
        Debug.debugMethodBegin();
        
        getRequest().setAttribute( Constants.THESAURUSRESULT, result );
       
        Debug.debugMethodEnd();
    }

}