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

import org.deegree.enterprise.control.RPCMethodCall;
import org.deegree.enterprise.control.RPCParameter;
import org.deegree.enterprise.control.RPCStruct;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree_impl.clients.wcasclient.CatalogClientException;
import org.deegree_impl.clients.wcasclient.Constants;
import org.deegree_impl.clients.wcasclient.configuration.CSWClientConfiguration;
import org.deegree_impl.clients.wcasclient.model.BaseMetadata;
import org.deegree_impl.clients.wcasclient.model.MetadataFactory;
import org.deegree_impl.clients.wcasclient.model.MetadataList;
import org.deegree_impl.enterprise.control.RPCWebEvent;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.deegree_impl.tools.Debug;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;


/**
 * 
 *
 * The class listens to the get request against iso 19115 formated metadata
 * here the result shall be returned in full format
 *
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version $Revision$ $Date$
 */
public class DetailedSearchListener extends FreeSearchListener {
    
    protected GM_Envelope bbox = null;
    
    /**
     * validates the request to be performed.
     *
     * @param event event object containing the request to be performed
     */
    protected void validateRequest( RPCWebEvent event ) throws CatalogClientException {
        Debug.debugMethodBegin();

        RPCMethodCall mc = event.getRPCMethodCall();        
        RPCParameter[] para = mc.getParameters();     
        
        RPCStruct struct = (RPCStruct)para[1].getValue();  
        // create envelop if a bounding box was part of the request
        if (struct.getMember( Constants.RPC_BBOX ) != null ) {
            RPCStruct bboxStruct = (RPCStruct)struct.getMember( Constants.RPC_BBOX ).getValue();
            Double minx = (Double)bboxStruct.getMember( Constants.RPC_BBOXMINX ).getValue();
            Double miny = (Double)bboxStruct.getMember( Constants.RPC_BBOXMINY ).getValue();
            Double maxx = (Double)bboxStruct.getMember( Constants.RPC_BBOXMAXX ).getValue();
            Double maxy = (Double)bboxStruct.getMember( Constants.RPC_BBOXMAXY ).getValue();

            bbox = GeometryFactory.createGM_Envelope( minx.doubleValue(), miny.doubleValue(),
            										  maxx.doubleValue(), maxy.doubleValue() );                                
        } else {
            bbox = CSWClientConfiguration.getInstance().getRootBoundingBox();
        }
        
        // write request parameter into session to reconstruct the search form
        HttpSession session = ( (HttpServletRequest)this.getRequest() ).getSession( true );
        session.setAttribute( Constants.SESSION_DETAILEDSEARCHPARAM, para );
       
        Debug.debugMethodEnd(); 
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
    
    
}