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
package org.deegree_impl.services.gazetteer.protocol;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.HashMap;

import org.deegree.services.InconsistentRequestException;
import org.deegree.services.OGCWebServiceRequest;
import org.deegree.services.OGCWebServiceException;
import org.deegree.services.gazetteer.GazetteerException;
import org.deegree.services.gazetteer.capabilities.WFSGCapabilities;
import org.deegree.services.gazetteer.protocol.WFSGDescribeFeatureTypeRequest;
import org.deegree.services.gazetteer.protocol.WFSGGetCapabilitiesRequest;
import org.deegree.services.gazetteer.protocol.WFSGGetCapabilitiesResponse;
import org.deegree.services.gazetteer.protocol.WFSGGetFeatureRequest;
import org.deegree.services.gazetteer.protocol.WFSGGetFeatureResponse;
import org.deegree.services.wfs.filterencoding.Filter;
import org.deegree.services.wfs.filterencoding.FilterConstructionException;
import org.deegree.services.wfs.protocol.WFSNative;
import org.deegree.services.wfs.protocol.WFSQuery;
import org.deegree.xml.XMLTools;
import org.deegree.xml.Marshallable;
import org.deegree_impl.services.gazetteer.ThesaurusFilter;
import org.deegree_impl.services.wfs.protocol.WFSNative_Impl;
import org.deegree_impl.services.wfs.protocol.WFSQuery_Impl;
import org.deegree_impl.tools.Debug;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

/**
 *
 * @version $Revision$
 * @author AxxL
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 *
 */
public class WFSGProtocolFactory {
	
	// TODO Exception Handling

    private static final String gmlNS = "http://www.opengis.net/gml";
    private static final String wfsNS = "http://www.opengis.net/wfs";
    private static final String ogcNS = "http://www.opengis.net/ogc";

    private static final String GETCAPABILITIES = "GetCapabilities";
    private static final String DESCRIBEFEATURE = "DescribeFeatureType";
    private static final String GETFEATURE = "GetFeature";
    private static final String TRANSACTION = "Transaction";

    private static final String WFS_VERSION = "1.0.0";

    // no implementation needed
    private static final HashMap VENDORSPECIFICPARAMETER = null;
    private static final WFSNative NATIVE_ = null;

    /**
     * creates a WFS request from a reader that contains an XML encoded form of the request
     * @param id
     * @param reader
     * @return
     * @throws InconsistentRequestException
     */
    public static OGCWebServiceRequest createRequest(String id, Reader reader)
        throws InconsistentRequestException, GazetteerException, IOException, SAXException {
        Debug.debugMethodBegin();

        OGCWebServiceRequest request = null;

        Document doc = XMLTools.parse(reader);
        reader.close();

        String root = doc.getDocumentElement().getLocalName();

        if (root.equals(GETCAPABILITIES)) {
            request = (OGCWebServiceRequest) createWFSGGetCapabilitiesRequest(id);
        } else if (root.equals(DESCRIBEFEATURE)) {
            request = createWFSGDescribeFeatureTypeRequest(id, doc);
        } else if (root.equals(GETFEATURE)) {
            request = (OGCWebServiceRequest) createWFSGGetFeatureRequest(id, doc);
        } else if (root.equals(TRANSACTION)) {
            // request = createWFSGTransactionRequest(id, doc);
        } else {
            throw new InconsistentRequestException("Gazetteer: Unknown request: " + root);
        }

        Debug.debugMethodEnd();
        return request;
    }

    /**
     * 
     * @param id
     * @return
     */
    public static WFSGGetCapabilitiesRequest createWFSGGetCapabilitiesRequest(String id) {
        Debug.debugMethodBegin();

        WFSGGetCapabilitiesRequest req = new WFSGGetCapabilitiesRequest_Impl("1.0.0", id, VENDORSPECIFICPARAMETER, NATIVE_);

        Debug.debugMethodEnd();
        return req;
    }
    
    /**
     * @param request
     * @param capa
     * @return
     */
    public static WFSGGetCapabilitiesResponse createWFSGGetCapabilitiesResponse(WFSGGetCapabilitiesRequest request,
                                                                                WFSGCapabilities capa) {
        Debug.debugMethodBegin();

        WFSGGetCapabilitiesResponse res = new WFSGGetCapabilitiesResponse_Impl( request, null, capa );
 
        Debug.debugMethodEnd();
        return res;
        
    }
    
    /**
     * @param request
     * @param exception
     * @return
     */
    public static WFSGGetCapabilitiesResponse createWFSGGetCapabilitiesResponse(WFSGGetCapabilitiesRequest request,
                                                                                Document exception) {
        Debug.debugMethodBegin();

        WFSGGetCapabilitiesResponse res = new WFSGGetCapabilitiesResponse_Impl( request, exception, null );

        Debug.debugMethodEnd();
        return res;
        
    }

    /**
     * 
     * @param id
     * @return
     */
    public static WFSGDescribeFeatureTypeRequest createWFSGDescribeFeatureTypeRequest(String id, Document doc) {
        Debug.debugMethodBegin("WFSGProtocolFactory", "createWFSGDescribeFeatureTypeRequest");
        Element element = doc.getDocumentElement();
        String outputFormat = XMLTools.getAttrValue(element, "outputFormat");
        NodeList nl = element.getElementsByTagName("TypeName");
        String[] typeNames = null;
        if ((nl != null) && (nl.getLength() > 0)) {
            typeNames = new String[nl.getLength()];

            for (int i = 0; i < nl.getLength(); i++) {
                typeNames[i] = nl.item(i).getFirstChild().getNodeValue();
            }
        }
        WFSGDescribeFeatureTypeRequest request = createWFSGDescribeFeatureTypeRequest("1.0.0", id, null, null, outputFormat, typeNames);

        Debug.debugMethodEnd();
        return request;
    }

    /**
     * creates a <tt>WFSDescribeFeatureTypeRequest</tt> object.
     * @param id id of the request
     * @param vendorSpecificParameter none standadized parameters as
     *            name-value pairs
     * @param native_ is intended to allow access to vendor specific capabilities
     * @param outputFormat indicates the format the result shall be formated
     * @param typeNames names of the feature types that shalle be described
     */
    public static WFSGDescribeFeatureTypeRequest createWFSGDescribeFeatureTypeRequest(
        String version,
        String id,
        HashMap vendorSpecificParameter,
        WFSNative native_,
        String outputFormat,
        String[] typeNames) {

        Debug.debugMethodBegin("WFSGProtocolFactory", "createWFSGDescribeFeatureTypeRequest");

        WFSGDescribeFeatureTypeRequest req =
            new WFSGDescribeFeatureTypeRequest_Impl(version, id, vendorSpecificParameter, native_, outputFormat, typeNames);

        Debug.debugMethodEnd();
        return req;
    }

    /**
     * 
     * @param id
     * @param doc
     * @return
     * @throws InconsistentRequestException
     * @throws GazetteerException
     */
    public static WFSGGetFeatureRequest createWFSGGetFeatureRequest(String id, Document doc)
        throws InconsistentRequestException, GazetteerException {
        Debug.debugMethodBegin("WFSGProtocolFactory", "createWFSGGetFeatureRequest(id, doc)");

        Element element = doc.getDocumentElement();

        // native
        WFSNative native_ = getNative(element);

        // outputformat
        String outputFormat = XMLTools.getAttrValue(element, "outputFormat");

        // handle (optional)
        String handle = XMLTools.getAttrValue(element, "handle");

        // query containing filter(s)
        // null
        WFSQuery[] queries = getQuery(element);

        int maxFeatures = -1;
        if (XMLTools.getAttrValue(element, "maxFeatures") != null) {
            maxFeatures = Integer.parseInt(XMLTools.getAttrValue(element, "maxFeatures").trim());
        }

        int startPosition = -1;
        if (XMLTools.getAttrValue(element, "startPosition") != null) {
            startPosition = Integer.parseInt(XMLTools.getAttrValue(element, "startPosition").trim());
        }

        HashMap vendorspecificparams = null;
        Filter wfsfilterdirect = null;
        String[] propertyNames = null;
        String[] featureIds = { "2" };
        String[] typeNames = null;

        /* constructor WFSGGetFeatureRequest_Impl: 
         * String version, String id, HashMap vendorSpecificParameter,
         * WFSNative native_, String outputFormat, String handle,
         * Filter filter, int maxFeatures, int startPosition,
         * WFSQuery[] query, String[] propertyNames,
         * String[] featureIds, String[] typeNames
         */

        WFSGGetFeatureRequest getfeaturerequest =
            new WFSGGetFeatureRequest_Impl(
                WFS_VERSION,
                id,
                vendorspecificparams,
                native_,
                outputFormat,
                handle,
                wfsfilterdirect,
                maxFeatures,
                startPosition,
                queries,
                propertyNames,
                featureIds,
                typeNames);

        return getfeaturerequest;
    }

    /**
     * The query defines which feature type to query, what properties
     * to retrieve and what constraints (spatial and non-spatial) to
     * apply to those properties. <p>
     * only used for xml-coded requests
     * 
     * @param element
     * @return
     * @throws GazetteerException
     */
    private static WFSQuery[] getQuery(Element element) throws GazetteerException {
        Debug.debugMethodBegin("WFSGProtocolFactory", "getQuery");

        NodeList nl = element.getChildNodes();
        ArrayList list = new ArrayList();

        if ((nl != null) && (nl.getLength() > 0)) {
            for (int i = 0; i < nl.getLength(); i++) {
                if (nl.item(i) instanceof Element && nl.item(i).getLocalName().equals("Query")) {
                    Element elem = (Element) nl.item(i);
                    String[] propertyNames = getPropertyNames(elem);

                    String handle = XMLTools.getAttrValue(elem, "handle");
                    String version = XMLTools.getAttrValue(elem, "version");
                    String typeName = XMLTools.getAttrValue(elem, "typeName");

                    Filter filter = null;
                    try {
                        filter = getFilter(elem);
                    } catch (FilterConstructionException e) {
                        e.printStackTrace();
                    }
                    list.add(new WFSQuery_Impl(propertyNames, handle, version, typeName, filter));
                }
            }
        }

        WFSQuery[] query = (WFSQuery[]) list.toArray(new WFSQuery[list.size()]);

        Debug.debugMethodEnd();
        return query;

    }

    /**
     * returns the filter that limits the query
     */
    public static Filter getFilter(Element element) throws FilterConstructionException {
        Debug.debugMethodBegin("WFSGProtocolFactory", "getFilter");

        Filter filter = null;

        Element el = XMLTools.getChildByName("Filter", ogcNS, element);
        if (el != null) {
            // filter = org.deegree_impl.services.wfs.filterencoding.AbstractFilter.buildFromDOM(el);
            filter = ThesaurusFilter.buildFromDOM(el);
        }

        Debug.debugMethodEnd();
        return filter;
    }

    /**
     * The <Native> element is intended to allow access to vendor
     * specific capabilities of any particular web feature server or datastore.
     * The <Native> tag simply delimits the vendor specific command or operation.
     */
    public static WFSNative getNative(Element element) {
        Debug.debugMethodBegin("WFSGProtocolFactory", "getNative");
        NodeList nl = element.getChildNodes();

        WFSNative native_ = null;

        if ((nl != null) && (nl.getLength() > 0)) {
            for (int i = 0; i < nl.getLength(); i++) {
                if (nl.item(i) instanceof Element && nl.item(i).getLocalName().equals("Native")) {
                    String vendorId = XMLTools.getAttrValue(element, "vendorId");
                    String s = XMLTools.getAttrValue(element, "safeToIgnore");
                    boolean safeToIgnore = s.equalsIgnoreCase("true");
                    String n = nl.item(i).getNodeValue();
                    native_ = new WFSNative_Impl(n, vendorId, safeToIgnore);
                }
            }
        }
        Debug.debugMethodEnd();
        return native_;
    }

    /**
     * The property names is used to enumerate the feature properties
     * or attributes that should be selected. If no property names are
     * specified then all properties should be fetched.
     */
    private static String[] getPropertyNames(Element element) {
        Debug.debugMethodBegin("WFSGProtocolFactory", "getPropertyNames");

        String[] propertynames = null;
        NodeList nl = element.getChildNodes();
        ArrayList list = new ArrayList();

        if (nl != null) {
            if (nl.getLength() > 0) {
                for (int i = 0; i < nl.getLength(); i++) {
                    if (nl.item(i) instanceof Element && nl.item(i).getLocalName().equals("PropertyName")) {
                        String s = nl.item(i).getFirstChild().getNodeValue();
                        if (s.startsWith("/")) {
                            s = s.substring(1, s.length());
                        }
                        list.add(s);
                    }
                }
            }
        }

        propertynames = (String[]) list.toArray(new String[list.size()]);

        Debug.debugMethodEnd();
        return propertynames;
    }
    
    /**
     * creates a <tt>WFSGetFeatureResponse</tt> object
     * @param request a copy of the request that leads to this response
     * @param exception a describtion of an excetion (only if raised)
     * @param native_ is intended to allow access to vendor specific capabilities
     * @param affectedFeatureTypes names of the feature types affected by the
     *            response
     * @param response the response to the request
     */
    public static WFSGGetFeatureResponse createWFSGGetFeatureResponse(OGCWebServiceRequest request, 
                                                             String[] affectedFeatureTypes, 
                                                             OGCWebServiceException exception, 
                                                             Object response) {
        Debug.debugMethodBegin();

        Document doc = null;

        if (exception != null) {
            StringReader reader = new StringReader( ((Marshallable)exception).exportAsXML() );
            try {
                doc = XMLTools.parse( reader );
            } catch(Exception e) {
                System.out.println(e);	
            }
        }

        WFSGGetFeatureResponse res = new WFSGGetFeatureResponse_Impl(request, affectedFeatureTypes, 
                                                                   doc, response);

        Debug.debugMethodEnd();
        return res;
    }

}