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

import org.deegree_impl.tools.Debug;


/**
 *
 * @author  administrator
 */
public class ISO19119RequestFactory {
    /**
     * creates a GetRecord request that is conform to the OGC Stateless Web
     * Service Catalog Profil and GDI NRW catalog specifications against a 
     * a catalog serving ISO 19119 conform service metadata
     */
    public static String createRequest( String setName, String serviceType, String[] datasetIds, String operation )
                                throws Exception {
        Debug.debugMethodBegin( "WCASRequestFactory", "createRequest( ... )" );

        StringBuffer sb = new StringBuffer( "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>" );
        sb.append( "<GetRecord xmlns:ogc=\"http://www.opengis.net/ogc\" " );
        sb.append( "xmlns:gml=\"http://www.opengis.net/gml\" maxRecords=\"10\" " );
        sb.append( "outputFormat=\"XML\" outputRecType=\"ISO19119\" queryScope=\"0\" " );
        sb.append( "startPosition=\"-1\">" );
        sb.append( "<Query typeName=\"Service\"><PropertySet setName=\"" + setName + "\"/>" );
        sb.append( "<ogc:Filter><ogc:And>" );
        sb.append( "<PropertyIsLike wildCard=\"*\" singleChar=\"?\" escape=\"/\">" );
        sb.append( "<PropertyName>ISO19119/serviceType</PropertyName>" );
        sb.append( "<Literal>" + serviceType + "</Literal></PropertyIsLike>" );
        sb.append( "<PropertyIsLike wildCard=\"*\" singleChar=\"?\" escape=\"/\">" );
        sb.append( "<PropertyName>ISO19119/operationMetadata/operationName</PropertyName>" );
        sb.append( "<Literal>" + operation + "</Literal></PropertyIsLike>" );

        for ( int i = 0; i < datasetIds.length; i++ ) {
            sb.append( "<PropertyIsLike wildCard=\"*\" singleChar=\"?\" escape=\"/\">" );
            sb.append( "<PropertyName>ISO19119/ISO19115/fileIdentifier</PropertyName>" );
            sb.append( "<Literal>" + datasetIds[i] + "</Literal></PropertyIsLike>" );
        }

        sb.append( "</ogc:And></ogc:Filter></Query></GetRecord>" );

        Debug.debugMethodEnd();
        return sb.toString();
    }
    
    /**
     * creates a GetRecord request that is conform to the OGC Stateless Web
     * Service Catalog Profil and GDI NRW catalog specifications against a 
     * a catalog serving ISO 19119 conform service metadata
     */
    public static String createRequest( String setName, String[] datasetIds )
                                throws Exception {
        Debug.debugMethodBegin( );

        StringBuffer sb = new StringBuffer( "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>" );
        sb.append( "<GetRecord xmlns:ogc=\"http://www.opengis.net/ogc\" " );
        sb.append( "xmlns:gml=\"http://www.opengis.net/gml\" maxRecords=\"10\" " );
        sb.append( "outputFormat=\"XML\" outputRecType=\"ISO19119\" queryScope=\"0\" " );
        sb.append( "startPosition=\"-1\">" );
        sb.append( "<Query typeName=\"Service\"><PropertySet setName=\"" + setName + "\"/>" );
        if ( datasetIds != null && datasetIds.length > 0 ) {
            sb.append( "<ogc:Filter>");

            if ( datasetIds.length > 1 ) {
                sb.append( "<ogc:And>" );     
            }

            for ( int i = 0; i < datasetIds.length; i++ ) {
                sb.append( "<PropertyIsLike wildCard=\"*\" singleChar=\"?\" escape=\"/\">" );
                sb.append( "<PropertyName>ISO19119/ISO19115/fileIdentifier</PropertyName>" );
                sb.append( "<Literal>" + datasetIds[i] + "</Literal></PropertyIsLike>" );
            }

            if ( datasetIds.length > 1 ) {
                sb.append( "</ogc:And>");
            }        

            sb.append( "</ogc:Filter>" );
        }
        sb.append( "</Query></GetRecord>" );

        Debug.debugMethodEnd();
        return sb.toString();
    }
}