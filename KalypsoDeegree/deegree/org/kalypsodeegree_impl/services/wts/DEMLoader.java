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
package org.deegree_impl.services.wts;

import java.io.StringReader;

import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Surface;
import org.deegree.services.OGCWebServiceEvent;
import org.deegree.services.wcs.protocol.WCSGetCoverageRequest;
import org.deegree.services.wfs.protocol.WFSGetFeatureRequest;
import org.deegree.services.wts.configuration.WTSConfiguration;
import org.deegree.services.wts.protocol.WTSGetViewRequest;
import org.deegree.xml.XMLTools;
import org.deegree_impl.services.OGCWebServiceEvent_Impl;
import org.deegree_impl.services.wcs.WCService_Impl;
import org.deegree_impl.services.wcs.protocol.WCSProtocolFactory;
import org.deegree_impl.services.wfs.WFSService_Impl;
import org.deegree_impl.services.wfs.protocol.WFSProtocolFactory;
import org.deegree_impl.services.wts.configuration.WTSConfiguration_Impl;
import org.w3c.dom.Document;

/**
 * 
 *
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 */
class DEMLoader extends Thread {
    private WTService_Impl parent = null;
    private WTSGetViewRequest request = null;

    /**
     * Creates a new DEMLoader object.
     *
     * @param request 
     * @param parent 
     */
    DEMLoader( WTSGetViewRequest request, WTService_Impl parent ) {
        this.request = request;
        this.parent = parent;
    }

    /**
     *
     */
    public void run() {
        try {
        	GM_Surface[][] boxes = parent.getBoxes();
            WTSConfiguration conf = WTSConfiguration_Impl.getInstance();
            String layer = request.getElevationModels()[0];
            int srcType = conf.getSourceType( layer );

            if ( ( srcType == WTSConfiguration.LOCALWCS ) || 
                     ( srcType == WTSConfiguration.REMOTEWCS ) ) {
                handleWCS( boxes );
            } else if ( ( srcType == WTSConfiguration.LOCALWFS ) || 
                            ( srcType == WTSConfiguration.REMOTEWFS ) ) {
            	handleWFS( boxes );
            }
        } catch ( Exception e ) {
            e.printStackTrace();
        }
    }

    /**
     * handles the creation and performing of a GetCoverage request to access
     * the DEM underlying the 3D scene to be created
     */
    private void handleWCS(GM_Surface[][] boxes) throws Exception {
    	
        WTSConfiguration conf = WTSConfiguration_Impl.getInstance();
        String layer = request.getElevationModels()[0];

        // create and perform dem requests
        for ( int i = 0; i < boxes.length; i++ ) {
            for ( int j = 0; j < boxes[0].length; j++ ) {
                int sx = (int)boxes[i][j].getEnvelope().getWidth();
                int sy = (int)boxes[i][j].getEnvelope().getHeight();

                while ( sx > 40 )
                    sx /= 2;

                if ( sx < 3 ) {
                    sx = 3;
                }

                while ( sy > 30 )
                    sy /= 2;

                if ( sy < 3 ) {
                    sy = 3;
                }

                WCSGetCoverageRequest wcsReq = WCSProtocolFactory.createWCSGetCoverageRequest( 
                                                       "1.0.0", i + "-" + j, null, layer, 
                                                       request.getSrs(), request.getSrs(), 
                                                       boxes[i][j].getEnvelope(), null, sx, sy, -1, 
                                                       conf.getFormatName( layer ), null, 
                                                       request.getExceptions() );

                OGCWebServiceEvent event = new OGCWebServiceEvent_Impl( this, wcsReq, "", parent );
                WCService_Impl service = (WCService_Impl)conf.getResponsibleService( layer );
                service = (WCService_Impl)service.clone();
                service.doService( event );
            }
        }
    }

    /**
     * handles the creation and performing of a GetFeature request to access
     * the DEM underlying the 3D scene to be created as a TIN
     */
    private void handleWFS(GM_Surface[][] boxes) throws Exception {
    	
        WTSConfiguration conf = WTSConfiguration_Impl.getInstance();
        String layer = request.getElevationModels()[0];
        int srcType = conf.getSourceType( layer );

        for ( int i = 0; i < boxes.length; i++ ) {
            for ( int j = 0; j < boxes[0].length; j++ ) {
                GM_Envelope bbox = boxes[i][j].getEnvelope();
                StringBuffer sb = new StringBuffer( 5000 );
                sb.append( "<?xml version='1.0' encoding='UTF-8'?>" );
                sb.append( "<GetFeature xmlns='http://www.opengis.net/wfs' " );
                sb.append( "xmlns:ogc='http://www.opengis.net/ogc' " );
                sb.append( "xmlns:gml='http://www.opengis.net/gml' " );
                sb.append( "service='WFS' version='1.0.0' " );

                if ( srcType == WTSConfiguration.LOCALWFS ) {
                    sb.append( "outputFormat='FEATURECOLLECTION'>" );
                } else {
                    sb.append( "outputFormat='GML2'>" );
                }

                sb.append( "<Query typeName='" + layer + "'>" );
                sb.append( "<PropertyName>" );
                sb.append( "GEOM" );
                sb.append( "</PropertyName>" );
                sb.append( "<ogc:Filter><ogc:BBOX>" );
                sb.append( "<PropertyName>" ).append( "GEOM" );
                sb.append( "</PropertyName>" );
                sb.append( "<gml:Box srsName='EPSG:31466'>" );
                sb.append( "<gml:coordinates>" ).append( bbox.getMin().getX() );
                sb.append( ',' ).append( bbox.getMin().getY() );
                sb.append( ' ' ).append( bbox.getMax().getX() ).append( ',' );
                sb.append( bbox.getMax().getY() ).append( "</gml:coordinates >" );
                sb.append( "</gml:Box>" );
                sb.append( "</ogc:BBOX>" );
                sb.append( "</ogc:Filter></Query></GetFeature>" );

                Document doc = XMLTools.parse( new StringReader( sb.toString() ) );

                // create OGCWebServiceEvent object                    		
                WFSGetFeatureRequest gfr = WFSProtocolFactory.createWFSGetFeatureRequest( "TIN" + i + 
                                                                                          "-" + j, 
                                                                                          doc );

                OGCWebServiceEvent event = new OGCWebServiceEvent_Impl( this, gfr, "", parent );
                WFSService_Impl service = (WFSService_Impl)conf.getResponsibleService( layer );
                //service = (WFSService_Impl)service.clone();
                service.doService( event );
            }
        }
    }
}