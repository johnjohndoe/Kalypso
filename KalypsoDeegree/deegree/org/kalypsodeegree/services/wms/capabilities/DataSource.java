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
package org.deegree.services.wms.capabilities;

import org.deegree.services.OGCWebService;
import org.deegree.services.wcs.protocol.WCSGetCoverageRequest;
import org.deegree.services.wfs.protocol.WFSQuery;
import org.deegree.services.wms.protocol.WMSGetMapRequest;


/**
 * name of the data source where the WMS can find the data of a layer. the 
 * FilterServiceName element identifies the filter servive that's responsible 
 * for accessing the data.
 * <p>----------------------------------------------------------------------</p>
 *
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version 2002-03-01
 */
public interface DataSource {
    
    int LOCALWCS  = 0;
    int LOCALWFS  = 1;
    int REMOTEWMS = 2;
    int REMOTEWCS = 3;
    int REMOTEWFS = 4;
    
    /**
     * returns the scale interval the data source is valid
     */
    ScaleHint getScaleHint();

    /**
     * returns the name of the data source. The method may returns <tt>null</tt>
     * if so no name is defined and a online resource or WFS filter have shall
     * be returned.
     */
    String getName();

    /**
     * returns the type of the data source. possible values are:
     * <ul>
     *  <li>LOCALWFS</li>
     *  <li>LOCALWCS</li>
     *  <li>REMOTEWFS</li>
     *  <li>REMOTEWCS</li>
     *  <li>REMOTEWMS</li>     
     * </ul>
     * the values are defined as constants in <tt>DataSource</tt>
     *
     * @return 
     */
    int getType();
    
    /**
     * returns an instance of the <tt>OGCWebService</tt> that represents the
     * datasource. Notice: if more than one layer uses data that are offered
     * by the same OWS the deegree WMS shall just use one instance for accessing
     * the OWS 
     */
    OGCWebService getOGCWebService();

    /**
     * returns the WFS Query describe the access/filtering to the data source.     
     */
    WFSQuery getQuery();

    /**
     * returns an instance of a <tt>WMSGetMapRequest</tt> encapsulating the
     * filter conditions against a remote WMS. The request object contains:
     * WMTVER, LAYERS, STYLES, FORMAT, TRANSPARENT, VENDORSPECIFICPARAMETERS 
     *
     * @return filter conditions
     */
    WMSGetMapRequest getGetMapRequest();

    /**
     * returns an instance of a <tt>WCSGetCoverageRequest</tt> encapsulating the
     * filter conditions against a remote WCS. The request object contains:
     * VERSION, LAYER, FORMAT, VENDORSPECIFICPARAMETERS 
     *
     * @return filter conditions 
     */
    WCSGetCoverageRequest getGetCoverageRequest();
    
    /**
     * Returns the name of the geometry property in case the datasource is of
     * type LOCALWFS / REMOTEWFS.
     * <p>
     * @return name of the geometry property
     */
    String getGeometryProperty ();
}