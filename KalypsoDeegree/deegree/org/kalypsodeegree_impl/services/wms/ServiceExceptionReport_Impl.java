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

import org.deegree.services.wms.*;
import org.deegree.xml.*;


/**
 * the calss encapsulates a OGC WMS 1.1.1 Service Exception Report
 *
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 */
public class ServiceExceptionReport_Impl implements ServiceExceptionReport, Marshallable {
    
    private String code = null;
    private String report = null;
    private String version = null;
    
    /**
     * Creates a new ServiceExceptionReport_Impl object.
     * @param version version of the WMS
     * @param code exception code
     * @param report free description of the exception
     */
    public ServiceExceptionReport_Impl(String version, String code, String report) {
        this.code = code;
        this.report = report;
        this.version = version;
    }

    /** returns the code of te expetion as defined in the OGC WMS 1.1.1 spec and
     * the WMS 1.1.1 test suite
     *
     * @return
     *
     */
    public String getCode() {
        return code;
    }

    /** returns a detailed description of the exption
     *
     * @return
     *
     */
    public String getReport() {
        return report;
    }

    /** returns the version of the ServiceExceptionReport
     *
     * @return
     *
     */
    public String getVersion() {
        return version;
    }
    
    /** Produces an XML-representation of this object.
     * <p>
     * @return XML-representation of this object
     *
     */
    public String exportAsXML() {
        
        StringBuffer sb = new StringBuffer( "<!DOCTYPE ServiceExceptionReport ");
        sb.append( "SYSTEM 'http://schemas.opengis.net/wms/1.1.1/WMS_exception_1_1_1.dtd'>" );
        sb.append( "<ServiceExceptionReport version='" + version +"'>" );
        sb.append( "<ServiceException code='" + code + "'>" );
        sb.append( report + "</ServiceException></ServiceExceptionReport>" );

        return sb.toString();
    }
    
}