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
package org.deegree_impl.services.wms.protocol;

import org.deegree.services.OGCWebServiceRequest;
import org.deegree.services.wms.protocol.WMSGetMapResponse;
import org.deegree_impl.services.OGCWebServiceResponse_Impl;
import org.w3c.dom.Document;

/**
 * This interface describes the access to the response of a GetMap request.
 * <p>
 * </p>
 * The response to a valid GetMap request shall be a map of the georeferenced
 * information layer requested, in the desired style, and having the specified
 * spatial reference system, bounding box, size, format and transparency.
 * <p>
 * </p>
 * An invalid GetMap request shall yield an error output in the requested
 * Exceptions format (or a network protocol error response in extreme cases). In
 * an HTTP environment, the MIME type of the returned value's Content-type
 * entity header shall match the format of the return value.
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp </a>
 * @version 2002-03-01
 */
public class WMSGetMapResponse_Impl extends OGCWebServiceResponse_Impl implements WMSGetMapResponse
{
  private Object map = null;

  /**
   * constructor initializing the class with the <WMSGetMapResponse>
   */
  WMSGetMapResponse_Impl( OGCWebServiceRequest request, Document exception, Object map )
  {
    super( request, exception );
    setMap( map );
  }

  /**
   * returns the map that fullfills the GetMap request. If a exception raised
   * generating the map and the exception format doesn't equals
   * application/vnd.ogc.se_inimage or application/vnd.ogc.se_blank
   * <tt>null</tt> will be returned.
   */
  public Object getMap()
  {
    return map;
  }

  /**
   * sets the map that fullfills the GetMap request. If a exception raised
   * generating the map and the exception format doesn't equals
   * application/vnd.ogc.se_inimage or application/vnd.ogc.se_blank
   * <tt>null</tt> will be returned.
   */
  public void setMap( Object map )
  {
    this.map = map;
  }
}