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
import org.deegree.services.wms.protocol.WMSGetLegendGraphicResponse;
import org.deegree_impl.services.OGCWebServiceResponse_Impl;
import org.w3c.dom.Document;

/**
 * The class encapsulates the response to a GetLegendGraphic request.
 * 
 * <p>
 * --------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp </a>
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public class WMSGetLegendGraphicResponse_Impl extends OGCWebServiceResponse_Impl implements
    WMSGetLegendGraphicResponse
{
  private Object legendGraphic = null;

  /**
   * constructor initializing the class
   */
  WMSGetLegendGraphicResponse_Impl( OGCWebServiceRequest request, Object legendGraphic )
  {
    super( request, null );
    setLegendGraphic( legendGraphic );
  }

  /**
   * constructor initializing the class
   */
  WMSGetLegendGraphicResponse_Impl( OGCWebServiceRequest request, Document exception )
  {
    super( request, exception );
  }

  /**
   * returns the graphic that corresponds to a GetLegendGraphics request. If an
   * exception raised during processing or the request was invalid <tt>null</tt>
   * will be returned.
   */
  public Object getLegendGraphic()
  {
    return legendGraphic;
  }

  /**
   * sets the graphic that corresponds to a GetLegendGraphics request. If an
   * exception raised during processing or the request was invalid <tt>null</tt>
   * will be returned.
   */
  public void setLegendGraphic( Object legendGraphic )
  {
    this.legendGraphic = legendGraphic;
  }
}