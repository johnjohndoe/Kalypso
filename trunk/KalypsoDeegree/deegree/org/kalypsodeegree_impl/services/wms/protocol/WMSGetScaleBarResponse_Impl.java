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

import org.deegree.services.wms.protocol.WMSGetScaleBarRequest;
import org.deegree.services.wms.protocol.WMSGetScaleBarResponse;
import org.deegree_impl.services.OGCWebServiceResponse_Impl;
import org.w3c.dom.Document;

/**
 * interface defining the access to the response of the deegree specific WMS
 * request GetScaleBar
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @author <a href="mailto:wanhoff@uni-bonn.de">Jeronimo Wanhoff </a>
 */
public class WMSGetScaleBarResponse_Impl extends OGCWebServiceResponse_Impl implements
    WMSGetScaleBarResponse
{
  private Object sBar;

  /**
   * Constructor for this Response
   * 
   * @param request
   * @param exception
   */
  WMSGetScaleBarResponse_Impl( WMSGetScaleBarRequest request, Document exception )
  {
    super( request, exception );
  }

  /**
   * Constructor for this Response
   * 
   * @param request
   * @param scaleBar
   */
  WMSGetScaleBarResponse_Impl( WMSGetScaleBarRequest request, Object scaleBar )
  {
    super( request, null );
    setScaleBar( scaleBar );
  }

  /**
   * sets the graphic representation of the requested scale bar.
   * 
   * @param scaleBar
   *          scale bar Object
   */
  public void setScaleBar( Object scaleBar )
  {
    sBar = scaleBar;
  }

  /**
   * returns the graphic representation of the requested scale bar.
   * 
   * @return scale bar
   */
  public Object getScaleBar()
  {
    return sBar;
  }
}