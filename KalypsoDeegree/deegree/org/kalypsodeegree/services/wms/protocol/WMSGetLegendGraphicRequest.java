// $Header:
// /cvsroot/deegree/deegree/org/deegree/services/wms/protocol/WMSGetLegendGraphicRequest.java,v
// 1.5 2004/03/30 07:10:13 poth Exp $
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
package org.deegree.services.wms.protocol;

import java.net.URL;

import org.deegree.services.OGCWebServiceRequest;

/**
 * 
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @author last edited by: $Author$
 * 
 * @version 1.0. $Revision$, $Date$
 * 
 * @since 1.0
 */
public interface WMSGetLegendGraphicRequest extends OGCWebServiceRequest
{
  /**
   * 
   * 
   * @return
   */
  String getLayer();

  /**
   * 
   * 
   * @return
   */
  String getStyle();

  /**
   * 
   * 
   * @return
   */
  String getFeatureType();

  /**
   * 
   * 
   * @return
   */
  String getRule();

  /**
   * 
   * 
   * @return
   */
  double getScale();

  /**
   * 
   * 
   * @return
   */
  URL getSLD();

  /**
   * 
   * 
   * @return
   */
  String getSLD_Body();

  /**
   * 
   * 
   * @return
   */
  String getFormat();

  /**
   * This gives a hint for the width of the returned graphic in pixels.
   * Vector-graphics can use this value as a hint for the level of detail to
   * include.
   */
  int getWidth();

  /**
   * This gives a hint for the height of the returned graphic in pixels.
   * Vector-graphics can use this value as a hint for the level of detail to
   * include.
   */
  int getHeight();

  /**
   * This gives the MIME type of the format in which to return exceptions.
   * Allowed values are the same as for the EXCEPTIONS= parameter of the WMS
   * GetMap request.
   */
  String getExceptions();
}
/*******************************************************************************
 * Changes to this class. What the people have been up to: $Log:
 * WMSGetLegendGraphicRequest.java,v $ Revision 1.5 2004/03/30 07:10:13 poth no
 * message
 * 
 * 
 *  
 ******************************************************************************/