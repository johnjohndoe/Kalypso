// $Header:
// /cvsroot/deegree/deegree/org/deegree_impl/services/wms/GetScaleBarHandler.java,v
// 1.1 2004/04/02 09:28:54 poth Exp $
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

import java.awt.Graphics;

import org.deegree.graphics.ScaleBar;
import org.deegree.services.WebServiceException;
import org.deegree.services.wms.capabilities.WMSCapabilities;
import org.deegree.services.wms.protocol.WMSGetScaleBarRequest;
import org.deegree.services.wms.protocol.WMSGetScaleBarResponse;
import org.deegree_impl.graphics.ScaleBar_Impl;
import org.deegree_impl.services.wms.protocol.WMSProtocolFactory;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.MimeTypeMapper;

/**
 * performs a GetLegendGraphic request. The capability of the deegree
 * implementation is limited to handle requests containing a named style or
 * using the (named) styles defined in a passed or referenced SLD. featuretype
 * and rule are not supported yet.
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @author last edited by: $Author$
 * 
 * @version 1.0. $Revision$, $Date$
 * 
 * @since 1.1
 */
class GetScaleBarHandler
{

  private WMSCapabilities capabilities = null;

  private WMSGetScaleBarRequest request = null;

  /**
   * Creates a new GetMapHandler object.
   * 
   * @param request
   *          request to perform
   */
  public GetScaleBarHandler( WMSCapabilities capabilities, WMSGetScaleBarRequest request )
      throws WebServiceException
  {
    this.capabilities = capabilities;
    this.request = request;
  }

  /**
   * performs the request and returns the result of it.
   */
  public WMSGetScaleBarResponse performGetScaleBar() throws WebServiceException
  {
    Debug.debugMethodBegin();

    String mime = MimeTypeMapper.toMimeType( request.getFormat() );

    // get target object for rendering
    Object target = GraphicContextFactory.createGraphicTarget( mime, request.getSize(), 50 );
    // get graphic context of the target
    Graphics g = GraphicContextFactory.createGraphicContext( mime, target );
    g.setClip( 0, 0, request.getSize(), 50 );

    try
    {
      ScaleBar sb = new ScaleBar_Impl( request.getTopLabel(), request.getBottomLabel(), request
          .getScale(), request.getScaleDenominator(), request.getUnits(), request.getLabelColor(),
          request.getColor(), request.getBGColor(), request.getStyle(), request.getFont() );
      sb.paint( g );
      g.dispose();
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw new WebServiceException( "couldn't get legendgraphic as image\n" + e );
    }

    WMSGetScaleBarResponse res = WMSProtocolFactory.createGetScaleBarResponse( request, target );

    Debug.debugMethodEnd();
    return res;
  }

}
/*******************************************************************************
 * Changes to this class. What the people have been up to: $Log:
 * GetScaleBarHandler.java,v $ Revision 1.1 2004/04/02 09:28:54 poth no message
 * 
 * Revision 1.3 2004/03/31 15:40:20 poth no message
 * 
 * Revision 1.2 2004/03/31 07:12:07 poth no message
 * 
 * Revision 1.1 2004/03/30 07:09:33 poth no message
 * 
 * 
 *  
 ******************************************************************************/