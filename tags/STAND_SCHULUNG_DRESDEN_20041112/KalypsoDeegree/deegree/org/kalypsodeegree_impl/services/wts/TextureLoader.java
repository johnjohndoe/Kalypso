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

import java.awt.Color;
import java.awt.Graphics;
import java.awt.image.BufferedImage;

import org.deegree.model.geometry.GM_Surface;
import org.deegree.services.OGCWebServiceEvent;
import org.deegree.services.wcs.protocol.WCSGetCoverageRequest;
import org.deegree.services.wcs.protocol.WCSGetCoverageResponse;
import org.deegree.services.wts.configuration.WTSConfiguration;
import org.deegree.services.wts.protocol.WTSGetViewRequest;
import org.deegree_impl.services.OGCWebServiceEvent_Impl;
import org.deegree_impl.services.wcs.protocol.WCSProtocolFactory;
import org.deegree_impl.services.wts.configuration.WTSConfiguration_Impl;

/**
 * loader for textures in a seperat thread
 */
class TextureLoader extends Thread
{
  private WTSGetViewRequest request = null;

  private WTService_Impl parent = null;

  private int layerId = 0;

  /**
   * Creates a new TextureLoader object.
   * 
   * @param request
   * @param layerId
   * @param parent
   */
  TextureLoader( WTSGetViewRequest request, int layerId, WTService_Impl parent )
  {
    this.request = request;
    this.parent = parent;
    this.layerId = layerId;
  }

  /**
   *  
   */
  public void run()
  {
    try
    {

      GM_Surface[][] boxes = parent.getBoxes();
      WTSConfiguration conf = WTSConfiguration_Impl.getInstance();

      WTSGetViewRequest.Layer layer = request.getLayers()[layerId];
      int wi = conf.getTileWidth( layer.getName() );
      int hi = conf.getTileHeight( layer.getName() );

      int vwi = request.getWidth();
      int vhi = request.getHeight();
      int dw = calcTileSize( vwi / boxes[0].length );
      int dh = dw;//calcTileSize( vhi / boxes.length );

      System.out.println( "length: " + boxes.length );

      OGCWebServiceEvent event = null;
      WCSGetCoverageRequest wcsReq = null;
      for( int i = 0; i < boxes.length; i++ )
      {
        for( int j = 0; j < boxes[0].length; j++ )
        {
          wcsReq = WCSProtocolFactory.createWCSGetCoverageRequest( "1.0.0", layerId + "-" + i + "-"
              + j, null, layer.getName(), request.getSrs(), request.getSrs(), boxes[i][j]
              .getEnvelope(), null, dw, dh, -1, conf.getFormatName( layer.getName() ), null,
              "application/vnd.ogc.se_xml" );
          if( i > boxes.length - 2 )
          {
            BufferedImage image = new BufferedImage( 2, 2, BufferedImage.TYPE_INT_ARGB );
            Graphics g = image.getGraphics();
            g.setColor( new Color( 50, 140, 130 ) );
            g.fillRect( 0, 0, 2, 2 );
            g.dispose();
            // don't load the last two rows; use a default image
            WCSGetCoverageResponse res = WCSProtocolFactory.createGetCoverageResponse( wcsReq,
                image );
            event = new OGCWebServiceEvent_Impl( this, res, "" );
            parent.write( event );
          }
          else
          {
            event = new OGCWebServiceEvent_Impl( this, wcsReq, "", parent );
            conf.getResponsibleService( layer.getName() ).doService( event );
          }
        }
      }
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }

  private int calcTileSize( int d )
  {
    if( d <= 128 )
    {
      d = 128;
    }
    else if( d <= 256 )
    {
      d = 256;
    }
    else if( d <= 512 )
    {
      d = 512;
    }
    else
    {
      d = 1024;
    }
    return d;
  }

}