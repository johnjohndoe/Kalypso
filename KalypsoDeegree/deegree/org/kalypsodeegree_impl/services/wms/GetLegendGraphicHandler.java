// $Header:
// /cvsroot/deegree/deegree/org/deegree_impl/services/wms/GetLegendGraphicHandler.java,v
// 1.8 2004/08/26 12:42:21 poth Exp $
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

import java.awt.image.BufferedImage;
import java.io.InputStreamReader;

import org.deegree.graphics.legend.LegendElement;
import org.deegree.graphics.sld.StyledLayerDescriptor;
import org.deegree.graphics.sld.UserStyle;
import org.deegree.services.WebServiceException;
import org.deegree.services.wms.LayerNotDefinedException;
import org.deegree.services.wms.StyleNotDefinedException;
import org.deegree.services.wms.capabilities.Layer;
import org.deegree.services.wms.capabilities.LegendURL;
import org.deegree.services.wms.capabilities.WMSCapabilities;
import org.deegree.services.wms.protocol.WMSGetLegendGraphicRequest;
import org.deegree.services.wms.protocol.WMSGetLegendGraphicResponse;
import org.deegree_impl.graphics.legend.LegendFactory;
import org.deegree_impl.graphics.sld.SLDFactory;
import org.deegree_impl.services.wms.protocol.WMSProtocolFactory;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.ImageUtils;
import org.deegree_impl.tools.StringExtend;

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
class GetLegendGraphicHandler
{

  private WMSCapabilities capabilities = null;

  private StyledLayerDescriptor sld = null;

  private WMSGetLegendGraphicRequest request = null;

  /**
   * Creates a new GetMapHandler object.
   * 
   * @param request
   *          request to perform
   */
  public GetLegendGraphicHandler( WMSCapabilities capabilities, WMSGetLegendGraphicRequest request )
      throws WebServiceException
  {
    this.capabilities = capabilities;
    this.request = request;
  }

  /**
   * performs the request and returns the result of it.
   */
  public WMSGetLegendGraphicResponse performGetLegendGraphic() throws WebServiceException
  {
    Debug.debugMethodBegin();

    validate( request );
    LegendElement lege = getSymbol( request );
    BufferedImage bi = null;
    try
    {
      bi = lege.exportAsImage();
    }
    catch( Exception e )
    {
      throw new WebServiceException( "couldn't get legendgraphic as image\n"
          + StringExtend.stackTraceToString( e.getStackTrace() ) );
    }

    WMSGetLegendGraphicResponse res = WMSProtocolFactory.createGetLegendGraphicResponse( request,
        bi );

    Debug.debugMethodEnd();
    return res;
  }

  /**
   * validates if the passed request is valid against the WMS it was sent to and
   * the SLD maybe contained or referenced in/by the request.
   * 
   * @param request
   *          request to validate
   */
  private void validate( WMSGetLegendGraphicRequest request ) throws WebServiceException
  {
    Debug.debugMethodBegin();

    String layerName = request.getLayer();
    String style = request.getStyle();

    if( request.getSLD() == null && request.getSLD_Body() == null )
    {
      Layer layer = capabilities.getCapability().getLayer( layerName );
      if( layer == null )
      {
        throw new LayerNotDefinedException( "Layer: " + layerName + " isn not defined by the WMS" );
      }
      if( getNamedStyle( style ) == null )
      {
        throw new StyleNotDefinedException( "Style: " + style + " is not defined by the WMS" );
      }
    }
    else
    {
      try
      {
        if( request.getSLD() != null )
        {
          InputStreamReader isr = new InputStreamReader( request.getSLD().openStream(), "UTF-8" );
          sld = SLDFactory.createSLD( isr );
        }
        else
        {
          sld = SLDFactory.createSLD( request.getSLD_Body() );
        }
        // check if layer and style are present
        org.deegree.graphics.sld.Layer[] sldLayers = sld.getLayers();
        boolean found = false;
        for( int i = 0; i < sldLayers.length; i++ )
        {
          if( layerName.equals( sldLayers[i].getName() ) )
          {
            org.deegree.graphics.sld.Style[] sldStyles = sldLayers[i].getStyles();
            for( int k = 0; k < sldStyles.length; k++ )
            {
              if( sldStyles[k].getName().equals( style ) )
              {
                found = true;
                break;
              }
            }
            if( found )
              break;
          }
        }
        if( !found )
        {
          throw new WebServiceException( "Layer: " + layerName + " isn not "
              + "defined is the passed/referenced SLD" );
        }

      }
      catch( Exception e )
      {
        throw new WebServiceException( "Invalid SLD or SLD reference\n"
            + StringExtend.stackTraceToString( e.getStackTrace() ) );
      }

    }

    Debug.debugMethodEnd();
  }

  private org.deegree.services.wms.capabilities.Style getNamedStyle( String name )
  {
    String layerName = request.getLayer();
    Layer layer = capabilities.getCapability().getLayer( layerName );
    org.deegree.services.wms.capabilities.Style[] styles = layer.getStyles();
    for( int i = 0; i < styles.length; i++ )
    {
      if( styles[i].getName().equals( name ) )
      {
        return styles[i];
      }
    }
    return null;
  }

  /**
   * @param request
   * @return @throws
   *         WebServiceException
   */
  private LegendElement getSymbol( WMSGetLegendGraphicRequest request ) throws WebServiceException
  {
    Debug.debugMethodBegin();

    LegendElement le = null;
    try
    {
      if( request.getSLD() == null && request.getSLD_Body() == null )
      {
        le = getFromWellKnownStyle();
      }
      else
      {
        le = getFromSLDStyle();
      }
    }
    catch( Exception e )
    {
      throw new WebServiceException( "couldn't create LegendElement\n"
          + StringExtend.stackTraceToString( e.getStackTrace() ) );
    }

    Debug.debugMethodEnd();
    return le;
  }

  /**
   * creates a LegendElement from a style known by the WMS
   */
  private LegendElement getFromWellKnownStyle() throws WebServiceException
  {
    Debug.debugMethodBegin();

    String layerName = request.getLayer();
    String styleName = request.getStyle();
    LegendElement le = null;
    LegendFactory lf = new LegendFactory();

    try
    {
      // get Layer object from the WMS capabilities
      Layer layer = capabilities.getCapability().getLayer( layerName );
      // get the Style section from the matching the requested style
      org.deegree.services.wms.capabilities.Style nStyle = getNamedStyle( styleName );
      LegendURL[] lURLs = nStyle.getLegendURL();
      // if a legend url is defined will be used for creating the legend
      // symbol; otherwise it will be tried to create the legend symbol
      // dynamicly
      try
      {
        BufferedImage bi = ImageUtils.loadImage( lURLs[0].getOnlineResource() );
        le = lf.createLegendElement( bi );
      }
      catch( Exception e )
      {
        UserStyle style = layer.getStyle( styleName );
        if( style != null )
        {
          String title = capabilities.getCapability().getLayer( layerName ).getTitle();
          le = lf.createLegendElement( style, request.getWidth(), request.getHeight(), title );
        }
        else
        {
          throw new WebServiceException( "no well known style, no SLD style "
              + "and no valid LegendURL defied for style: " + styleName );
        }
      }
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw new WebServiceException( StringExtend.stackTraceToString( e.getStackTrace() ) );
    }

    Debug.debugMethodEnd();
    return le;
  }

  /**
   * creates a LegendElement from a style defined in the SLD document
   * passed/referenced by/in the request
   */
  private LegendElement getFromSLDStyle() throws WebServiceException
  {
    Debug.debugMethodBegin();

    String layerName = request.getLayer();
    String styleName = request.getStyle();
    LegendElement le = null;
    LegendFactory lf = new LegendFactory();

    try
    {
      org.deegree.graphics.sld.Layer[] sldLayers = sld.getLayers();
      for( int i = 0; i < sldLayers.length; i++ )
      {
        if( layerName.equals( sldLayers[i].getName() ) )
        {
          org.deegree.graphics.sld.Style[] sldStyles = sldLayers[i].getStyles();
          org.deegree.graphics.sld.Style style = null;
          if( styleName == null )
          {
            style = sldStyles[0];
          }
          else
          {
            for( int k = 0; k < sldStyles.length; k++ )
            {
              if( sldStyles[k].getName().equals( styleName ) )
              {
                style = sldStyles[k];
                break;
              }
            }
          }
          String title = capabilities.getCapability().getLayer( layerName ).getTitle();
          le = lf.createLegendElement( style, request.getWidth(), request.getHeight(), title );
        }
      }
    }
    catch( Exception e )
    {
      throw new WebServiceException( StringExtend.stackTraceToString( e.getStackTrace() ) );
    }

    Debug.debugMethodEnd();
    return le;
  }

}
/*******************************************************************************
 * Changes to this class. What the people have been up to: $Log:
 * GetLegendGraphicHandler.java,v $ Revision 1.8 2004/08/26 12:42:21 poth no
 * message
 * 
 * Revision 1.7 2004/08/10 10:31:27 poth no message
 * 
 * Revision 1.6 2004/07/20 07:05:24 poth no message
 * 
 * Revision 1.5 2004/04/07 06:43:50 poth no message
 * 
 * Revision 1.4 2004/04/02 06:41:56 poth no message
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