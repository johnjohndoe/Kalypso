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

import java.net.URL;
import java.net.URLEncoder;
import java.util.HashMap;

import org.deegree.services.WebServiceException;
import org.deegree.services.wms.protocol.WMSGetLegendGraphicRequest;
import org.deegree_impl.services.OGCWebServiceRequest_Impl;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.NetWorker;

/**
 * 
 * 
 * <p>
 * --------------------------------------------------------
 * </p>
 * 
 * @author Katharina Lupp <a href="mailto:k.lupp@web.de">Katharina Lupp </a>
 * @version $Revision$ $Date$
 */
public class WMSGetLegendGraphicRequest_Impl extends OGCWebServiceRequest_Impl implements
    WMSGetLegendGraphicRequest
{
  private String rule = null;

  private String sLD_Body = null;

  private String featureType = null;

  private String format = null;

  private String layer = null;

  private URL sLD = null;

  private String style = null;

  private double scale = 0;

  private int width = 0;

  private int height = 0;

  private String exceptions = null;

  /**
   * Creates a new WMSGetLegendGraphicRequest_Impl object.
   * 
   * @param layer
   * @param style
   * @param featureType
   * @param rule
   * @param scale
   * @param sLD
   * @param sLD_Body
   * @param format
   * @param version
   * @param id
   * @param vendorSpecificParameter
   */
  WMSGetLegendGraphicRequest_Impl( String id, String version, String layer, String style,
      String featureType, String rule, double scale, URL sLD, String sLD_Body, String format,
      int width, int height, String exceptions, HashMap vendorSpecificParameter )
  {
    super( "GetLegendGraphicRequest", "WMS", version, id, vendorSpecificParameter );
    setLayer( layer );
    setStyle( style );
    setFeatureType( featureType );
    setRule( rule );
    setScale( scale );
    setSLD( sLD );
    setSLD_Body( sLD_Body );
    setFormat( format );
    this.width = width;
    this.height = height;
    this.exceptions = exceptions;
  }

  /**
   * returns the <Layer>. A Map Server MUST include at least one <Layer>element
   * for each map layer offered. If desired, data layers MAY be repeated in
   * different categories when relevant. A Layer element MAY state the Name by
   * which a map of the layer is requested, MUST give a Title to be used in
   * human-readable menus, and MAY include: a human-readable Abstract containing
   * further description, available Spatial Reference Systems (SRS), bounding
   * boxes in Lat/Lon and SRS-specific coordinates indicating the available
   * geographic coverage, styles in which the layer is available, a URL for more
   * information about the data, and a hint concerning appropriate map scales
   * for displaying this layer. Use of the nesting hierarchy is optional.
   */
  public String getLayer()
  {
    return layer;
  }

  /**
   * sets the <Layer>
   */
  public void setLayer( String layer )
  {
    this.layer = layer;
  }

  /**
   * returns the <Style>. Named style that can be used for rendering the layer.
   */
  public String getStyle()
  {
    return style;
  }

  /**
   * sets the <Style>
   */
  public void setStyle( String style )
  {
    this.style = style;
  }

  /**
   * returns the <FeatureType>
   */
  public String getFeatureType()
  {
    return featureType;
  }

  /**
   * sets the <FeatureType>
   */
  public void setFeatureType( String featureType )
  {
    this.featureType = featureType;
  }

  /**
   * returns the <Rule>
   */
  public String getRule()
  {
    return rule;
  }

  /**
   * sets the <Rule>
   */
  public void setRule( String rule )
  {
    this.rule = rule;
  }

  /**
   * returns the <Scale>. Comma-seperated min and max scale values of a layer.
   */
  public double getScale()
  {
    return scale;
  }

  /**
   * sets the <Scale>. Comma-seperated min and max scale values of a layer.
   */
  public void setScale( double scale )
  {
    this.scale = scale;
  }

  /**
   * returns a reference (URL) to a SLD document
   */
  public URL getSLD()
  {
    return sLD;
  }

  /**
   * sets a reference (URL) to a SLD document
   */
  public void setSLD( URL sLD )
  {
    this.sLD = sLD;
  }

  /**
   * returns the body of a SLD document. If SLD_BODY parameter is set, the SLD
   * parameter isn't set and vice versa
   */
  public String getSLD_Body()
  {
    return sLD_Body;
  }

  /**
   * sets the body of a SLD document. If SLD_BODY parameter is set, the SLD
   * parameter isn't set and vice versa
   */
  public void setSLD_Body( String sLD_Body )
  {
    this.sLD_Body = sLD_Body;
  }

  /**
   * returns the name of the image format the legend graphics shall have
   */
  public String getFormat()
  {
    return format;
  }

  /**
   * sets the name of the image format the legend graphics shall have
   */
  public void setFormat( String format )
  {
    this.format = format;
  }

  /**
   * This gives a hint for the height of the returned graphic in pixels.
   * Vector-graphics can use this value as a hint for the level of detail to
   * include.
   *  
   */
  public int getHeight()
  {
    return height;
  }

  /**
   * @see WMSGetLegendGraphicRequest_Impl#getHeight()
   * @param height
   */
  public void setHeight( int height )
  {
    this.height = height;
  }

  /**
   * This gives a hint for the width of the returned graphic in pixels.
   * Vector-graphics can use this value as a hint for the level of detail to
   * include.
   *  
   */
  public int getWidth()
  {
    return width;
  }

  /**
   * @see WMSGetLegendGraphicRequest_Impl#getWidth()
   * @param width
   */
  public void setWidth( int width )
  {
    this.width = width;
  }

  /**
   * This gives the MIME type of the format in which to return exceptions.
   * Allowed values are the same as for the EXCEPTIONS= parameter of the WMS
   * GetMap request.
   *  
   */
  public String getExceptions()
  {
    return exceptions;
  }

  /**
   * @see WMSGetLegendGraphicRequest_Impl#getExceptions()
   * @param exceptions
   */
  public void setExceptions( String exceptions )
  {
    this.exceptions = exceptions;
  }

  /**
   * method for creating a OGC WMS 1.1.1 conform legend graphic request
   */
  public String getRequestParameter() throws WebServiceException
  {

    Debug.debugMethodBegin( "WMSGetMapRequest", "getRequestParameter" );

    StringBuffer url = new StringBuffer( "SERVICE=WMS?VERSION=" + getVersion() );
    url.append( "&LAYER=" + getLayer() );

    if( getStyle() != null && getStyle().length() > 0 )
    {
      url.append( "&STYLE=" + getStyle() );
    }

    if( getFeatureType() != null && getFeatureType().length() > 0 )
    {
      url.append( "&FEATURETYPE=" + getFeatureType() );
    }

    if( getSLD() != null )
    {
      url.append( "&SLD=" + NetWorker.url2String( getSLD() ) );
    }
    else if( getSLD_Body() != null )
    {
      String tmp = null;
      try
      {
        tmp = URLEncoder.encode( getSLD_Body(), "ISO-8859-1" );
      }
      catch( Exception e )
      {
        throw new WebServiceException( e.toString() );
      }
      url.append( "&SLD_BODY=" + tmp );
    }

    url.append( "&FORMAT=" + getFormat() );
    url.append( "&WIDTH=" + getWidth() );
    url.append( "&HEIGHT=" + getHeight() );

    if( ( getExceptions() != null ) && ( getExceptions().length() > 0 ) )
    {
      url.append( "&EXCEPTIONS=" + getExceptions() );
    }

    Debug.debugMethodEnd();

    return url.toString();
  }

}