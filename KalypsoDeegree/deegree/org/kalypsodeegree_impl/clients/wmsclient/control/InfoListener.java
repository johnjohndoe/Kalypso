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
package org.deegree_impl.clients.wmsclient.control;

import java.net.MalformedURLException;
import java.util.HashMap;

import org.deegree.enterprise.control.FormEvent;
import org.deegree.services.InconsistentRequestException;
import org.deegree.services.wms.capabilities.Capability;
import org.deegree.services.wms.capabilities.Layer;
import org.deegree.services.wms.capabilities.WMSCapabilities;
import org.deegree.services.wms.protocol.WMSGetFeatureInfoRequest;
import org.deegree.xml.XMLParsingException;
import org.deegree_impl.clients.wmsclient.configuration.Format;
import org.deegree_impl.clients.wmsclient.configuration.MapSize;
import org.deegree_impl.clients.wmsclient.configuration.WMSClientConfiguration;
import org.deegree_impl.clients.wmsclient.model.Constants;
import org.deegree_impl.services.wms.protocol.WMSProtocolFactory;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.StringExtend;

/**
 * will be called if the client forces a feature info action and performs it. A
 * extending class can change the behavior of the the response by overriding the
 * modifyModel() and the modifySettings() methods.
 * 
 * <p>
 * ---------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public class InfoListener extends AbstractMapListener
{
  /**
   * the method will be called by the <tt>MapListener</tt> if a feature info
   * action/event occurs.
   */
  public void actionPerformed( FormEvent event )
  {
    Debug.debugMethodBegin( this, "actionPerformed" );

    super.actionPerformed( event );

    HashMap model = this.toModel();

    // modify the GetMap Request
    WMSGetFeatureInfoRequest fir = null;

    try
    {
      WMSClientConfiguration config = (WMSClientConfiguration)getRequest().getAttribute(
          Constants.WMSCLIENTCONFIGURATION );
      fir = modifyModel( config, model );
      config.setSelectedMapOperation( Constants.WMS_OPERATION_IDENTIFY );
    }
    catch( Exception e )
    {
      System.out.println( this + " " + e );
    }

    this.getRequest().setAttribute( Constants.WMSGETFEATUREINFOREQUEST, fir );
    this.getRequest().setAttribute( Constants.WMSGETMAPREQUEST, fir.getGetMapRequestCopy() );

    Debug.debugMethodEnd();
  }

  /**
   *  
   */
  private WMSGetFeatureInfoRequest modifyModel( WMSClientConfiguration config, HashMap model )
      throws InconsistentRequestException, XMLParsingException, MalformedURLException
  {
    // get GetMap request
    String tmp = (String)model.get( Constants.WMSGETMAPREQUEST );
    HashMap getFI = toMap( tmp );

    // set new map size
    MapSize ms = config.getSelectedMapSize();
    getFI.put( "WIDTH", "" + ms.getWidth() );
    getFI.put( "HEIGHT", "" + ms.getHeight() );

    // set new image/map format
    Format format = config.getSelectedMapFormat();
    getFI.put( "FORMAT", format.getName() );

    // set Layers & styles
    // invert layer order
    tmp = (String)model.get( Constants.LAYERLIST );
    String[] v = StringExtend.toArray( tmp, ",", false );

    WMSCapabilities capa = config.getWMSCapabilities()[0];
    Capability capability = capa.getCapability();
    StringBuffer lay = new StringBuffer( 200 );
    StringBuffer sty = new StringBuffer( 200 );
    StringBuffer qlay = new StringBuffer( 200 );
    for( int i = 0; i < v.length; i++ )
    {
      tmp = v[v.length - 1 - i];
      int pos = tmp.indexOf( '|' );
      Layer layer = null;
      if( pos >= 0 )
      {
        layer = capability.getLayer( tmp.substring( 0, pos ) );
      }
      else
      {
        layer = capability.getLayer( tmp );
      }
      if( layer != null )
      {
        // check if layer is queryable
        if( layer.isQueryable() )
        {
          qlay.append( layer.getName() );
        }
        lay.append( layer.getName() );
        sty.append( tmp.substring( pos + 1, tmp.length() ) );
        if( i < v.length - 1 )
        {
          lay.append( "," );
          sty.append( "," );
          qlay.append( "," );
        }
      }
    }

    getFI.put( "LAYERS", lay.toString() );
    getFI.put( "STYLES", sty.toString() );

    getFI.put( "QUERY_LAYERS", qlay.toString() );
    getFI.put( "INFO_FORMAT", config.getSelectedInfoFormat().getName() );
    getFI.put( "FEATURE_COUNT", "999" );
    getFI.put( "EXCEPTIONS", "application/vnd.ogc.se_xml" );
    int x = (int)Double.parseDouble( (String)model.get( Constants.WMS_X ) );
    getFI.put( "X", x + "" );
    int y = (int)Double.parseDouble( (String)model.get( Constants.WMS_Y ) );
    getFI.put( "Y", y + "" );
    getFI.put( "REQUEST", "GetFeatureInfo" );

    return WMSProtocolFactory.createGetFeatureInfoRequest( "1", getFI );
  }

}