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

import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;

import org.deegree.enterprise.control.FormEvent;
import org.deegree.services.wms.capabilities.Capability;
import org.deegree.services.wms.capabilities.Layer;
import org.deegree.services.wms.capabilities.LegendURL;
import org.deegree.services.wms.capabilities.Style;
import org.deegree.services.wms.capabilities.WMSCapabilities;
import org.deegree_impl.clients.wmsclient.configuration.WMSClientConfiguration;
import org.deegree_impl.clients.wmsclient.model.Constants;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.NetWorker;
import org.deegree_impl.tools.StringExtend;

/**
 * listern class for offering the required parameter for selecting a style for
 * one or more layers.
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public class SelectStyleListener extends AbstractMapListener
{
  /**
   * the method will be called if a reset action/event occurs.
   */
  public void actionPerformed( FormEvent event )
  {
    Debug.debugMethodBegin( this, "actionPerformed" );

    super.actionPerformed( event );

    HashMap model = this.toModel();

    // set Layers & styles
    // invert layer order
    String tmp = (String)model.get( Constants.LAYERLIST );
    String[] v = StringExtend.toArray( tmp, ",", false );

    WMSClientConfiguration config = (WMSClientConfiguration)getRequest().getAttribute(
        Constants.WMSCLIENTCONFIGURATION );
    WMSCapabilities capa = config.getWMSCapabilities()[0];
    Capability capability = capa.getCapability();
    HashMap layerStyle = new HashMap();
    HashMap availableStyles = new HashMap();
    ArrayList list = new ArrayList();
    for( int i = 0; i < v.length; i++ )
    {
      tmp = v[v.length - 1 - i];
      int pos = tmp.indexOf( '|' );
      Layer layer = capability.getLayer( tmp.substring( 0, pos ) );
      if( layer != null )
      {
        list.add( layer );
        layerStyle.put( layer.getName(), tmp.substring( pos + 1, tmp.length() ) );
        // create map of available styles
        Style[] styles = layer.getStyles();
        for( int j = 0; j < styles.length; j++ )
        {
          String s = styles[j].getName();
          if( s.equals( "default" ) )
          {
            s = s + ":" + layer.getName();
          }
          if( layer.getStyleResource( s ) != null )
          {
            Style style = layer.getStyleResource( s );
            LegendURL[] lu = style.getLegendURL();
            if( lu != null && lu.length > 0 )
            {
              URL url = lu[0].getOnlineResource();
              String src = NetWorker.url2String( url );
              availableStyles.put( s, src );
            }
          }
        }
      }
    }

    this.getRequest().setAttribute( "LAYERSTYLE", layerStyle );
    this.getRequest().setAttribute( "LAYERS", list );
    this.getRequest().setAttribute( "AVAILABLESTYLES", availableStyles );

    Debug.debugMethodEnd();
  }
}