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

import java.util.HashMap;
import java.util.StringTokenizer;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.deegree.enterprise.control.FormEvent;
import org.deegree.services.wms.protocol.WMSGetMapRequest;
import org.deegree_impl.clients.wmsclient.configuration.WMSClientConfiguration;
import org.deegree_impl.clients.wmsclient.model.Constants;
import org.deegree_impl.enterprise.control.AbstractListener;
import org.deegree_impl.tools.Debug;

/**
 * Basic class for all listerens that shall be notified if a map cliented raises
 * an action/event.
 * <p>
 * </p>
 * There are several predefined listeres for actions that most map clients
 * support:
 * <ul>
 * <li><tt>ZoomInListener</tt> for handling zoomin actions. supported are
 * zooming via point or vie rectangle.
 * <li><tt>ZoomOutListener</tt> for handling zoomout action.
 * <li><tt>PanListener</tt> for handling of pan action. supported is panning
 * to eight directions.
 * <li><tt>RecenterListener</tt> recenters the map to a specified point. This
 * can be interpreted as a special versio of zooming
 * <li><tt>RefreshListener</tt> reloads the map without any change
 * <li><tt>ResetListener</tt> recovers the initial status of the map
 * <li><tt>InfoListener</tt> will be notified if a feature info request
 * should be send.
 * </ul>
 * The user can additional listeners/action by extending the
 * <tt>AbstractActionListener</tt> class or one of the predefined listener.
 * <p>
 * </p>
 * Each Listerner have to be registered to the <tt>MapListener</tt> which is
 * the class that will be informed about each event/action within a map client.
 * To register a class as listener it has to stored within the
 * MapListener.ConfigurationFile.
 * 
 * <p>
 * ---------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public abstract class AbstractMapListener extends AbstractListener
{

  /**
   * 
   * 
   * @param event
   */
  public void actionPerformed( FormEvent event )
  {
    Debug.debugMethodBegin( this, "actionPerformed" );

    HttpSession session = ( (HttpServletRequest)getRequest() ).getSession();
    // get configuration from the users session
    WMSClientConfiguration config = (WMSClientConfiguration)session
        .getAttribute( Constants.WMSCLIENTCONFIGURATION );

    // if no configuration is stored in the session get default config
    if( config == null )
    {
      // get default client configuration
      config = MapApplicationHandler.getDefaultClientConfiguration();

      // add a deep copy of the configuration to the request so that the
      // may change it without changing the original default configuration
      config = (WMSClientConfiguration)config.clone();
    }
    this.getRequest().setAttribute( Constants.WMSCLIENTCONFIGURATION, config );

    Debug.debugMethodEnd();
  }

  /**
   * The default listeners (s.o.) are calling this method within the
   * actionPerformed method. A user can extend a predefined listener and the
   * override this method to modify the LegendModel. If the method will be
   * overwritten at first <tt>super.modifyLegendModel(..)</tt> shall be
   * called.
   */

  //    protected LegendModel modifyLegendModel( LegendModel lm, MapRequestModel
  // mrm ) {
  //        return lm;
  //    }
  /**
   * maps a string representation of a request to a <tt>HashMap</tt>
   */
  protected HashMap toMap( String request )
  {
    int p = request.indexOf( '?' );
    if( p >= 0 )
    {
      request = request.substring( p + 1, request.length() );
    }
    StringTokenizer st = new StringTokenizer( request, "&" );
    HashMap map = new HashMap();

    while( st.hasMoreTokens() )
    {
      String s = st.nextToken();
      int pos = s.indexOf( '=' );
      String s1 = s.substring( 0, pos );
      String s2 = s.substring( pos + 1, s.length() );
      map.put( s1.toUpperCase(), s2 );
    }

    return map;
  }

  /**
   * the method returns the scale of the map defined as diagonal size of a pixel
   * at the center of the map.
   */
  protected double getScale( WMSGetMapRequest mrm )
  {

    double bwidth = mrm.getBoundingBox().getWidth();
    double bheight = mrm.getBoundingBox().getHeight();
    double width = mrm.getWidth();
    double height = mrm.getHeight();

    double sx = Math.sqrt( bwidth * bwidth + bheight * bheight );
    double px = Math.sqrt( width * width + height * height );

    return sx / px;
  }
}