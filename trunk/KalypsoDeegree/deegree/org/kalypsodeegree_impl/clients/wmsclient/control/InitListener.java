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

import org.deegree.enterprise.control.FormEvent;
import org.deegree_impl.clients.wmsclient.configuration.WMSClientConfiguration;
import org.deegree_impl.clients.wmsclient.model.Constants;
import org.deegree_impl.enterprise.control.AbstractListener;
import org.deegree_impl.services.wms.protocol.WMSProtocolFactory;

/**
 * 
 * 
 * <p>
 * ---------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public class InitListener extends AbstractListener
{
  /**
   * 
   * 
   * @param event
   */
  public void actionPerformed( FormEvent event )
  {
    // get default client configuration
    WMSClientConfiguration defaultConfig = MapApplicationHandler.getDefaultClientConfiguration();
    this.getRequest().setAttribute( Constants.WMSCLIENTCONFIGURATION, defaultConfig );

    HashMap model = this.toModel();

    if( model.get( "BBOX" ) != null )
    {

      try
      {
        model.put( "BBOX", model.get( "BBOX" ) );
        this.getRequest().setAttribute( Constants.WMSGETMAPREQUEST,
            WMSProtocolFactory.createGetMapRequest( "1", model ) );
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }

    }
    else
    {
      this.getRequest().setAttribute( Constants.WMSGETMAPREQUEST,
          defaultConfig.getInitialGetMapRequest() );
    }

  }

  /**
   * maps a string representation of a request to a <tt>HashMap</tt>
   */
  protected HashMap toMap( String request )
  {
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

}

/*
 * Changes to this class. What the people haven been up to:
 * 
 * $Log$
 * Revision 1.2  2004/08/30 00:36:40  doemming
 * *** empty log message ***
 * Revision 1.1.1.1 2004/05/11 16:43:27 doemming
 * backup of local modified deegree sources
 * 
 * Revision 1.6 2004/02/23 07:47:47 poth no message
 * 
 * Revision 1.5 2004/02/19 10:08:56 poth no message
 * 
 * Revision 1.4 2004/02/09 07:59:20 poth no message
 * 
 * Revision 1.3 2004/01/03 13:46:45 poth no message
 * 
 * Revision 1.2 2003/11/10 10:59:43 poth no message
 * 
 * Revision 1.1 2003/07/11 12:29:58 poth no message
 * 
 * Revision 1.2 2002/03/25 08:34:59 ap no message 8 Revision 1.1.1.1 2002/03/22
 * 15:23:10 ap no message
 * 
 * Revision 1.2 2001/08/08 15:00:59 ap no message
 * 
 *  
 */
