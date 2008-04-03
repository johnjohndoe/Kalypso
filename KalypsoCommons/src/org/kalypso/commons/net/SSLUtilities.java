/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.commons.net;

import java.net.URL;

import org.apache.commons.httpclient.contrib.ssl.AuthSSLProtocolSocketFactory;
import org.apache.commons.httpclient.contrib.ssl.EasySSLProtocolSocketFactory;
import org.apache.commons.httpclient.protocol.Protocol;
import org.apache.commons.httpclient.protocol.ProtocolSocketFactory;

/**
 * This class should help configuring a http client with ssl capabilities.
 * 
 * @author Holger Albert
 */
public class SSLUtilities
{
  /**
   * The constructor.
   */
  private SSLUtilities( )
  {
  }

  /**
   * This function will register the https protocol, which will accept all certificates.<br>
   * This setting will apply, as long as your application is running.
   */
  public static void acceptSelfSignedCertificatesSSL( )
  {
    ProtocolSocketFactory easyfactory = new EasySSLProtocolSocketFactory();
    Protocol easyhttps = new Protocol( "https", easyfactory, 443 );
    Protocol.registerProtocol( "https", easyhttps );
  }

  /**
   * This function configures the whole thing, provided a key- and truststore are available.
   * 
   * @param keyStore
   *            The keystore.
   * @param keyPassphrase
   *            The passphrase of the client certificate.
   * @param trustStore
   *            The truststore.
   * @param trustPassphrase
   */
  public static void configureWhole( URL keyStore, String keyPassphrase, URL trustStore, String trustPassphrase ) throws Exception
  {
    ProtocolSocketFactory authfactory = new AuthSSLProtocolSocketFactory( keyStore, keyPassphrase, trustStore, trustPassphrase );
    Protocol authhttps = new Protocol( "https", authfactory, 443 );
    Protocol.registerProtocol( "https", authhttps );
  }
}