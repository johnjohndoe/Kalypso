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
package org.kalypso.project.database.client;

import java.net.MalformedURLException;
import java.net.URL;

import javax.xml.namespace.QName;
import javax.xml.ws.Service;

import org.kalypso.project.database.sei.IProjectDatabase;
import org.kalypso.project.database.server.ProjectDatabase;

/**
 * @author kuch
 */
public class ProjectDatabaseFactory
{
  private static IProjectDatabase m_service;

  public static  IProjectDatabase getService( )
  {
    if( m_service == null )
    {
      try
      {
        final String namespaceURI = "http://server.database.project.kalypso.org/";
        final String serviceImplName = ProjectDatabase.class.getSimpleName();

// final String wsdlLocationProperty = System.getProperty( "kalypso.hwv.observation.service.client.wsdl.location" );
        // TODO: get from outside
        final String wsdlLocationProperty = "http://localhost/projectdb?wsdl";
        final URL wsdlLocation = new URL( wsdlLocationProperty );
        final QName serviceName = new QName( namespaceURI, serviceImplName + "Service" );
        final Service service = Service.create( wsdlLocation, serviceName );

        m_service = service.getPort( new QName( namespaceURI, serviceImplName + "Port" ), IProjectDatabase.class );
      }
      catch( final MalformedURLException e )
      {
        e.printStackTrace();
        return null;
      }
    }

    return m_service;
  }
}
