/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

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

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ui.metadoc.util;

import java.io.IOException;

import javax.activation.DataHandler;
import javax.activation.FileDataSource;
import javax.xml.rpc.ServiceException;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.java.lang.reflect.ClassUtilities;
import org.kalypso.metadoc.Document;
import org.kalypso.services.ProxyFactory;
import org.kalypso.services.proxy.IMetaDocService;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * Wrapps the service to provide utility functions
 * 
 * @author belger
 */
public class MetadocServiceWrapper
{
  private final IMetaDocService m_service;

  /**
   * @throws CoreException
   */
  public MetadocServiceWrapper() throws CoreException
  {
    try
    {
      final ProxyFactory serviceProxyFactory = KalypsoGisPlugin.getDefault().getServiceProxyFactory();
      m_service = (IMetaDocService)serviceProxyFactory.getAnyProxy( "Kalypso_MetaDocService", ClassUtilities
          .getOnlyClassName( IMetaDocService.class ) );
    }
    catch( final ServiceException e )
    {
      e.printStackTrace();
      throw new CoreException( KalypsoGisPlugin.createErrorStatus(
          "Berichtsablage-Dienst konnte nicht initialisiert werden", e ) );
    }
  }

  /**
   * @param fileExtension
   *          Extension with '.' (e.g. '.csv')
   * @param username
   * @return the prepared document
   * @throws CoreException
   */
  public Document prepareDocument( final String fileExtension, final String username ) throws CoreException
  {
    try
    {
      return new Document( fileExtension, m_service.prepareNewDocument( username ) );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new CoreException( KalypsoGisPlugin.createErrorStatus( "Fehler beim Aufruf des Berichtsablage-Dienstes", e ) );
    }
  }

  public void commitDocument( final Document doc ) throws CoreException
  {
    try
    {
      m_service.commitNewDocument( doc.getMetadata(), new DataHandler( new FileDataSource( doc.getFile() ) ), doc
          .getFileExtension() );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      throw new CoreException( KalypsoGisPlugin.createErrorStatus( "Berichtsablage gescheitert", e ) );
    }
  }
}