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

import java.io.OutputStream;
import java.util.HashMap;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.services.proxy.DocBean;
import org.kalypso.ui.metadoc.IExportableDocument;

/**
 * @author belger
 */
public class MultiDocumentServiceWrapper
{
  private final IExportableDocument m_dummyDoc = new IExportableDocument()
  {
    public void exportDocument( final OutputStream outs ) throws Exception
    {
    // ignore
    }

    public String getDocumentExtension()
    {
      return ".dummy";
    }
  };
  
  private MetadocServiceWrapper m_wrapper;

  private DocBean m_dummybean;

  private String m_username;

  
  public MultiDocumentServiceWrapper() throws CoreException
  {
    m_username = System.getProperty( "user.name" );
    m_wrapper = new MetadocServiceWrapper(  );
    m_dummybean = m_wrapper.prepareDocument( m_dummyDoc.getDocumentExtension(), m_username );
  }


  public IExportableDocument getDummyDoc()
  {
    return m_dummyDoc;
  }

  public DocBean getDummyBean()
  {
    return m_dummybean;
  }

  public void dispose()
  {
    try
    {
      m_wrapper.cancelData( m_dummybean );
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
    }
  }
  
  
  public DocBean getCopyBean( final String extension ) throws CoreException
  {
    final DocBean newbean = m_wrapper.prepareDocument( extension, m_username );
    newbean.setMetadata( new HashMap( m_dummybean.getMetadata() ) );
    return newbean;
  }

  public void commitBean( final DocBean doc ) throws CoreException
  {
    m_wrapper.commitData( doc );
  }

  public void cancelBean( final DocBean doc ) throws CoreException
  {
    m_wrapper.cancelData( doc );
  }
}
