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
package org.kalypso.ogc.sensor.template;

import java.io.InputStream;
import java.net.URL;

import org.eclipse.core.resources.IEncodedStorage;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;

/**
 * TemplateStorage is a wrapper over an IFile. It delivers additionaly its context and the URL from which the file was
 * retrieved.
 * 
 * @author schlienger
 */
public class TemplateStorage implements IEncodedStorage
{
  private final URL m_context;

  private final IFile m_file;

  private final String m_href;

  public TemplateStorage( final IFile file, final URL context, final String href )
  {
    m_file = file;
    m_context = context;
    m_href = href;
  }

  /**
   * @return Returns the context
   */
  public URL getContext()
  {
    return m_context;
  }

  /**
   * @return Returns the file
   */
  public IFile getFile()
  {
    return m_file;
  }

  /**
   * @return Returns the href
   */
  public String getHref()
  {
    return m_href;
  }

  /**
   * @see org.eclipse.core.resources.IStorage#getContents()
   */
  public InputStream getContents() throws CoreException
  {
    return m_file.getContents();
  }

  /**
   * @see org.eclipse.core.resources.IStorage#getFullPath()
   */
  public IPath getFullPath()
  {
    return m_file.getFullPath();
  }

  /**
   * @see org.eclipse.core.resources.IStorage#getName()
   */
  public String getName()
  {
    return m_file.getName();
  }

  /**
   * @see org.eclipse.core.resources.IStorage#isReadOnly()
   */
  public boolean isReadOnly()
  {
    return false;
  }

  /**
   * @see org.eclipse.core.runtime.IAdaptable#getAdapter(java.lang.Class)
   */
  public Object getAdapter( Class adapter )
  {
    return null;
  }

  /**
   * @see org.eclipse.core.resources.IEncodedStorage#getCharset()
   */
  public String getCharset() throws CoreException
  {
    return m_file.getCharset();
  }
}