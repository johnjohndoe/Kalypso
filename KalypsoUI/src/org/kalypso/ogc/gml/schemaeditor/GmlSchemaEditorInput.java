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
package org.kalypso.ogc.gml.schemaeditor;

import java.net.URL;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IPersistableElement;

/**
 * A schema editor input with namespace and schemalocation
 * 
 * @author thuel2
 */
public class GmlSchemaEditorInput implements IEditorInput
{
  private final String m_namespace;

  private final URL m_location;

  private final String m_gmlVersion;

  public GmlSchemaEditorInput( final String namespace, final String gmlVersion, final URL location )
  {
    m_namespace = namespace;
    m_gmlVersion = gmlVersion;
    m_location = location;
  }

  /**
   * @see org.eclipse.ui.IEditorInput#exists()
   */
  public boolean exists( )
  {
    return false;
  }

  /**
   * @see org.eclipse.ui.IEditorInput#getImageDescriptor()
   */
  public ImageDescriptor getImageDescriptor( )
  {
    return null;
  }

  /**
   * @see org.eclipse.ui.IEditorInput#getName()
   */
  public String getName( )
  {
    return m_namespace;
  }

  /**
   * @see org.eclipse.ui.IEditorInput#getPersistable()
   */
  public IPersistableElement getPersistable( )
  {
    return null;
  }

  /**
   * @see org.eclipse.ui.IEditorInput#getToolTipText()
   */
  public String getToolTipText( )
  {
    return m_namespace + " - " + m_location;
  }

  /**
   * @see org.eclipse.core.runtime.IAdaptable#getAdapter(java.lang.Class)
   */
  public Object getAdapter( final Class adapter )
  {
    return null;
  }

  public String getNamespace( )
  {
    return m_namespace;
  }

  public URL getLocation( )
  {
    return m_location;
  }

  public String getGmlVersion( )
  {
    return m_gmlVersion;
  }

}
