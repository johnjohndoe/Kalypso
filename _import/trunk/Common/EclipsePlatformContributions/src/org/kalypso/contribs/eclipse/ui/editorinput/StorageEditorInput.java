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
package org.kalypso.contribs.eclipse.ui.editorinput;

import org.eclipse.core.resources.IStorage;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.IPersistableElement;
import org.eclipse.ui.IStorageEditorInput;

/**
 * TODO: normally should also override equals() according to interface specification... Check if this is possible here.
 * 
 * @author belger
 */
public class StorageEditorInput implements IStorageEditorInput
{
  private final IStorage m_storage;

  public StorageEditorInput( final IStorage storage )
  {
    m_storage = storage;
  }

  /**
   * @see org.eclipse.ui.IStorageEditorInput#getStorage()
   */
  public IStorage getStorage()
  {
    return m_storage;
  }

  /**
   * @see org.eclipse.ui.IEditorInput#exists()
   */
  public boolean exists()
  {
    return true;
  }

  /**
   * @see org.eclipse.ui.IEditorInput#getImageDescriptor()
   */
  public ImageDescriptor getImageDescriptor()
  {
    return null;
  }

  /**
   * @see org.eclipse.ui.IEditorInput#getName()
   */
  public String getName()
  {
    return m_storage.getName();
  }

  /**
   * @see org.eclipse.ui.IEditorInput#getPersistable()
   */
  public IPersistableElement getPersistable()
  {
    return null;
  }

  /**
   * @see org.eclipse.ui.IEditorInput#getToolTipText()
   */
  public String getToolTipText()
  {
    // null not allowed, so return name of storage
    return m_storage.getName();
  }

  /**
   * @see org.eclipse.core.runtime.IAdaptable#getAdapter(java.lang.Class)
   */
  public Object getAdapter( Class adapter )
  {
    return null;
  }
}