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
package org.kalypso.repository.file;

import java.io.File;

import org.kalypso.java.io.FileUtilities;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.IRepositoryItem;

/**
 * An item of a <code>FileRepository</code> that represents a
 * <code>File</code>.
 * 
 * @author schlienger
 */
public class FileItem implements IRepositoryItem
{
  private final static IRepositoryItem[] EMPTY_ITEMS = new IRepositoryItem[0];

  private final FileRepository m_rep;

  private final File m_file;

  public FileItem( final FileRepository rep, final File file )
  {
    m_rep = rep;
    m_file = file;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getName()
   */
  public String getName( )
  {
    return m_file.getName();
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getParent()
   */
  public IRepositoryItem getParent( )
  {
    return m_rep.createItem( m_file.getParentFile() );
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getChildren()
   */
  public IRepositoryItem[] getChildren( )
  {
    final File[] files = m_file.listFiles( m_rep.getFilter() );

    if( files == null )
      return EMPTY_ITEMS;

    final IRepositoryItem[] items = new IRepositoryItem[files.length];

    for( int i = 0; i < items.length; i++ )
      items[i] = m_rep.createItem( files[i] );

    return items;
  }

  public File getFile( )
  {
    return m_file;
  }

  public FileRepository getRep( )
  {
    return m_rep;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#hasChildren()
   */
  public boolean hasChildren( )
  {
    return m_file.isDirectory();
  }

  public String toString( )
  {
    return getName();
  }

  /**
   * @see org.kalypso.util.adapter.IAdaptable#getAdapter(java.lang.Class)
   */
  public Object getAdapter( Class anotherClass )
  {
    if( anotherClass == File.class )
      return m_file;

    return null;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getRepository()
   */
  public IRepository getRepository( )
  {
    return m_rep;
  }

  /**
   * Returns the identifier of the FileRepository and the relative path of the
   * file in the repository
   * 
   * @see org.kalypso.repository.IRepositoryItem#getIdentifier()
   */
  public String getIdentifier( )
  {
    return m_rep.getIdentifier()
        + ":/"
        + FileUtilities.getRelativePathTo( m_rep.m_root, m_file ).replace('\\', '/');
  }
}