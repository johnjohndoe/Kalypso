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
import java.io.FileFilter;

import org.kalypso.java.io.FileUtilities;
import org.kalypso.java.io.filter.AcceptAllFileFilter;
import org.kalypso.repository.AbstractRepository;
import org.kalypso.repository.IRepositoryFactory;
import org.kalypso.repository.IRepositoryItem;

/**
 * Ein File Repository.
 * 
 * @author schlienger
 */
public class FileRepository extends AbstractRepository
{
  protected final File m_root;

  protected final String m_identifier;

  protected final FileFilter m_filter;

  /**
   * Creates a FileRepository.
   * 
   * @param factory
   * @param location
   *          path of the root
   * @param identifier
   *          user defined identifier for this repository
   * @param readOnly
   *          if true the repository is read only
   * @param filter
   *          [optional] if null an <code>AcceptAllFileFilter</code> is used.
   */
  public FileRepository( final IRepositoryFactory factory, final String location,
      final String identifier, final boolean readOnly, final FileFilter filter )
  {
    super( factory, location, readOnly );

    m_identifier = identifier;

    if( filter == null )
      m_filter = new AcceptAllFileFilter();
    else
      m_filter = filter;

    m_root = new File( location );
    if( !m_root.exists() )
      throw new IllegalArgumentException( "Location existiert nicht! (Location: " + location + ")" );

  }

  /**
   * @see FileRepository#FileRepository(IRepositoryFactory, String, String,
   *      boolean, FileFilter)
   * 
   * @param factory
   * @param location
   * @param identifier
   * @param readOnly
   */
  public FileRepository( final IRepositoryFactory factory, final String location,
      final String identifier, final boolean readOnly )
  {
    this( factory, location, identifier, readOnly, null );
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getChildren()
   */
  public IRepositoryItem[] getChildren()
  {
    return createItem( m_root ).getChildren();
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#hasChildren()
   */
  public boolean hasChildren()
  {
    return m_root.isDirectory();
  }

  public FileFilter getFilter()
  {
    return m_filter;
  }

  /**
   * Factory method that can be overriden by subclasses to create adequate
   * items.
   * 
   * @param file
   * @return IRepositoryItem instance
   */
  public FileItem createItem( final File file )
  {
    return new FileItem( this, file );
  }

  /**
   * Returns the URL of the root dir.
   * 
   * @see org.kalypso.repository.IRepository#getIdentifier()
   */
  public String getIdentifier()
  {
    //    try
    //    {
    //      return m_root.toURL().toExternalForm();
    //    }
    //    catch( MalformedURLException e )
    //    {
    //      throw new IllegalStateException( e.getLocalizedMessage() );
    //    }
    return m_identifier;
  }

  /**
   * @see org.kalypso.repository.IRepository#reload()
   */
  public void reload()
  {
  // nothing to do
  }

  /**
   * @see org.kalypso.repository.IRepository#findItem(java.lang.String)
   */
  public IRepositoryItem findItem( final String id )
  {
    // both lowercase to be sure comparison is done homogeneously
    final String baseId = getIdentifier().toLowerCase();
    final String itemId = id.toLowerCase();

    final String scheme = baseId + ":/";

    if( !itemId.startsWith( scheme ) )
      return null;

    // absolute path of the root (replace backslashes on windows with forward
    // slashes)
    final String strRoot = m_root.getAbsolutePath().replace( '\\', '/' );

    // replaceFirst can not handle "$" in itemId, so replaced by next line
    //    final String path = itemId.replaceFirst( scheme, strRoot );
    final String path = strRoot + itemId.replaceFirst( scheme, "" );
    final File f = new File( path );

    if( !f.exists() )
      return null;

    if( !m_filter.accept( f ) )
      return null;

    if( !FileUtilities.isChildOf( m_root, f ) )
      return null;

    return createItem( f );
  }
}