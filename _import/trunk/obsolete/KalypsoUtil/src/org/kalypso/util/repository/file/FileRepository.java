package org.kalypso.util.repository.file;

import java.io.File;
import java.io.FileFilter;

import org.kalypso.java.io.filter.AcceptAllFileFilter;
import org.kalypso.util.repository.AbstractRepository;
import org.kalypso.util.repository.IRepositoryItem;

/**
 * Ein File Repository.
 * 
 * @author schlienger
 */
public class FileRepository extends AbstractRepository
{
  protected final File m_root;

  protected final FileFilter m_filter;

  public FileRepository( final String location, final FileFilter filter )
  {
    super( location );

    if( filter == null )
      m_filter = new AcceptAllFileFilter();
    else
      m_filter = filter;

    m_root = new File( location );
    if( !m_root.exists() )
      throw new IllegalArgumentException( "Location existiert nicht! (Location: " + location + ")" );
  }

  public FileRepository( final String location )
  {
    this( location, null );
  }

  /**
   * @see org.kalypso.util.repository.IRepositoryItem#getChildren()
   */
  public IRepositoryItem[] getChildren()
  {
    return createItem( m_root ).getChildren();
  }

  /**
   * @see org.kalypso.util.repository.IRepositoryItem#hasChildren()
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
   * Factory method
   */
  public FileItem createItem( final File file )
  {
    return new FileItem( this, file );
  }
}