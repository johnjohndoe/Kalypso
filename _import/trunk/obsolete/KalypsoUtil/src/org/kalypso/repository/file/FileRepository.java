package org.kalypso.repository.file;

import java.io.File;
import java.io.FileFilter;
import java.net.MalformedURLException;

import org.kalypso.java.io.FileUtilities;
import org.kalypso.java.io.filter.AcceptAllFileFilter;
import org.kalypso.repository.AbstractRepository;
import org.kalypso.repository.IRepositoryFactory;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;

/**
 * Ein File Repository.
 * 
 * @author schlienger
 */
public class FileRepository extends AbstractRepository
{
  protected final File m_root;

  protected final FileFilter m_filter;

  public FileRepository( final IRepositoryFactory factory, final String location, final boolean readOnly, final FileFilter filter )
  {
    super( factory, location, readOnly );

    if( filter == null )
      m_filter = new AcceptAllFileFilter();
    else
      m_filter = filter;

    m_root = new File( location );
    if( !m_root.exists() )
      throw new IllegalArgumentException( "Location existiert nicht! (Location: " + location + ")" );
  }

  public FileRepository( final IRepositoryFactory factory, final String location, final boolean readOnly )
  {
    this( factory, location, readOnly, null );
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
   * Factory method
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
    try
    {
      return m_root.toURL().toExternalForm();
    }
    catch( MalformedURLException e )
    {
      throw new IllegalStateException( e.getLocalizedMessage() );
    }
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
  public IRepositoryItem findItem( final String id ) throws RepositoryException
  {
    // both lowercase to be sure comparison is done homogeneously
    final String rootURL = getIdentifier().toLowerCase();
    final String mid = id.toLowerCase();
    
    if( !mid.startsWith( rootURL ) )
      throw new RepositoryException( "File <" + mid + "> seems not to be part of the repository: "
          + m_root );

    final String path = m_root.getAbsolutePath() + File.separator + mid.replaceFirst( rootURL, "" );

    final File f = new File( path );

    if( !f.exists() )
      throw new RepositoryException( "File <" + mid + "> does not exist!" );

    if( !m_filter.accept( f ) )
      throw new RepositoryException( "File <" + mid + "> does not fit filter!" );

    if( !FileUtilities.isChildOf( m_root, f ) )
      throw new RepositoryException( "File <" + mid
          + "> is not part of this File Repository starting at:" + m_root );

    return createItem( f );
  }
}