package org.kalypso.repository.file;

import java.io.File;
import java.io.FileFilter;

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
  public FileRepository( final IRepositoryFactory factory,
      final String location, final String identifier, final boolean readOnly,
      final FileFilter filter )
  {
    super( factory, location, readOnly );

    m_identifier = identifier;

    if( filter == null )
      m_filter = new AcceptAllFileFilter();
    else
      m_filter = filter;

    m_root = new File( location );
    if( !m_root.exists() )
      throw new IllegalArgumentException(
          "Location existiert nicht! (Location: " + location + ")" );
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
  public FileRepository( final IRepositoryFactory factory,
      final String location, final String identifier, final boolean readOnly )
  {
    this( factory, location, identifier, readOnly, null );
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getChildren()
   */
  public IRepositoryItem[] getChildren( )
  {
    return createItem( m_root ).getChildren();
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#hasChildren()
   */
  public boolean hasChildren( )
  {
    return m_root.isDirectory();
  }

  public FileFilter getFilter( )
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
  public String getIdentifier( )
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
  public void reload( )
  {
    // nothing to do
  }

  /**
   * @see org.kalypso.repository.IRepository#findItem(java.lang.String)
   */
  public IRepositoryItem findItem( final String id ) throws RepositoryException
  {
    //    // both lowercase to be sure comparison is done homogeneously
    //    final String rootURL = getIdentifier().toLowerCase();
    //    final String mid = id.toLowerCase();
    //    
    //    if( !mid.startsWith( rootURL ) )
    //      throw new RepositoryException( "File <" + mid + "> seems not to be part
    // of the repository: "
    //          + m_root );
    //
    //    final String path = m_root.getAbsolutePath() + File.separator +
    // mid.replaceFirst( rootURL, "" );
    //
    //    final File f = new File( path );
    //
    //    if( !f.exists() )
    //      throw new RepositoryException( "File <" + mid + "> does not exist!" );
    //
    //    if( !m_filter.accept( f ) )
    //      throw new RepositoryException( "File <" + mid + "> does not fit filter!"
    // );
    //
    //    if( !FileUtilities.isChildOf( m_root, f ) )
    //      throw new RepositoryException( "File <" + mid
    //          + "> is not part of this File Repository starting at:" + m_root );
    //
    //    return createItem( f );

    // both lowercase to be sure comparison is done homogeneously
    final String baseId = getIdentifier().toLowerCase();
    final String itemId = id.toLowerCase();

    final String scheme = baseId + ":/"; 
    
    if( !itemId.startsWith( scheme ) )
      throw new RepositoryException( "File <" + id
          + "> seems not to be part of the repository: " + m_root );

    // absolute path of the root (replace backslashes on windows with forward slashes)
    final String strRoot = m_root.getAbsolutePath().replace( '\\', '/' );
    final String path = itemId.replaceFirst( scheme, strRoot );

    final File f = new File( path );

    if( !f.exists() )
      throw new RepositoryException( "File <" + id + "> does not exist!" );

    if( !m_filter.accept( f ) )
      throw new RepositoryException( "File <" + id + "> does not fit filter!" );

    if( !FileUtilities.isChildOf( m_root, f ) )
      throw new RepositoryException( "File <" + id
          + "> is not part of this File Repository starting at:" + m_root );

    return createItem( f );
  }
}