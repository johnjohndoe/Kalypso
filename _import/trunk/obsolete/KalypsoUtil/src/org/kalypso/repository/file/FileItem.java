package org.kalypso.repository.file;

import java.io.File;
import java.net.MalformedURLException;

import org.kalypso.repository.IRepository;
import org.kalypso.repository.IRepositoryItem;

/**
 * An item of a <code>FileRepository</code> that represents a <code>File</code>.
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
  public String getName()
  {
    return m_file.getName();
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getParent()
   */
  public IRepositoryItem getParent()
  {
    return m_rep.createItem( m_file.getParentFile() );
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getChildren()
   */
  public IRepositoryItem[] getChildren()
  {
    final File[] files = m_file.listFiles( m_rep.getFilter() );
    
    if( files == null )
      return EMPTY_ITEMS;
    
    final IRepositoryItem[] items = new IRepositoryItem[ files.length ];
    
    for( int i = 0; i < items.length; i++ )
      items[i] = m_rep.createItem( files[i] );
    
    return items;
  }

  public File getFile()
  {
    return m_file;
  }
  
  public FileRepository getRep()
  {
    return m_rep;
  }
  
  /**
   * @see org.kalypso.repository.IRepositoryItem#hasChildren()
   */
  public boolean hasChildren()
  {
    return m_file.isDirectory();
  }
  
  public String toString()
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
  public IRepository getRepository()
  {
    return m_rep;
  }

  /**
   * Returns the URL of the file.
   * 
   * @see org.kalypso.repository.IRepositoryItem#getIdentifier()
   */
  public String getIdentifier()
  {
    try
    {
      return m_file.toURL().toExternalForm();
    }
    catch( MalformedURLException e )
    {
      throw new IllegalStateException( e.getLocalizedMessage() );
    }
  }
}
