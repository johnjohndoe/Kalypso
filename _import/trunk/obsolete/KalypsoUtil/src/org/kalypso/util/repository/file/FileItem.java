package org.kalypso.util.repository.file;

import java.io.File;

import org.kalypso.util.repository.IRepositoryItem;

/**
 * @author schlienger
 */
public class FileItem implements IRepositoryItem
{
  private final FileRepository m_rep;
  private final File m_file;

  public FileItem( final FileRepository rep, final File file )
  {
    m_rep = rep;
    m_file = file;
  }

  /**
   * @see org.kalypso.util.repository.IRepositoryItem#getName()
   */
  public String getName()
  {
    return m_file.getName();
  }

  /**
   * @see org.kalypso.util.repository.IRepositoryItem#getParent()
   */
  public IRepositoryItem getParent()
  {
    return m_rep.createItem( m_file.getParentFile() );
  }

  /**
   * @see org.kalypso.util.repository.IRepositoryItem#getChildren()
   */
  public IRepositoryItem[] getChildren()
  {
    File[] files = m_file.listFiles( m_rep.getFilter() );
    IRepositoryItem[] items = new IRepositoryItem[ files.length ];
    
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
   * @see org.kalypso.util.repository.IRepositoryItem#hasChildren()
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
    return null;
  }
}
