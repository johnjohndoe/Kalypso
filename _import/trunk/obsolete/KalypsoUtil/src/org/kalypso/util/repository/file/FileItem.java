package org.kalypso.util.repository.file;

import java.io.File;
import java.io.FileFilter;

import org.kalypso.util.repository.IRepositoryItem;

/**
 * @author schlienger
 *
 */
public class FileItem implements IRepositoryItem
{
  private final File m_file;
  private final FileFilter m_filter;

  public FileItem( final String fileName, final FileFilter filter )
  {
    this( new File( fileName ), filter);
  }
  
  public FileItem( final File file, final FileFilter filter )
  {
    m_file = file;
    m_filter = filter;
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
    return new FileItem( m_file.getParent(), m_filter );
  }

  /**
   * @see org.kalypso.util.repository.IRepositoryItem#getChildren()
   */
  public IRepositoryItem[] getChildren()
  {
    File[] files = m_file.listFiles( m_filter );
    FileItem[] items = new FileItem[ files.length ];
    
    for( int i = 0; i < items.length; i++ )
      items[i] = new FileItem( files[i], m_filter );
    
    return items;
  }

  /**
   * @see org.kalypso.util.repository.IRepositoryItem#hasChildren()
   */
  public boolean hasChildren()
  {
    return m_file.isDirectory();
  }
}
