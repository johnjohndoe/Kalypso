package org.kalypso.eclipse.core.resources;

import java.io.InputStream;

import org.eclipse.core.resources.IStorage;
import org.eclipse.core.runtime.IPath;

import com.sun.xml.bind.StringInputStream;

/**
 * @author belger
 */
public class StringStorage implements IStorage
{
  private final String m_data;
  private final IPath m_path;

  public StringStorage( final String data, final IPath path )
  {
    m_data = data;
    m_path = path;
  }

  /**
   * @see org.eclipse.core.resources.IStorage#getContents()
   */
  public InputStream getContents()
  {
    return new StringInputStream( m_data );
  }

  /**
   * @see org.eclipse.core.resources.IStorage#getFullPath()
   */
  public IPath getFullPath()
  {
    return m_path;
  }

  /**
   * @see org.eclipse.core.resources.IStorage#getName()
   */
  public String getName()
  {
    return null;
  }

  /**
   * @see org.eclipse.core.resources.IStorage#isReadOnly()
   */
  public boolean isReadOnly()
  {
    return true;
  }

  /**
   * @see org.eclipse.core.runtime.IAdaptable#getAdapter(java.lang.Class)
   */
  public Object getAdapter( Class adapter )
  {
    return null;
  }

}