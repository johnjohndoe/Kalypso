package org.kalypso.eclipse.core.resources;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;

import org.eclipse.core.resources.IEncodedStorage;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * @author belger
 */
public class StringStorage implements IEncodedStorage
{
  private final String m_data;
  private final IPath m_path;

  public StringStorage( final String data, final IPath path )
  {
    m_data = data;
    m_path = path;
  }

  /**
   * @throws CoreException
   * @see org.eclipse.core.resources.IStorage#getContents()
   */
  public InputStream getContents() throws CoreException
  {
    try
    {
      final byte[] bytes = m_data.getBytes( "UTF-8" );
      return new ByteArrayInputStream( bytes );
    }
    catch( final UnsupportedEncodingException e )
    {
      e.printStackTrace();
      
      throw new CoreException( KalypsoGisPlugin.createErrorStatus( "", e ) );
    }
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

  /**
   * @see org.eclipse.core.resources.IEncodedStorage#getCharset()
   */
  public String getCharset()
  {
    // allways Unicode, because wie 
    return "UTF-8";
  }

}