package org.kalypso.ogc.sensor.template;

import java.io.InputStream;
import java.net.URL;

import org.eclipse.core.resources.IEncodedStorage;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;

/**
 * TemplateStorage
 * 
 * @author schlienger
 */
public class TemplateStorage implements IEncodedStorage
{
  private final URL m_context;
  private final IFile m_zmlFile;
  private final String m_href;

  public TemplateStorage( final IFile zmlFile, final URL context, final String href )
  {
    m_zmlFile = zmlFile;
    m_context = context;
    m_href = href;
  }

  /**
   * @return Returns the context.
   */
  public URL getContext( )
  {
    return m_context;
  }
  
  /**
   * @return Returns the zmlFile.
   */
  public IFile getZmlFile( )
  {
    return m_zmlFile;
  }

  /**
   * @return Returns the href.
   */
  public String getHref( )
  {
    return m_href;
  }
  
  /**
   * @see org.eclipse.core.resources.IStorage#getContents()
   */
  public InputStream getContents( ) throws CoreException
  {
    return m_zmlFile.getContents();
  }

  /**
   * @see org.eclipse.core.resources.IStorage#getFullPath()
   */
  public IPath getFullPath( )
  {
    return m_zmlFile.getFullPath();
  }

  /**
   * @see org.eclipse.core.resources.IStorage#getName()
   */
  public String getName( )
  {
    return m_zmlFile.getName();
  }

  /**
   * @see org.eclipse.core.resources.IStorage#isReadOnly()
   */
  public boolean isReadOnly( )
  {
    return false;
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
  public String getCharset( ) throws CoreException
  {
    return "UTF-8";
  }
}