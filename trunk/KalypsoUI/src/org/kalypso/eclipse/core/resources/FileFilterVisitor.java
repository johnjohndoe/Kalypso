package org.kalypso.eclipse.core.resources;

import java.io.File;
import java.io.FileFilter;
import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.runtime.IPath;

/**
 * die Visitor sammelt alle IFile-Objekte, deren Location auf einen festgelegten FileFilter passen.
 * 
 * @author belger
 */
public class FileFilterVisitor implements IResourceVisitor
{
  private final FileFilter m_filter;
  
  private final Collection m_foundFiles = new ArrayList();

  public FileFilterVisitor( final FileFilter filter )
  {
    m_filter = filter;
  }
  
  public IFile[] getFiles()
  {
    return (IFile[])m_foundFiles.toArray( new IFile[m_foundFiles.size()] );
  }

  /**
   * @see org.eclipse.core.resources.IResourceVisitor#visit(org.eclipse.core.resources.IResource)
   */
  public boolean visit( final IResource resource )
  {
    if( resource instanceof IFile )
    {
      final IFile file = (IFile)resource;
      final IPath location = file.getLocation();
      final File fileFile = location.toFile();
      if( m_filter.accept( fileFile ) )
        m_foundFiles.add( file );
    }
    
    return true;
  }

}
