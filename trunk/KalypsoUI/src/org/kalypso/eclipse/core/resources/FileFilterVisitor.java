package org.kalypso.eclipse.core.resources;

import java.io.FileFilter;
import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.runtime.CoreException;

/**
 * die Visitor sammelt alle IFile-Objekte, deren Location auf einen festgelegten FileFilter passen.
 * 
 * @author belger
 */
public class FileFilterVisitor implements IResourceVisitor
{
  private final FileFilter m_filter;
  
  private final Collection m_foundFile = new ArrayList();

  public FileFilterVisitor( final FileFilter filter )
  {
    m_filter = filter;
  }
  
  public IFile[] getFiles()
  {
    return (IFile[])m_foundFile.toArray( new IFile[m_foundFile.size()] );
  }

  /**
   * @see org.eclipse.core.resources.IResourceVisitor#visit(org.eclipse.core.resources.IResource)
   */
  public boolean visit( IResource resource ) throws CoreException
  {
    return false;
  }

}
