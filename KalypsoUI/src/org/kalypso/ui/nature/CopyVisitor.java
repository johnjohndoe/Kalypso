package org.kalypso.ui.nature;

import java.io.File;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.runtime.CoreException;

/**
 * @author belger
 */
public class CopyVisitor implements IResourceVisitor
{
  public CopyVisitor( final File targetDir )
  {
    
  }

  /**
   * @see org.eclipse.core.resources.IResourceVisitor#visit(org.eclipse.core.resources.IResource)
   */
  public boolean visit( IResource resource ) throws CoreException
  {
    return false;
  }

}
