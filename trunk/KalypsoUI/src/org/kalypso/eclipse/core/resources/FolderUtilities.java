package org.kalypso.eclipse.core.resources;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * @author belger
 */
public class FolderUtilities
{
  /** Do not instantiate */
  private FolderUtilities()
  {
     // 
  }
  
  public static void mkdirs( final IContainer folder ) throws CoreException
  {
    if( folder == null || folder.exists() )
      return;
    
    if( !( folder instanceof IFolder ) )
      throw new CoreException( KalypsoGisPlugin.createErrorStatus( "Cannot mkdirs project or workspace", null ) );
    
    // create parents
    mkdirs( folder.getParent() );
    
    ((IFolder)folder).create( false, true, new NullProgressMonitor() );
  }
  
  public static IFolder createUnusedFolder( final IFolder parentFolder, final String prefix )
  {
    int i = 0;
    while( true )
    {
      final IFolder f = parentFolder.getFolder( prefix + i );
      if( !f.exists() )
        return f;
      
      i++;
    }
  }

}
