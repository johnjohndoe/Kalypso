package org.kalypso.eclipse.core.resources;

import org.eclipse.core.resources.IFolder;

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
