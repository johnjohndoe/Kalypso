package org.kalypso.ui.editorLauncher;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.ui.IEditorLauncher;

/**
 * @author belger
 */
public class GmlTemplateLauncher implements IEditorLauncher
{
  /**
   * @see org.eclipse.ui.IEditorLauncher#open(org.eclipse.core.runtime.IPath)
   */
  public void open( final IPath filePath )
  {
    // was haben wir den da?
    final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
    final IFile file = root.getFile( filePath );
    if( !file.exists() )
      // TODO: error handling?
      return;
    
    // vorhandene Vorlagen im Verzeichnis finden
    final IContainer folder = file.getParent();
    
    new IResourceVisitor() {

      public boolean visit( final IResource resource ) throws CoreException
      {
        return false;
      }};
    
    try
    {
      folder.accept( null, 1, false );
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
    }
    
    // virtuelle Vorlagen finden
    
    // einen Dialog mit den möglichen Vorlagen anzeigen
    
    // falls eine ausgewählt wurde, die Vorlage öffnen
  }

}
