package org.kalypso.ui.editorLauncher;

import java.io.FileFilter;

import org.apache.commons.io.filefilter.FileFilterUtils;
import org.apache.commons.io.filefilter.IOFileFilter;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.ui.IEditorLauncher;

/**
 * @author belger
 */
public class GisTemplateLauncher implements IEditorLauncher
{
  /**
   * @see org.eclipse.ui.IEditorLauncher#open(org.eclipse.core.runtime.IPath)
   */
  public void open( final IPath filePath )
  {
    // was haben wir den da?
    final IWorkspace workspace = ResourcesPlugin.getWorkspace();
    final IWorkspaceRoot root = workspace.getRoot();
    final IFile file = root.getFileForLocation( filePath );
    if( file == null )
      return;
    
    // vorhandene Vorlagen im Verzeichnis finden
    final IContainer folder = file.getParent();
    
    final IOFileFilter gttFilter = FileFilterUtils.suffixFileFilter( ".gmt" );
    final IOFileFilter gmtFilter = FileFilterUtils.suffixFileFilter( ".gtt" );
    final FileFilter filter = FileFilterUtils.orFileFilter( gmtFilter, gttFilter );
    
    // virtuelle Vorlagen finden
    final IFile gmtDefault = folder.getFile( new Path( "<Standard Kartenansicht>" ) );
    final IFile gttDefault = folder.getFile( new Path( "<Standard Datenansicht>" ) );

    ViewEditorLauncherHelper.showTemplateDialog( folder, filter, new IFile[] { gmtDefault, gttDefault } );
  }
}
