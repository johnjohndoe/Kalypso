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
public class ObsTemplateLauncher implements IEditorLauncher
{

  /**
   * @see org.eclipse.ui.IEditorLauncher#open(org.eclipse.core.runtime.IPath)
   */
  public void open( final IPath filePath )
  {
    final IWorkspace workspace = ResourcesPlugin.getWorkspace();
    final IWorkspaceRoot root = workspace.getRoot();
    final IFile file = root.getFileForLocation( filePath );
    if( file == null )
      return;

    // vorhandene Vorlagen im Verzeichnis finden
    final IContainer folder = file.getParent();
    
    final IOFileFilter ottFilter = FileFilterUtils.suffixFileFilter( ".ott" );
    final IOFileFilter odtFilter = FileFilterUtils.suffixFileFilter( ".odt" );
    final FileFilter filter = FileFilterUtils.orFileFilter( odtFilter, ottFilter );

    // virtuelle Vorlagen finden
    final IFile odtDefault = folder.getFile( new Path( "<Standard Diagrammansicht>" ) );
    final IFile ottDefault = folder.getFile( new Path( "<Standard Tabellenansicht>" ) );

    ViewEditorLauncherHelper.showTemplateDialog( folder, filter, new IFile[] { odtDefault, ottDefault } );
  }

}