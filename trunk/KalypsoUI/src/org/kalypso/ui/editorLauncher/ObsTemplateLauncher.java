package org.kalypso.ui.editorLauncher;

import java.io.FileFilter;

import org.apache.commons.io.filefilter.FileFilterUtils;
import org.apache.commons.io.filefilter.IOFileFilter;
import org.eclipse.core.runtime.IPath;
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
    final IOFileFilter ottFilter = FileFilterUtils.suffixFileFilter( ".ott" );
    final IOFileFilter odtFilter = FileFilterUtils.suffixFileFilter( ".odt" );
    final FileFilter filter = FileFilterUtils.orFileFilter( odtFilter, ottFilter );

    final IDefaultTemplateLauncher dl = new DefaultObsDiagTemplateLauncher();

    ViewEditorLauncherHelper.showTemplateDialog( filePath, filter, new IDefaultTemplateLauncher[] { dl } );
  }
}
