package org.kalypso.ui.editorLauncher;

import java.io.FileFilter;

import org.apache.commons.io.filefilter.FileFilterUtils;
import org.apache.commons.io.filefilter.IOFileFilter;
import org.eclipse.core.runtime.IPath;
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
    final IOFileFilter gttFilter = FileFilterUtils.suffixFileFilter( ".gmt" );
    final IOFileFilter gmtFilter = FileFilterUtils.suffixFileFilter( ".gtt" );
    final FileFilter filter = FileFilterUtils.orFileFilter( gmtFilter, gttFilter );
    
    // virtuelle Vorlagen finden
    final Object gmtDefault = "<Standard Kartenansicht>";
    final Object gttDefault = "<Standard Datenansicht>";

    ViewEditorLauncherHelper.showTemplateDialog( filePath, filter, new Object[] { gmtDefault, gttDefault } );
  }
}
