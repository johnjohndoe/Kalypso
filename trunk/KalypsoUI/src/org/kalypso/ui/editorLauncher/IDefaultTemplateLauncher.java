package org.kalypso.ui.editorLauncher;

import org.eclipse.core.resources.IFile;
import org.eclipse.ui.IEditorDescriptor;
import org.eclipse.ui.IEditorInput;

/**
 * @author belger
 */
public interface IDefaultTemplateLauncher
{
  /**
   * Dieser 'Dateiname' dient zur Anzeige im Auswahldialog. Der Prefix (vor dem
   * Punkt) bestimmt den Anzeigenamen, die Extension das Icon = Icon des
   * DefaultEditors für diese Endung.
   */
  public String getFilename();

  public IEditorDescriptor getEditor();

  public IEditorInput createInput( final IFile file );
}