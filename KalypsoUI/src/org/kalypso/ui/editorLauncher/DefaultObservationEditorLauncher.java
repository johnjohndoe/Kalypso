package org.kalypso.ui.editorLauncher;

import java.net.MalformedURLException;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IPath;
import org.eclipse.ui.IEditorDescriptor;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorRegistry;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.ogc.sensor.template.PseudoTemplateEditorInput;
import org.kalypso.ogc.sensor.template.TemplateStorage;

/**
 * DefaultObservationTemplateLauncher
 * 
 * @author schlienger
 */
public class DefaultObservationEditorLauncher implements IDefaultTemplateLauncher
{
  private final String m_pseudoFilename;
  private final String m_fileExtension;

  public DefaultObservationEditorLauncher( final String pseudoFilename, final String fileExtension )
  {
    m_pseudoFilename = pseudoFilename;
    m_fileExtension = fileExtension;
  }
  
  /**
   * @see org.kalypso.ui.editorLauncher.IDefaultTemplateLauncher#getFilename()
   */
  public String getFilename( )
  {
    return m_pseudoFilename;
  }

  /**
   * @see org.kalypso.ui.editorLauncher.IDefaultTemplateLauncher#getEditor()
   */
  public IEditorDescriptor getEditor( )
  {
    final IWorkbench workbench = PlatformUI.getWorkbench();
    final IEditorRegistry editorRegistry = workbench.getEditorRegistry();

    return editorRegistry.getDefaultEditor( getFilename() );
  }

  /**
   * @see org.kalypso.ui.editorLauncher.IDefaultTemplateLauncher#createInput(org.eclipse.core.resources.IFile)
   */
  public IEditorInput createInput( final IFile file )
  {
    try
    {
      final IPath projectRelativePath = file.getProjectRelativePath();
      
      final PseudoTemplateEditorInput input = new PseudoTemplateEditorInput(
          new TemplateStorage( file, ResourceUtilities
              .createURL( file ), "project:/" + projectRelativePath ), m_fileExtension );

      return input;
    }
    catch( MalformedURLException e )
    {
      e.printStackTrace();
      return null;
    }
  }
}