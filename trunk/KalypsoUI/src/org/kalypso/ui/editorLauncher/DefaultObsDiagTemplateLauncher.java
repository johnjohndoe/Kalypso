package org.kalypso.ui.editorLauncher;

import java.io.StringWriter;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IPath;
import org.eclipse.ui.IEditorDescriptor;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorRegistry;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.eclipse.core.resources.StringStorage;
import org.kalypso.eclipse.ui.editorinput.StorageEditorInput;
import org.kalypso.ogc.sensor.diagview.ObservationTemplateHelper;
import org.kalypso.ogc.sensor.diagview.impl.LinkedDiagramTemplate;
import org.kalypso.template.obsdiagview.ObsdiagviewType;

/**
 * DefaultObservationTemplateLauncher
 * 
 * @author schlienger
 */
public class DefaultObsDiagTemplateLauncher implements
    IDefaultTemplateLauncher
{
  /**
   * @see org.kalypso.ui.editorLauncher.IDefaultTemplateLauncher#getFilename()
   */
  public String getFilename( )
  {
    return "<Standard Diagram Editor>.odt";
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
  public IEditorInput createInput( IFile file )
  {
    StringWriter writer = null;
    try
    {
      final IPath projectRelativePath = file.getProjectRelativePath();

      final LinkedDiagramTemplate template = new LinkedDiagramTemplate();
      template.addObservation( file.getName(), ResourceUtilities.createURL( file), "project:/" + projectRelativePath, "zml", null );

      final ObsdiagviewType tType = ObservationTemplateHelper.buildDiagramTemplateXML( template );
      
      writer = new StringWriter();
      ObservationTemplateHelper.saveDiagramTemplateXML( tType, writer );

      final String string = writer.toString();

      // als StorageInput zurückgeben
      final StorageEditorInput input = new StorageEditorInput(
          new StringStorage( string, file.getFullPath() ) );

      return input;
    }
    catch( Exception e )
    {
      e.printStackTrace();
      return null;
    }
    finally
    {
      IOUtils.closeQuietly( writer );
    }
  }
}
