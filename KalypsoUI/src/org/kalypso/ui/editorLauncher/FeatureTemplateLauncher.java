package org.kalypso.ui.editorLauncher;

import java.io.IOException;
import java.io.StringWriter;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IPath;
import org.eclipse.ui.IEditorDescriptor;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorRegistry;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.kalypso.eclipse.core.resources.StringStorage;
import org.kalypso.eclipse.ui.editorinput.StorageEditorInput;
import org.kalypso.template.featureview.Featuretemplate;
import org.kalypso.template.featureview.ObjectFactory;
import org.kalypso.template.featureview.FeaturetemplateType.LayerType;

/**
 * @author belger
 */
public class FeatureTemplateLauncher implements IDefaultTemplateLauncher
{
  /**
   * @see org.kalypso.ui.editorLauncher.IDefaultTemplateLauncher#getFilename()
   */
  public String getFilename()
  {
    return "<Standard Feature Editor>.gft";
  }

  /**
   * @see org.kalypso.ui.editorLauncher.IDefaultTemplateLauncher#getEditor()
   */
  public IEditorDescriptor getEditor()
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
      
      // ein default template erzeugen
      final ObjectFactory factory = new ObjectFactory();

      final LayerType layer = factory.createFeaturetemplateTypeLayerType();
      layer.setId( file.toString() );
      layer.setHref( "PATH=" + "project:/" + projectRelativePath );
      layer.setLinktype( "gml" );
      layer.setFeaturePath( "" ); // immer das root-feature

      final Featuretemplate featuretemplate = factory.createFeaturetemplate();
      featuretemplate.setLayer( layer );

      final Marshaller marshaller = factory.createMarshaller();

      final StringWriter w = new StringWriter();
      marshaller.marshal( featuretemplate, w );
      w.close();

      final String string = w.toString();

      // als StorageInput zurückgeben
      final StorageEditorInput input = new StorageEditorInput( new StringStorage( string, file.getFullPath() ) );

      return input;
    }
    catch( JAXBException e )
    {
      e.printStackTrace();
      
      return null;
    }
    catch( IOException e )
    {
      e.printStackTrace();
      
      return null;
    }
  }

}