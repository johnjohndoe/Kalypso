package org.kalypso.ui.editorLauncher;

import java.io.StringWriter;

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
import org.kalypso.template.obsdiagview.ObjectFactory;
import org.kalypso.template.obsdiagview.ObsdiagviewType;
import org.kalypso.template.obsdiagview.TypeAxis;
import org.kalypso.template.obsdiagview.ObsdiagviewType.LegendType;

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
    try
    {
      final IPath projectRelativePath = file.getProjectRelativePath();

      final ObjectFactory of = new ObjectFactory();

      final ObsdiagviewType template = of.createObsdiagviewType();
      template.setTitle( file.getName() );
      
      final LegendType legend = of.createObsdiagviewTypeLegendType();
      legend.setTitle( "Legende" );
      legend.setVisible( true );
      template.setLegend( legend );
      
      // TODO ...
//      TypeAxis axis = of.createTypeAxis();
//      axis.s
//      template.getAxis();
      
      template.getObservation();
      
//      layer.setHref( "project:/" + projectRelativePath );

      
      final Marshaller marshaller = of.createMarshaller();
      marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );

      final StringWriter w = new StringWriter();
      marshaller.marshal( template, w );
      w.close();

      final String string = w.toString();

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
  }
}
