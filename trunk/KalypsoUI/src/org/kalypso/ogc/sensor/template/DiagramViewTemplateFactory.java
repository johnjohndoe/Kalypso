package org.kalypso.ogc.sensor.template;

import java.net.MalformedURLException;

import org.eclipse.core.resources.IFile;
import org.kalypso.ogc.sensor.diagview.DefaultDiagramTemplate;
import org.kalypso.ogc.sensor.diagview.IDiagramTemplate;
import org.kalypso.ogc.sensor.zml.ZmlObservation;

/**
 * @author schlienger
 *
 */
public class DiagramViewTemplateFactory
{
  private DiagramViewTemplateFactory()
  {
    // no to be instanciated
  }

  /**
   * 
   */
  public IDiagramTemplate createTemplate( final IFile file ) throws MalformedURLException
  {
    if( file.getFileExtension().equalsIgnoreCase( "zml" ) )
      return new DefaultDiagramTemplate( new ZmlObservation( file.getLocation().toFile() ) );
    else if( file.getFileExtension().equalsIgnoreCase( "odt" ) )
      return null; // TODO
    
    throw new IllegalArgumentException( "File extension type not supported: " + file.getFileExtension() + " for file: " + file.getName() );
  }
}
