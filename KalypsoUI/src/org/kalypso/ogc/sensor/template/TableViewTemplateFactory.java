package org.kalypso.ogc.sensor.template;

import java.net.MalformedURLException;

import org.eclipse.core.resources.IFile;
import org.kalypso.ogc.sensor.tableview.DefaultTableViewTemplate;
import org.kalypso.ogc.sensor.tableview.ITableViewTemplate;
import org.kalypso.ogc.sensor.zml.ZmlObservation;

/**
 * A Factory class that creates <code>ITableViewTemplate</code> objects based on 
 * the file extension.
 * 
 * @author schlienger
 */
public class TableViewTemplateFactory
{
  private TableViewTemplateFactory()
  {
    // not intended to be instanciated
  }
  
  public static ITableViewTemplate createTemplate( IFile file ) throws MalformedURLException
  {
    if( file.getFileExtension().equalsIgnoreCase( "zml" ) )
      return new DefaultTableViewTemplate( new ZmlObservation( file.getLocation().toFile() ) );
    else if( file.getFileExtension().equalsIgnoreCase( "ott" ) )
      return new TableViewTemplate( file );
    
    throw new IllegalArgumentException( "File extension type not supported: " + file.getFileExtension() + " for file: " + file.getName() );
  }
}
