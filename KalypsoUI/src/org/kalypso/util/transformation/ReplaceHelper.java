package org.kalypso.util.transformation;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Properties;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;

/**
 * @author belger
 */
public class ReplaceHelper
{
  private ReplaceHelper()
  {
    // do not instantiate
  }

  public static Properties configureReplaceProps( final IProject project, final IFolder calcdir )
  {
    final Properties properties = new Properties();
    
    properties.put( ":project:",  "platform:/resource/" + project.getName() + "/" );
    properties.put( ":calcdir:",  "platform:/resource/" + calcdir.getFullPath().toString() + "/" );
    
    return properties;
  }

  public static URL createURL( final IResource resource ) throws MalformedURLException
  {
    return new URL( "platform:/resource/" + resource.getFullPath().toString() );
  }
}
