package org.kalypso.util.transformation;

import java.util.Properties;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;

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
}
