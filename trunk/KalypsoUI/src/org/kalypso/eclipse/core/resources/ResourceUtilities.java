package org.kalypso.eclipse.core.resources;

import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.core.resources.IResource;

/**
 * @author belger
 */
public class ResourceUtilities
{
  private ResourceUtilities()
  {
    // do not instantiate
  }

  public static URL createURL( final IResource resource ) throws MalformedURLException
  {
    return new URL( "platform:/resource/" + resource.getFullPath().toString() );
  }
}
