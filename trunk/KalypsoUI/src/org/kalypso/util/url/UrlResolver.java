package org.kalypso.util.url;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.Map.Entry;

import org.eclipse.core.internal.resources.PlatformURLResourceConnection;
import org.eclipse.core.resources.IProject;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.java.net.test.UrlUtilities;

/**
 * <p>Erzeugt aus einem String eine URL</p>
 * <p>Davor kann noch eine Token-Ersetzung stattfinden</p>
 * 
 * @author belger
 */
public class UrlResolver
{
  final Properties m_replaceTokens = new Properties();
  
  public UrlResolver( final Properties replaceTokens )
  {
    m_replaceTokens.putAll( replaceTokens );
  }
  
  public URL resolveUrl( final String templateUrl ) throws MalformedURLException
  {
    for( final Iterator iter = m_replaceTokens.entrySet().iterator(); iter.hasNext(); )
    {
      final Map.Entry entry = (Entry)iter.next();
      final String token = (String)entry.getKey();
      final String replace = (String)entry.getValue();
      templateUrl.replaceAll( token, replace );
    }
    
    return new URL( templateUrl );
  }

  /**
   * <p>Löst eine URL relativ zu einer anderen auf.</p>
   * <p>Also handles the pseudo protocol 'project:'. If project: ist specified in relativeURL,
   * it tries to guess the project from the baseURL (e.g. the baseURL must be of the form platfrom:/resource/).
   * It then replaces project: by 'platform:/resource/<projectname>/
   * </p>
   * @throws MalformedURLException
   */
  public static URL resolveURL( final URL baseURL, final String relativeURL ) throws MalformedURLException
  {
    if( relativeURL.startsWith( "project:" ) )
    {
      if( !baseURL.toString().startsWith( PlatformURLResourceConnection.RESOURCE_URL_STRING ) )
        throw new MalformedURLException( "Protocol 'project:' need a resource url as context" );
      
      final IProject project = ResourceUtilities.findProjectFromURL( baseURL );
      final String projectURL = PlatformURLResourceConnection.RESOURCE_URL_STRING + "/" + project.getName();
      
      final String relPath = relativeURL.substring( "project:".length() + 1 );
      return new URL( projectURL + "/" + relPath );      
    }
    
    return UrlUtilities.resolveURL(  baseURL, relativeURL );
  }

}
