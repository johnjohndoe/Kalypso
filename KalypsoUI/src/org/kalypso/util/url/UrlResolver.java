package org.kalypso.util.url;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.Map.Entry;

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

}
