package org.kalypso.ui.calcwizard.bericht;

import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.Map.Entry;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypso.ui.calcwizard.Arguments;
import org.kalypso.zml.obslink.TimeseriesLinkType;

/**
 * @author belger
 */
public class ExporterHelper
{
  public static final String MSG_TOKEN_NOT_FOUND = "Token not found";

  private ExporterHelper()
  {
    //
  }
  
  public final static Properties createReplaceTokens( final Feature feature, final Arguments tokens )
  {
    final Properties replacetokens = new Properties();
    for( final Iterator tokIt = tokens.entrySet().iterator(); tokIt.hasNext(); )
    {
      final Map.Entry entry = (Entry) tokIt.next();
      final String tokenname = (String) entry.getKey();
      final String featureProperty = (String) entry.getValue();
      
      final Object property = feature.getProperty( featureProperty );
      String replace = null;
      if( property instanceof TimeseriesLinkType )
        replace = ((TimeseriesLinkType) property).getHref();
      else if( property != null )
        replace = property.toString();
      else if( property == null )
        replace = "!" + MSG_TOKEN_NOT_FOUND + ": " + tokenname + "!";
      
      if( replace != null )
        replacetokens.setProperty( tokenname, replace );
    }
    return replacetokens;
  }


}
