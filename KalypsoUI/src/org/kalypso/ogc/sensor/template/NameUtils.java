package org.kalypso.ogc.sensor.template;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;

/**
 * @author belger
 */
public class NameUtils
{
  public static final String TOKEN_AXISUNIT = "%axisunit%";

  public static final String TOKEN_AXISTYPE = "%axistype%";

  public static final String TOKEN_AXISNAME = "%axisname%";

  public static final String TOKEN_OBSNAME = "%obsname%";

  public static final String DEFAULT_ITEM_NAME = "%axistype% - %obsname%";
  
  private NameUtils()
  {
    // utility class
  }

  /**
   * Replace tokens in Format-String
   * 
   * <dl>
   * <dt>%obsname%</dt>
   * <dd>Name der Observation: obs.getName()</dd>
   * <dt>%axisname%</dt>
   * <dd>Name der Wert-Achse: axis.getName()</dd>
   * <dt>%axistype%</dt>
   * <dd>Typ der Wert-Achse: axis.getType()</dd>
   * <dt>%axisunit%</dt>
   * <dd>Einheit der Wert-Achse: axis.getUnit()</dd>
   * </dl>
   *  
   */
  public static String replaceTokens( final String formatString,
      final IObservation obs, final IAxis axis )
  {
    String result = formatString;
    result = result.replaceAll( TOKEN_OBSNAME, obs.getName() );
    result = result.replaceAll( TOKEN_AXISNAME, axis.getName() );
    result = result.replaceAll( TOKEN_AXISTYPE, axis.getType() );
    result = result.replaceAll( TOKEN_AXISUNIT, axis.getUnit() );

    // Metadaten
    int index = 0;
    while( index < result.length() - 1 )
    {
      final int start = result.indexOf( "%metadata-", index );
      if( start == -1 )
        break;

      final int stop = result.indexOf( '%', start + 1 );
      if( stop != -1 )
      {
        final String metaname = result.substring(
            start + "%metadata-".length(), stop );
        final StringBuffer sb = new StringBuffer( result );

        final String metaval = obs.getMetadataList().getProperty( metaname,
            "<Metavalue '" + metaname + "' not found>" );
        sb.replace( start, stop + 1, metaval );

        result = sb.toString();
      }

      index = stop + 1;
    }

    return result;
  }

//  /**
//   * Remove the tokens so that name is clean.
//   * 
//   * @param name
//   */
//  public static String removeTokens( final String name )
//  {
//    String res = name.replaceAll( TOKEN_AXISNAME, "" );
//    res = res.replaceAll( TOKEN_AXISTYPE, "" );
//    res = res.replaceAll( TOKEN_AXISUNIT, "" );
//    res = res.replaceAll( TOKEN_OBSNAME, "" );
//
//    return res;
//  }
//
}
