package org.deegree_impl.model.feature;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.deegree.model.feature.Feature;

/**
 * @author doemming
 *  
 */
public class FeatureHelper
{
  public static boolean booleanIsTrue( Feature feature, String propName, boolean defaultStatus )
  {
    Object property = feature.getProperty( propName );
    if( property != null && property instanceof Boolean )
      return ( (Boolean)property ).booleanValue();
    return defaultStatus;
  }

  /**
   * @param fe
   * @param string
   * @param string2
   * @return
   */
  public static String getFormatedDate( Feature feature, String propName,
      String simpleDateFormatPattern, String defaultValue )
  {
    Object property = feature.getProperty( propName );
    if( property != null && property instanceof Date )
    {
      DateFormat dateFormat = new SimpleDateFormat( simpleDateFormatPattern );
      return dateFormat.format( (Date)property );
    }
    return defaultValue;

  }

  public static double getAsDouble( Feature feature, String propName, double defaultValue )
  {
    Object value = feature.getProperty( propName );
    if( value == null )
      return defaultValue;
    if( value instanceof String )
      return Double.valueOf( (String)value ).doubleValue();
    // should be a Double
    return ( (Double)value ).doubleValue();
  }

  public static String getAsString( Feature nodeFE, String string )
  {
    // TODO use numberformat
    Object value = nodeFE.getProperty(string);
    if(value==null)
      return null;
    if(value instanceof String)
      return (String)value;
    return value.toString();
  }
}