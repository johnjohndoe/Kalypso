package org.kalypsodeegree_impl.model.feature;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.Map.Entry;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;

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

  public static String getAsString( Feature nodeFE, String property )
  {
    // TODO use numberformat
    Object value = nodeFE.getProperty( property );
    if( value == null )
      return null;
    if( value instanceof String )
      return (String)value;
    return value.toString();
  }

  /**
   * Überträgt die Daten eines Features in die Daten eines anderen.
   * <p>Die Properties werden dabei anhand der übergebenen {@link Properties} zugeordnet. 
   * 
   *  Es gilt:
   * <ul>
   *  <li>Es erfolgt ein Deep-Copy, inneliegende Features werden komplett kopiert.</li>
   *  <li><Bei Referenzen auf andere Features erfolgt nur ein shallow copy, das Referenzierte Feature bleibt gleich./li>
   *  <li>Die Typen der Zurodnung müssen passen, sonst gibts ne Exception.</li>
   * </ul>
   * 
   * @throws IllegalArgumentException Falls eine Zuordnung zwischen Properties unterschiedlkicher Typen erfolgt.
   * @throws NullPointerException falls eines der Argumente <codce>null</code> ist.
   * @throws UnsupportedOperationException Noch sind nicht alle Typen implementiert
   */
  public static void copyProperties( final Feature sourceFeature, final Feature targetFeature, final Properties propertyMap )
  {
    final FeatureType sourceType = sourceFeature.getFeatureType();
    final FeatureType targetType = targetFeature.getFeatureType();
    
    for( final Iterator pIt = propertyMap.entrySet().iterator(); pIt.hasNext(); )
    {
      final Map.Entry entry = (Entry)pIt.next();
      final String sourceProp = (String)entry.getKey();
      final String targetProp = (String)entry.getValue();

      final FeatureTypeProperty sourceFTP = sourceType.getProperty( sourceProp );
      final FeatureTypeProperty targetFTP = targetType.getProperty( targetProp );

      if( !sourceFTP.getType().equals( targetFTP.getType() ) )
        throw new IllegalArgumentException( "Types of mapped properties are different: '" + sourceProp + "' and '" + targetProp + "'" );

      final Object object = sourceFeature.getProperty( sourceProp );
      
      final Object newobject;
      if( object instanceof String )
        newobject = object;
      else if( object instanceof Number )
        newobject = object;
      else
        throw new UnsupportedOperationException( "Cannot copy propertie of type: " + sourceFTP.getType() );
      
      targetFeature.setProperty( FeatureFactory.createFeatureProperty( targetProp, newobject ) );
    }
  }
}