package org.kalypsodeegree_impl.model.feature;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Map.Entry;

import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

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

  public static String getFormatedDate( Feature feature, String propName, String simpleDateFormatPattern, String defaultValue )
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
   * �bertr�gt die Daten eines Features in die Daten eines anderen.
   * <p>Die Properties werden dabei anhand der �bergebenen {@link Properties} zugeordnet. 
   * 
   *  Es gilt:
   * <ul>
   *  <li>Es erfolgt ein Deep-Copy, inneliegende Features werden komplett kopiert.</li>
   *  <li><Bei Referenzen auf andere Features erfolgt nur ein shallow copy, das Referenzierte Feature bleibt gleich./li>
   *  <li>Die Typen der Zurodnung m�ssen passen, sonst gibts ne Exception.</li>
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

      if( sourceFTP == null )
        throw new IllegalArgumentException( "Quell-Property existiert nicht: " + sourceProp );
      if( targetFTP == null )
        throw new IllegalArgumentException( "Ziel-Property existiert nicht: " + targetProp );
      if( !sourceFTP.getType().equals( targetFTP.getType() ) )
        throw new IllegalArgumentException( "Typen der zugeordneten Properties sind unterschiedlich: '" + sourceProp + "' and '" + targetProp + "'" );

      final Object object = sourceFeature.getProperty( sourceProp );

      final Object newobject = cloneData( object, sourceFTP.getType() );

      targetFeature.setProperty( FeatureFactory.createFeatureProperty( targetProp, newobject ) );
    }
  }

  /**
   * @throws UnsupportedOperationException If type of object is not supported
   *           for clone
   */
  public static Object cloneData( final Object object, final String type )
  {
    if( object == null )
      return null;

    if( object instanceof String )
      return object;

    if( object instanceof Number )
      return object;

    if( object instanceof GM_Point )
    {
      final GM_Point point = (GM_Point)object;
      // todo: is there a universal clone-method for GM_Geometries?
      final GM_Position position = point.getPosition();
      final GM_Position newPos = GeometryFactory.createGM_Position( (double[])position.getAsArray().clone() );
      return GeometryFactory.createGM_Point( newPos, point.getCoordinateSystem() );
    }

    if( object instanceof TimeseriesLinkType )
    {
      // TODO: eigentlich clonen!
      return object;
    }

    throw new UnsupportedOperationException( "Kann Datenobjekt vom Typ '" + type + "' nicht kopieren." );
  }

  public static boolean isCompositionLink( Feature srcFE, String linkPropName, Feature destFE )
  {
    final Object property = srcFE.getProperty( linkPropName );
    if( property == null )
      return false;
    if( srcFE.getFeatureType().isListProperty( linkPropName ) )
    {
      // list:
      final List list = (List)property;
      return list.contains( destFE );
    }
    // no list:
    return property == destFE;
  }

  /**
   * @return position of link or -1 if relation does not exists
   */
  public static int getPositionOfAssoziation( Feature srcFE, String linkPropName, Feature destFE )
  {
    if( !srcFE.getFeatureType().isListProperty( linkPropName ) )
      return 0;

    final List list = (List)srcFE.getProperty( linkPropName );
    int pos = -1;
    pos = list.indexOf( destFE );
    if( pos > -1 )
      return pos;
    return list.indexOf( destFE.getId() );
  }
}