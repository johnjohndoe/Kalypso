package org.kalypsodeegree_impl.model.feature;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Map.Entry;

import org.apache.commons.lang.ArrayUtils;
import org.kalypso.contribs.java.lang.MultiException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureAssociationTypeProperty;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.extension.IMarshallingTypeHandler;
import org.kalypsodeegree_impl.extension.ITypeRegistry;
import org.kalypsodeegree_impl.extension.MarshallingTypeRegistrySingleton;

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

  public static String getFormatedDate( Feature feature, String propName, String simpleDateFormatPattern,
      String defaultValue )
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
   * <p>
   * Die Properties werden dabei anhand der übergebenen {@link Properties}zugeordnet.
   * 
   * Es gilt:
   * <ul>
   * <li>Es erfolgt ein Deep-Copy, inneliegende Features werden komplett kopiert.</li>
   * <li><Bei Referenzen auf andere Features erfolgt nur ein shallow copy, das Referenzierte Feature bleibt gleich./li>
   * <li>Die Typen der Zurodnung müssen passen, sonst gibts ne Exception.</li>
   * </ul>
   * 
   * @throws CloneNotSupportedException
   * 
   * @throws IllegalArgumentException
   *           Falls eine Zuordnung zwischen Properties unterschiedlkicher Typen erfolgt.
   * @throws NullPointerException
   *           falls eines der Argumente <codce>null</code> ist.
   * @throws UnsupportedOperationException
   *           Noch sind nicht alle Typen implementiert
   */
  public static void copyProperties( final Feature sourceFeature, final Feature targetFeature,
      final Properties propertyMap ) throws CloneNotSupportedException
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
        throw new IllegalArgumentException( "Typen der zugeordneten Properties sind unterschiedlich: '" + sourceProp
            + "' and '" + targetProp + "'" );

      final Object object = sourceFeature.getProperty( sourceProp );

      final Object newobject = cloneData( object, sourceFTP.getType() );

      targetFeature.setProperty( FeatureFactory.createFeatureProperty( targetProp, newobject ) );
    }
  }

  /**
   * @throws CloneNotSupportedException
   * @throws UnsupportedOperationException
   *           If type of object is not supported for clone
   */
  public static Object cloneData( final Object object, final String type ) throws CloneNotSupportedException
  {
    if( object == null )
      return null;

    // TODO: we still need a type handler for the simple types

    // imutable types
    if( object instanceof Boolean )
      return object;
    if( object instanceof String )
      return object;
    if( object instanceof Number )
      return object;

    // if we have an IMarhsallingTypeHandler, it will do the clone for us.
    final ITypeRegistry typeRegistry = MarshallingTypeRegistrySingleton.getTypeRegistry();
    final IMarshallingTypeHandler typeHandler = (IMarshallingTypeHandler)typeRegistry.getTypeHandlerForClassName( type );
    if( typeHandler != null )
      return typeHandler.cloneObject( object );

    // TODO: delete these lines AFTEr we have checked that it is still working
    //    if( object instanceof GM_Point )
    //    {
    //      final GM_Point point = (GM_Point)object;
    //      // todo: is there a universal clone-method for GM_Geometries?
    //      final GM_Position position = point.getPosition();
    //      final GM_Position newPos = GeometryFactory.createGM_Position( (double[])position.getAsArray().clone() );
    //      return GeometryFactory.createGM_Point( newPos, point.getCoordinateSystem() );
    //    }

    throw new CloneNotSupportedException( "Kann Datenobjekt vom Typ '" + type + "' nicht kopieren." );
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

  public static Feature[] getFeaturess( Object object )
  {
    if( object == null )
      return new Feature[] {};
    if( object instanceof Feature )
    {
      return new Feature[]
      { (Feature)object };
    }
    else if( object instanceof FeatureList )
    {
      return ( (FeatureList)object ).toFeatures();
    }
    else
    {
      throw new UnsupportedOperationException( "unexcepted object, can not convert to Feature[]" );
    }
  }

  /**
   * TODO change String argument to <code>class</code> type
   */
  public static boolean isGeometryType( String type )
  {
    return type.startsWith( GM_Object.class.getPackage().getName() );
  }

  /**
   * Checks if one of the feature properties is a collection.
   * 
   * @param f
   * @return It returns true after the first occurrenc of a list
   */
  public static boolean hasCollections( Feature f )
  {
    FeatureType featureType = f.getFeatureType();
    FeatureTypeProperty[] properties = featureType.getProperties();
    for( int i = 0; i < properties.length; i++ )
    {
      FeatureTypeProperty property = properties[i];
      if( featureType.isListProperty( property.getName() ) )
        return true;
    }
    return false;

  }

  public static int[] getPositionOfAllAssociations( Feature feature )
  {
    ArrayList res = new ArrayList();
    FeatureType featureType = feature.getFeatureType();
    FeatureTypeProperty[] properties = featureType.getProperties();
    for( int i = 0; i < properties.length; i++ )
    {
      FeatureTypeProperty property = properties[i];
      if( property instanceof FeatureAssociationTypeProperty )
      {
        res.add( new Integer( i ) );
      }
    }
    Integer[] positions = (Integer[])res.toArray( new Integer[res.size()] );
    return ArrayUtils.toPrimitive( positions );
  }

  public static FeatureAssociationTypeProperty[] getAllAssociations( Feature feature )
  {
    final ArrayList res = new ArrayList();
    final FeatureType featureType = feature.getFeatureType();
    final FeatureTypeProperty[] properties = featureType.getProperties();
    for( int i = 0; i < properties.length; i++ )
    {
      final FeatureTypeProperty property = properties[i];
      if( property instanceof FeatureAssociationTypeProperty )
        res.add( property );
    }
    return (FeatureAssociationTypeProperty[])res.toArray( new FeatureAssociationTypeProperty[res.size()] );
  }

  public static boolean isCollection( Feature f )
  {
    FeatureType featureType = f.getFeatureType();
    FeatureTypeProperty[] properties = featureType.getProperties();
    if( properties.length > 1 )
      return false;
    for( int i = 0; i < properties.length; i++ )
    {
      FeatureTypeProperty property = properties[i];
      if( featureType.isListProperty( property.getName() ) )
        return true;
    }
    return false;
  }

  public static boolean isFeatuerTypeInFeatureCollection( Feature feature, FeatureType ftToCheckFor )
  {
    if( isCollection( feature ) )
    {
      FeatureAssociationTypeProperty property = (FeatureAssociationTypeProperty)feature.getProperties()[0];
      FeatureType[] associationFeatureTypes = property.getAssociationFeatureTypes();
      for( int i = 0; i < associationFeatureTypes.length; i++ )
      {
        FeatureType type = associationFeatureTypes[i];
        if( type.equals( ftToCheckFor ) )
          return true;
      }
    }
    return false;
  }

  public static FeatureType[] getFeatureTypeFromCollection( Feature f )
  {
    FeatureType featureType = f.getFeatureType();
    FeatureTypeProperty[] properties = featureType.getProperties();
    FeatureTypeProperty property = featureType.getProperty( properties[0].getName() );

    FeatureType[] afT = null;
    if( property instanceof FeatureAssociationTypeProperty )
    {
      afT = ( (FeatureAssociationTypeProperty)property ).getAssociationFeatureTypes();
    }
    return afT;
  }

  /**
   * Create properties by using the property-value of the given feature for each of the replace-tokens
   * 
   * @param tokens
   *          replace-tokens (tokenKey-featurePropertyName;...)
   */
  public static Properties createReplaceTokens( final Feature f, final String tokens )
  {
    final Properties properties = new Properties();
    final String[] strings = tokens.split( ";" );
    for( int i = 0; i < strings.length; i++ )
    {
      final String[] splits = strings[i].split( "-" );
      String value = (String)f.getProperty( splits[1] );
      if( value == null )
        value = splits[1];

      properties.setProperty( splits[0], value );
    }

    return properties;
  }

  /**
   * copys all simple type properties from the source feature into the target feature
   * 
   * @param srcFE
   * @param targetFE
   * @throws MultiException
   */
  public static void copySimpleProperties( final Feature srcFE, final Feature targetFE ) throws MultiException
  {
    final MultiException multiException = new MultiException();
    final FeatureTypeProperty[] srcFTPs = srcFE.getFeatureType().getProperties();
    for( int i = 0; i < srcFTPs.length; i++ )
    {
      try
      {
        copySimpleProperty( srcFE, targetFE, srcFTPs[i] );
      }
      catch( CloneNotSupportedException e )
      {
        multiException.addException( e );
      }
    }
    if( !multiException.isEmpty() )
      throw multiException;
  }

  /**
   * 
   * @param srcFE
   * @param targetFE
   * @param property
   * @throws CloneNotSupportedException
   */
  public static void copySimpleProperty( Feature srcFE, Feature targetFE, FeatureTypeProperty property )
      throws CloneNotSupportedException
  {
    if( !( property instanceof FeatureAssociationTypeProperty ) )
    {
      final Object valueOriginal = srcFE.getProperty( property.getName() );
      final Object cloneValue;
      cloneValue = cloneData( valueOriginal, property.getType() );
      targetFE.setProperty( property.getName(), cloneValue );
    }
  }
}