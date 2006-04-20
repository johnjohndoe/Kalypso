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
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;

/**
 * @author doemming
 */
public class FeatureHelper
{
  public static IPropertyType getPT( Feature feature, String propName )
  {
    final IPropertyType[] properties = feature.getFeatureType().getProperties();

    for( int i = 0; i < properties.length; i++ )
    {
      final IPropertyType type = properties[i];
      if( propName.equals( type.getQName().getLocalPart() ) )
        return type;
    }
    return null;
  }

  public static boolean booleanIsTrue( Feature feature, String propName, boolean defaultStatus )
  {
    final Object property = feature.getProperty( propName );
    if( property != null && property instanceof Boolean )
      return ((Boolean) property).booleanValue();
    return defaultStatus;
  }

  public static String getFormatedDate( Feature feature, String propName, String simpleDateFormatPattern, String defaultValue )
  {
    final Object property = feature.getProperty( propName );
    if( property != null && property instanceof Date )
    {
      DateFormat dateFormat = new SimpleDateFormat( simpleDateFormatPattern );
      return dateFormat.format( (Date) property );
    }
    return defaultValue;

  }

  public static double getAsDouble( Feature feature, String propName, double defaultValue )
  {
    final Object value = feature.getProperty( propName );
    if( value == null )
      return defaultValue;
    if( value instanceof String )
      return Double.valueOf( (String) value ).doubleValue();
    // should be a Double
    return ((Double) value).doubleValue();
  }

  public static String getAsString( Feature feature, String propName )
  {
    final Object value = feature.getProperty( propName );
    // TODO use numberformat
    if( value == null )
      return null;
    if( value instanceof String )
      return (String) value;
    return value.toString();
  }

  /**
   * Überträgt die Daten eines Features in die Daten eines anderen.
   * <p>
   * Die Properties werden dabei anhand der übergebenen {@link Properties}zugeordnet. Es gilt:
   * <ul>
   * <li>Es erfolgt ein Deep-Copy, inneliegende Features werden komplett kopiert.</li>
   * <li><Bei Referenzen auf andere Features erfolgt nur ein shallow copy, das Referenzierte Feature bleibt gleich./li>
   * <li>Die Typen der Zurodnung müssen passen, sonst gibts ne Exception.</li>
   * </ul>
   * 
   * @throws CloneNotSupportedException
   * @throws IllegalArgumentException
   *           Falls eine Zuordnung zwischen Properties unterschiedlkicher Typen erfolgt.
   * @throws NullPointerException
   *           falls eines der Argumente <codce>null</code> ist.
   * @throws UnsupportedOperationException
   *           Noch sind nicht alle Typen implementiert
   */
  public static void copyProperties( final Feature sourceFeature, final Feature targetFeature, final Properties propertyMap ) throws CloneNotSupportedException
  {
    for( final Iterator pIt = propertyMap.entrySet().iterator(); pIt.hasNext(); )
    {
      final Map.Entry entry = (Entry) pIt.next();
      final String sourceProp = (String) entry.getKey();
      final String targetProp = (String) entry.getValue();

      final IValuePropertyType sourceFTP = (IValuePropertyType) getPT( sourceFeature, sourceProp );
      final IValuePropertyType targetFTP = (IValuePropertyType) getPT( targetFeature, targetProp );

      if( sourceFTP == null )
        throw new IllegalArgumentException( "Quell-Property existiert nicht: " + sourceProp );
      if( targetFTP == null )
        throw new IllegalArgumentException( "Ziel-Property existiert nicht: " + targetProp );
      if( !sourceFTP.getValueQName().equals( targetFTP.getValueQName() ) )
        throw new IllegalArgumentException( "Typen der zugeordneten Properties sind unterschiedlich: '" + sourceProp + "' and '" + targetProp + "'" );

      final Object object = sourceFeature.getProperty( sourceFTP );

      final Object newobject = cloneData( object, sourceFTP.getValueClass() );

      targetFeature.setProperty( FeatureFactory.createFeatureProperty( targetFTP, newobject ) );
    }
  }

  /**
   * @throws CloneNotSupportedException
   * @throws UnsupportedOperationException
   *           If type of object is not supported for clone
   */
  public static Object cloneData( final Object object, Class clazz ) throws CloneNotSupportedException
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
    final IMarshallingTypeHandler typeHandler = (IMarshallingTypeHandler) typeRegistry.getTypeHandlerForClassName( clazz );
    if( typeHandler != null )
      return typeHandler.cloneObject( object );

    // TODO: delete these lines AFTEr we have checked that it is still working
    // if( object instanceof GM_Point )
    // {
    // final GM_Point point = (GM_Point)object;
    // // todo: is there a universal clone-method for GM_Geometries?
    // final GM_Position position = point.getPosition();
    // final GM_Position newPos = GeometryFactory.createGM_Position( (double[])position.getAsArray().clone() );
    // return GeometryFactory.createGM_Point( newPos, point.getCoordinateSystem() );
    // }

    throw new CloneNotSupportedException( "Kann Datenobjekt vom Typ '" + clazz.getName() + "' nicht kopieren." );
  }

  public static boolean isCompositionLink( Feature srcFE, IRelationType linkProp, Feature destFE )
  {
    final Object property = srcFE.getProperty( linkProp );
    if( property == null )
      return false;
    if( linkProp.isList() )
    {
      // list:
      final List list = (List) property;
      return list.contains( destFE );
    }
    // no list:
    return property == destFE;
  }

  /**
   * @return position of link or -1 if relation does not exists
   */
  public static int getPositionOfAssoziation( Feature srcFE, IRelationType linkProp, Feature destFE )
  {
    if( !(linkProp.isList()) )
      return 0;

    final List list = (List) srcFE.getProperty( linkProp );
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
      return new Feature[] { (Feature) object };
    }
    else if( object instanceof FeatureList )
    {
      return ((FeatureList) object).toFeatures();
    }
    else
    {
      throw new UnsupportedOperationException( "unexcepted object, can not convert to Feature[]" );
    }
  }

  /**
   * Checks if one of the feature properties is a collection.
   * 
   * @param f
   * @return It returns true after the first occurrenc of a list
   */
  public static boolean hasCollections( Feature f )
  {
    IFeatureType featureType = f.getFeatureType();
    IPropertyType[] properties = featureType.getProperties();
    for( int i = 0; i < properties.length; i++ )
    {
      IPropertyType property = properties[i];
      if( property.isList() )
        return true;
    }
    return false;

  }

  public static int[] getPositionOfAllAssociations( Feature feature )
  {
    ArrayList res = new ArrayList();
    IFeatureType featureType = feature.getFeatureType();
    IPropertyType[] properties = featureType.getProperties();
    for( int i = 0; i < properties.length; i++ )
    {
      IPropertyType property = properties[i];
      if( property instanceof IRelationType )
      {
        res.add( new Integer( i ) );
      }
    }
    Integer[] positions = (Integer[]) res.toArray( new Integer[res.size()] );
    return ArrayUtils.toPrimitive( positions );
  }

  public static IRelationType[] getAllAssociations( Feature feature )
  {
    final ArrayList res = new ArrayList();
    final IFeatureType featureType = feature.getFeatureType();
    final IPropertyType[] properties = featureType.getProperties();
    for( int i = 0; i < properties.length; i++ )
    {
      final IPropertyType property = properties[i];
      if( property instanceof IRelationType )
        res.add( property );
    }
    return (IRelationType[]) res.toArray( new IRelationType[res.size()] );
  }

  public static boolean isCollection( Feature f )
  {

    final IFeatureType featureType = f.getFeatureType();
    final IPropertyType[] properties = featureType.getProperties();

    if( properties.length > 1 )
      return false;

    for( int i = 0; i < properties.length; i++ )
    {
      final IPropertyType property = properties[i];
      if( property.isList() )
        return true;
    }
    return false;
  }

  public static IFeatureType[] getFeatureTypeFromCollection( Feature f )
  {
    final IFeatureType featureType = f.getFeatureType();
    final IPropertyType[] properties = featureType.getProperties();
    final IPropertyType property = featureType.getProperty( properties[0].getQName() );

    IFeatureType[] afT = null;
    if( property instanceof IRelationType )
    {

      IFeatureType ft = ((IRelationType) property).getTargetFeatureType();
      afT = ft.getSubstituts( null, false, true );
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

      String value = (String) f.getProperty( splits[1] );
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
    final IPropertyType[] srcFTPs = srcFE.getFeatureType().getProperties();
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
   * @param srcFE
   * @param targetFE
   * @param property
   * @throws CloneNotSupportedException
   */
  public static void copySimpleProperty( Feature srcFE, Feature targetFE, IPropertyType property ) throws CloneNotSupportedException
  {
    if( property instanceof IValuePropertyType )
    {
      final IValuePropertyType pt = (IValuePropertyType) property;
      final Object valueOriginal = srcFE.getProperty( property );
      final Object cloneValue = cloneData( valueOriginal, pt.getValueClass() );
      targetFE.setProperty( pt, cloneValue );
    }
  }

}