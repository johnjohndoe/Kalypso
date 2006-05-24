package org.kalypsodeegree_impl.model.feature;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Map.Entry;

import javax.xml.namespace.QName;

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
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.sort.SplitSort;
import org.kalypsodeegree_impl.tools.GeometryUtilities;
import org.opengis.cs.CS_CoordinateSystem;

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

      final Object newobject = cloneData( object, sourceFTP );

      targetFeature.setProperty( targetFTP, newobject );
    }
  }

  /**
   * @throws CloneNotSupportedException
   * @throws UnsupportedOperationException
   *           If type of object is not supported for clone
   */
  public static Object cloneData( final Object object, final IPropertyType pt ) throws CloneNotSupportedException
  {
    if( object == null )
      return null;

    // if we have an IMarhsallingTypeHandler, it will do the clone for us.
    final ITypeRegistry<IMarshallingTypeHandler> typeRegistry = MarshallingTypeRegistrySingleton.getTypeRegistry();
    final IMarshallingTypeHandler typeHandler = typeRegistry.getTypeHandlerFor( pt );
    if( typeHandler != null )
      return typeHandler.cloneObject( object );

    throw new CloneNotSupportedException( "Kann Datenobjekt vom Typ '" + pt.getQName() + "' nicht kopieren." );
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
    final ArrayList<Integer> res = new ArrayList<Integer>();
    final IFeatureType featureType = feature.getFeatureType();
    final IPropertyType[] properties = featureType.getProperties();
    for( int i = 0; i < properties.length; i++ )
    {
      final IPropertyType property = properties[i];
      if( property instanceof IRelationType )
        res.add( new Integer( i ) );
    }
    Integer[] positions = res.toArray( new Integer[res.size()] );
    return ArrayUtils.toPrimitive( positions );
  }

  public static IRelationType[] getAllAssociations( Feature feature )
  {
    final ArrayList<IRelationType> res = new ArrayList<IRelationType>();
    final IFeatureType featureType = feature.getFeatureType();
    final IPropertyType[] properties = featureType.getProperties();
    for( final IPropertyType property : properties )
    {
      if( property instanceof IRelationType )
        res.add( (IRelationType) property );
    }
    return res.toArray( new IRelationType[res.size()] );
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
   * Returns the value of the given property. If the property is a java.util.List, it then returns the first element of
   * the list or null if the list is empty.
   */
  public static Object getFirstProperty( final Feature feature, final QName property )
  {
    Object prop = feature.getProperty( property );

    if( prop == null )
      return null;

    if( prop instanceof List )
    {
      final List list = (List) prop;

      if( list.size() > 0 )
        return list.get( 0 );
      else
        return null;
    }

    return prop;
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
      final Object cloneValue = cloneData( valueOriginal, pt );
      targetFE.setProperty( pt, cloneValue );
    }
  }

  /**
   * <ul>
   * <li>If the property is not a list, just set the value</li>
   * <li>If the property ist a list, a the given value to the list. If the given value is a list, add all its values to
   * the list.</li>
   * </ul>
   * 
   * @see org.kalypsodeegree.model.feature.Feature#addProperty(org.kalypsodeegree.model.feature.FeatureProperty)
   */
  public static void addProperty( final Feature feature, final IPropertyType pt, final Object newValue )
  {
    final Object oldValue = feature.getProperty( pt );

    if( oldValue instanceof List )
    {
      if( newValue instanceof List )
        ((List) oldValue).addAll( (List) newValue );
      else
        ((List) oldValue).add( newValue );
    }
    else
      feature.setProperty( pt, newValue );
  }

  public static void setProperties( final Feature result, final Map<IPropertyType, Object> props )
  {
    for( final Map.Entry<IPropertyType, Object> entry : props.entrySet() )
      result.setProperty( entry.getKey(), entry.getValue() );
  }

  /**
   * Returns a value of the given feature as feature. If it is a link, it will be resolved.
   * 
   * @param qname
   *          Must denote a property of type IRelationType of maxoccurs 1.
   */
  public static Feature resolveLink( final Feature feature, final QName qname )
  {
    final IRelationType property = (IRelationType) feature.getFeatureType().getProperty( qname );
    final Object value = feature.getProperty( property );
    if( value instanceof Feature )
      return (Feature) value;
    else
      return feature.getWorkspace().getFeature( (String) value );
  }

  public static void addChild( final Feature parentFE, final IRelationType rt, final Feature childFE )
  {
    if( rt.isList() )
    {
      final FeatureList list = (FeatureList) parentFE.getProperty( rt );
      list.add( childFE );
    }
    else
      parentFE.setProperty( rt, childFE );
  }

  public static void addChild( Feature parentFE, IRelationType rt, String featureID )
  {
    if( rt.isList() )
    {
      final FeatureList list = (FeatureList) parentFE.getProperty( rt );
      list.add( featureID );
    }
    else
      parentFE.setProperty( rt, featureID );
  }

  /**
   * Creates a data object suitable for a feature property out of string.
   * 
   * @return null, if the data-type is unknown
   * @throws NumberFormatException
   */
  public static final Object createFeaturePropertyFromStrings( final IValuePropertyType type, final String format, final String[] input )
  {
    final IMarshallingTypeHandler typeHandler = MarshallingTypeRegistrySingleton.getTypeRegistry().getTypeHandlerFor( type );

    if( GeometryUtilities.getPointClass() == type.getValueClass() )
    {
      final String rwString = input[0].trim();
      final String hwString = input[1].trim();

      final String crsString;
      if( input.length > 2 )
        crsString = input[2];
      else
        crsString = format;

      final CS_CoordinateSystem crs = ConvenienceCSFactory.getInstance().getOGCCSByName( crsString );
      if( rwString == null || rwString.length() == 0 || hwString == null || hwString.length() == 0 )
        return GeometryFactory.createGM_Point( 0, 0, crs );

      final double rw = Double.parseDouble( rwString );
      final double hw = Double.parseDouble( hwString );

      return GeometryFactory.createGM_Point( rw, hw, crs );
    }

    if( typeHandler != null )
    {
      try
      {
        return typeHandler.parseType( input[0] );
      }
      catch( final ParseException e )
      {
        e.printStackTrace();
      }
    }

    return null;
  }

  public static void resortFeature( Feature feature )
  {
    Feature parent = feature.getParent();
    if( parent == null )
      return; // nothing to do
    final IPropertyType[] properties = parent.getFeatureType().getProperties();
    for( IPropertyType propType : properties )
    {
      if( propType instanceof IRelationType )
      {
        final IRelationType relationType = (IRelationType) propType;
        if( relationType.isList() )
        {
          final Object property = parent.getProperty( relationType );
          if( property instanceof SplitSort )
          {
            final SplitSort sort = (SplitSort) property;
            sort.resort( feature );
          }
        }
      }
    }
  }

  /**
   * Retrieves a property as a feature. Linked features are not suported.
   * <p>
   * If the property is not yet set, the feature is generated and set.
   * </p>
   */
  public static Feature getSubFeature( final Feature parent, final QName propertyName )
  {
    final Feature subFeature = (Feature) parent.getProperty( propertyName );
    if( subFeature != null )
      return subFeature;

    final IFeatureType parentType = parent.getFeatureType();
    final IPropertyType property = parentType.getProperty( propertyName );
    if( !(property instanceof IRelationType) )
      throw new IllegalArgumentException( "Property is no relation: " + propertyName );

    final IRelationType rt = (IRelationType) property;
    final IFeatureType targetFeatureType = rt.getTargetFeatureType();

    // neues machen
    final GMLWorkspace workspace = parent.getWorkspace();
    final Feature newSubFeature = workspace.createFeature( parent, targetFeatureType );
    parent.setProperty( propertyName, newSubFeature );
    return newSubFeature;
  }

  /**
   * @see #addProperty(Feature, IPropertyType, Object)
   */
  public static void addProperty( final Feature feature, final QName propertyName, final Object value )
  {
    final IPropertyType property = feature.getFeatureType().getProperty( propertyName );
    addProperty( feature, property, value );
  }

  /**
   * If the given object is a feature, return it, else return the feature with id (String)linkOrFeature.
   */
  public static Feature getFeature( final GMLWorkspace wrk, final Object linkOrFeature )
  {
    if( linkOrFeature instanceof Feature )
      return (Feature) linkOrFeature;

    if( linkOrFeature instanceof String )
      return wrk.getFeature( (String) linkOrFeature );

    return null;
  }
}