package org.kalypsodeegree_impl.model.feature;

import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Map.Entry;

import javax.xml.namespace.QName;

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.core.runtime.Assert;
import org.kalypso.commons.tokenreplace.ITokenReplacer;
import org.kalypso.commons.tokenreplace.TokenReplacerEngine;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.java.lang.MultiException;
import org.kalypso.contribs.javax.xml.namespace.QNameUtilities;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.annotation.AnnotationUtilities;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author doemming
 */
public class FeatureHelper
{
  private static ITokenReplacer TR_FEATUREID = new ITokenReplacer()
  {
    public String replaceToken( final Object value, final String argument )
    {
      return ((Feature) value).getId();
    }

    public String getToken( )
    {
      return "id";
    }
  };

  private static ITokenReplacer TR_PROPERTYVALUE = new ITokenReplacer()
  {
    public String replaceToken( final Object value, final String argument )
    {
      final Feature feature = (Feature) value;

      final String[] strings = argument.split( ";" );
      if( strings.length == 0 )
        return "No argument for property. Must be _qname_;[null-value];[format-string]";

      final String propName = strings[0];
      final String nullValue = strings.length > 1 ? strings[1] : null;
      final String formatString = strings.length > 2 ? strings[2] : null;

      final QName qname = QNameUtilities.createQName( propName );
      final Object property = feature.getProperty( qname );

      if( property == null )
        return "" + nullValue;

      if( formatString != null )
        return String.format( formatString, property );

      return "" + property;
    }

    public String getToken( )
    {
      return "property";
    }
  };

  private static ITokenReplacer TR_LISTPROPERTYVALUE = new ITokenReplacer()
  {
    public String replaceToken( final Object value, final String argument )
    {
      final Feature feature = (Feature) value;

      final String[] strings = argument.split( ";" );
      if( strings.length < 2 )
        return "Wrong argument for listProperty. Must be _qname_;listindex;[null-Value]";

      final QName qname = QNameUtilities.createQName( strings[0] );
      final int listindex = Integer.parseInt( strings[1] );
      final String nullValue = strings.length > 2 ? strings[2] : null;

      final List list = (List) feature.getProperty( qname );

      if( listindex >= list.size() )
        return "" + nullValue;

      final Object propertyValue = list.get( listindex );
      if( propertyValue == null )
        return "" + nullValue;

      return "" + propertyValue;
    }

    public String getToken( )
    {
      return "listProperty";
    }
  };

  private static TokenReplacerEngine FEATURE_TOKEN_REPLACE = new TokenReplacerEngine( new ITokenReplacer[] { FeatureHelper.TR_FEATUREID, FeatureHelper.TR_PROPERTYVALUE,
      FeatureHelper.TR_LISTPROPERTYVALUE } );

  /**
   * @deprecated Do not use strings as property names. Use {@link IFeatureType#getProperty(QName)} instead.
   */
  @Deprecated
  public static IPropertyType getPT( final Feature feature, final String propName )
  {
    final IPropertyType[] properties = feature.getFeatureType().getProperties();

    for( final IPropertyType type : properties )
      if( propName.equals( type.getQName().getLocalPart() ) )
        return type;
    return null;
  }

  /**
   * @deprecated use booleanIsTrue( Feature feature, QName propQName, boolean defaultStatus )
   */
  @Deprecated
  public static boolean booleanIsTrue( final Feature feature, final String propName, final boolean defaultStatus )
  {
    final Object property = feature.getProperty( propName );
    if( (property != null) && (property instanceof Boolean) )
      return ((Boolean) property).booleanValue();
    return defaultStatus;
  }

  public static boolean booleanIsTrue( final Feature feature, final QName propQName, final boolean defaultStatus )
  {
    final Object property = feature.getProperty( propQName );
    if( (property != null) && (property instanceof Boolean) )
      return ((Boolean) property).booleanValue();
    return defaultStatus;
  }

  public static String getFormatedDate( final Feature feature, final String propName, final String simpleDateFormatPattern, final String defaultValue )
  {
    final Object property = feature.getProperty( propName );
    if( (property != null) && (property instanceof Date) )
    {
      final DateFormat dateFormat = new SimpleDateFormat( simpleDateFormatPattern );
      return dateFormat.format( (Date) property );
    }
    return defaultValue;

  }

  public static double getAsDouble( final Feature feature, final QName propQName, final double defaultValue )
  {
    final Object value = feature.getProperty( propQName );
    if( value == null )
      return defaultValue;
    if( value instanceof String )
      return Double.valueOf( (String) value ).doubleValue();
    // should be a Double
    if( value instanceof BigDecimal )
      return ((BigDecimal) value).doubleValue();
    return ((Double) value).doubleValue();
  }

  /**
   * @deprecated use instead of propName the QName of the property
   */
  @Deprecated
  public static double getAsDouble( final Feature feature, final String propName, final double defaultValue )
  {
    final Object value = feature.getProperty( propName );
    if( value == null )
      return defaultValue;
    if( value instanceof String )
      return Double.valueOf( (String) value ).doubleValue();
    // should be a Double
    return ((Double) value).doubleValue();
  }

  public static String getAsString( final Feature feature, final String propName )
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
   * Die Properties werden dabei anhand der übergebenen {@link Properties} zugeordnet. Es gilt: TODO: die Doku ist
   * quatsch, relation properties werden im Moment gar nicht kopiert!
   * <ul>
   * <li>Es erfolgt ein Deep-Copy, inneliegende Features werden komplett kopiert.</li>
   * <li><Bei Referenzen auf andere Features erfolgt nur ein shallow copy, das Referenzierte Feature bleibt gleich./li>
   * <li>Die Typen der Zurodnung müssen passen, sonst gibts ne Exception.</li>
   * </ul>
   * 
   * @throws CloneNotSupportedException
   * @throws IllegalArgumentException
   *             Falls eine Zuordnung zwischen Properties unterschiedlkicher Typen erfolgt.
   * @throws NullPointerException
   *             falls eines der Argumente <codce>null</code> ist.
   * @throws UnsupportedOperationException
   *             Noch sind nicht alle Typen implementiert
   */
  public static void copyProperties( final Feature sourceFeature, final Feature targetFeature, final Properties propertyMap ) throws Exception
  {
    final GMLWorkspace workspace = sourceFeature.getWorkspace();
    final String gmlVersion = workspace == null ? null : workspace.getGMLSchema().getGMLVersion();
    for( final Object element : propertyMap.entrySet() )
    {
      final Map.Entry entry = (Entry) element;
      final String sourceProp = (String) entry.getKey();
      final String targetProp = (String) entry.getValue();

      final IValuePropertyType sourceFTP = (IValuePropertyType) FeatureHelper.getPT( sourceFeature, sourceProp );
      final IValuePropertyType targetFTP = (IValuePropertyType) FeatureHelper.getPT( targetFeature, targetProp );

      if( sourceFTP == null )
        throw new IllegalArgumentException( "Quell-Property existiert nicht: " + sourceProp );
      if( targetFTP == null )
        throw new IllegalArgumentException( "Ziel-Property existiert nicht: " + targetProp );
      if( !sourceFTP.getValueQName().equals( targetFTP.getValueQName() ) )
        throw new IllegalArgumentException( "Typen der zugeordneten Properties sind unterschiedlich: '" + sourceProp + "' and '" + targetProp + "'" );

      final Object object = sourceFeature.getProperty( sourceFTP );

      final Object newobject = FeatureHelper.cloneData( sourceFeature, targetFeature, sourceFTP, object, gmlVersion );

      targetFeature.setProperty( targetFTP, newobject );
    }
  }

  /**
   * Clones a feature and puts it into the given parent feature at the given property.
   * 
   * @param newParentFeature
   *            The parent where the cloned feature will be put into. May live in the same or in another workspace.
   * @param relation
   *            Property where to put the new feature. If a list, the new feature is added at the end of the list.
   * @param nullValuedProperties
   *            qname of IPropertiesTypes whos values will not be copied. only the featuretype will exist and the
   *            property of its is null in result feature
   */
  public static Feature cloneFeature( final Feature newParentFeature, final IRelationType relation, final Feature featureToClone, final QName[] nullValuedProperties ) throws Exception
  {
    final IFeatureType featureType = featureToClone.getFeatureType();

    final Feature newFeature = newParentFeature.getWorkspace().createFeature( newParentFeature, relation, featureType );
    // TODO: this is no good to add it here....
    // For example we cannot give a position where to add the new feature...
    if( relation.isList() )
    {
      /*
       * Get relation in the type system from the cloned feature, because the 'old'-relation may not work with the new
       * feature type (due to the fact that some implementation did not implement a fitting equals method).
       */
      final IRelationType newRelation = (IRelationType) newParentFeature.getFeatureType().getProperty( relation.getQName() );
      newParentFeature.getWorkspace().addFeatureAsComposition( newParentFeature, newRelation, -1, newFeature );
    }
    else
      newParentFeature.setProperty( relation, newFeature );

    final IPropertyType[] properties = featureType.getProperties();
    for( final IPropertyType pt : properties )
    {
      if( ArrayUtils.contains( nullValuedProperties, pt.getQName() ) )
        continue;

      try
      {
        final Object newValue = FeatureHelper.cloneProperty( featureToClone, newFeature, pt );
        newFeature.setProperty( pt, newValue );
      }
      catch( final CloneNotSupportedException e )
      {
        /* Just log, try to copy at least the rest */
        KalypsoDeegreePlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
      }
    }

    return newFeature;
  }

  /**
   * Clones a feature and puts it into the given parent feature at the given property.
   */
  public static Feature cloneFeature( final Feature newParentFeature, final IRelationType relation, final Feature featureToClone ) throws Exception
  {
    return FeatureHelper.cloneFeature( newParentFeature, relation, featureToClone, new QName[0] );
  }

  private static Object cloneProperty( final Feature sourceFeature, final Feature targetFeature, final IPropertyType pt ) throws Exception
  {
    final String version = sourceFeature.getWorkspace().getGMLSchema().getGMLVersion();

    final Object property = sourceFeature.getProperty( pt );
    if( pt.isList() )
    {
      final List list = (List) property;
      final List targetList = (List) targetFeature.getProperty( pt );

      for( final Object listElement : list )
      {
        final Object cloneData = FeatureHelper.cloneData( sourceFeature, targetFeature, pt, listElement, version );
        // TODO: this is not nice! Better: d not add feature to list within the cloneFeature Method
        if( (cloneData instanceof XLinkedFeature_Impl) || !(cloneData instanceof Feature) )
          targetList.add( cloneData );
      }

      return targetList;
    }

    return FeatureHelper.cloneData( sourceFeature, targetFeature, pt, property, version );
  }

  /**
   * @throws CloneNotSupportedException
   * @throws UnsupportedOperationException
   *             If type of object is not supported for clone
   */
  private static Object cloneData( final Feature sourceFeature, final Feature targetFeature, final IPropertyType pt, final Object object, final String gmlVersion ) throws Exception
  {
    if( object == null )
      return null;

    if( pt instanceof IRelationType )
    {
      final IRelationType rt = (IRelationType) pt;

      if( object instanceof String )
      {
        // its an internal link: change to external if we change the workspace
        if( sourceFeature.getWorkspace().equals( targetFeature.getWorkspace() ) )
          return object;
        else
          // TODO: not yet supported; internal links will be broken after clone
          return null;
      }
      else if( object instanceof XLinkedFeature_Impl )
      {
        final XLinkedFeature_Impl xlink = (XLinkedFeature_Impl) object;
        // retarget xlink
        return new XLinkedFeature_Impl( targetFeature, rt, xlink.getFeatureType(), xlink.getHref(), xlink.getRole(), xlink.getArcrole(), xlink.getTitle(), xlink.getShow(), xlink.getActuate() );
      }
      else if( object instanceof Feature )
        return FeatureHelper.cloneFeature( targetFeature, rt, (Feature) object );

      return null;
    }

    // its an geometry? -> clone geometry
    if( object instanceof GM_Object )
    {
      final GM_Object gmo = (GM_Object) object;

      return gmo.clone();
    }

    // if we have an IMarshallingTypeHandler, it will do the clone for us.
    final ITypeRegistry<IMarshallingTypeHandler> typeRegistry = MarshallingTypeRegistrySingleton.getTypeRegistry();
    final IMarshallingTypeHandler typeHandler = typeRegistry.getTypeHandlerFor( pt );

    if( typeHandler != null )
      try
      {
        return typeHandler.cloneObject( object, gmlVersion );
      }
      catch( final Exception e )
      {
        final CloneNotSupportedException cnse = new CloneNotSupportedException( "Kann Datenobjekt vom Typ '" + pt.getQName() + "' nicht kopieren." );
        cnse.initCause( e );
        throw cnse;
      }
    throw new CloneNotSupportedException( "Kann Datenobjekt vom Typ '" + pt.getQName() + "' nicht kopieren." );
  }

  // /**
  // * Clones a property of a feature. The clone is deep, i.e. inline feature are also cloned, referenced feature are
  // kept
  // * as reference.
  // */
  // private static Object cloneProperty( final Feature feature, final IPropertyType pt ) throws
  // CloneNotSupportedException
  // {
  // final String version = feature.getWorkspace().getGMLSchema().getGMLVersion();
  //
  // final Object property = feature.getProperty( pt );
  // if( pt.isList() )
  // {
  // final List list = (List) property;
  // final List<Object> otherList;
  //
  // if( pt instanceof IRelationType )
  // otherList = FeatureFactory.createFeatureList( feature.getParent(), (IRelationType) pt );
  // else
  // otherList = new ArrayList<Object>( list.size() );
  // for( final Object listElement : list )
  // otherList.add( cloneData( listElement, pt, version ) );
  //
  // return otherList;
  // }
  //
  // return cloneData( property, pt, version );
  // }

  public static boolean isCompositionLink( final Feature srcFE, final IRelationType linkProp, final Feature destFE )
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
  public static int getPositionOfAssoziation( final Feature srcFE, final IRelationType linkProp, final Feature destFE )
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

  public static Feature[] getFeaturess( final Object object )
  {
    if( object == null )
      return new Feature[] {};
    if( object instanceof Feature )
      return new Feature[] { (Feature) object };
    else if( object instanceof FeatureList )
      return ((FeatureList) object).toFeatures();
    else
      throw new UnsupportedOperationException( "unexcepted object, can not convert to Feature[]" );
  }

  /**
   * Checks if one of the feature properties is a collection.
   * 
   * @param f
   * @return It returns true after the first occurrenc of a list
   */
  public static boolean hasCollections( final Feature f )
  {
    final IFeatureType featureType = f.getFeatureType();
    final IPropertyType[] properties = featureType.getProperties();
    for( final IPropertyType property : properties )
      if( property.isList() )
        return true;
    return false;

  }

  public static int[] getPositionOfAllAssociations( final Feature feature )
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
    final Integer[] positions = res.toArray( new Integer[res.size()] );
    return ArrayUtils.toPrimitive( positions );
  }

  public static IRelationType[] getAllAssociations( final Feature feature )
  {
    final ArrayList<IRelationType> res = new ArrayList<IRelationType>();
    final IFeatureType featureType = feature.getFeatureType();
    final IPropertyType[] properties = featureType.getProperties();
    for( final IPropertyType property : properties )
      if( property instanceof IRelationType )
        res.add( (IRelationType) property );
    return res.toArray( new IRelationType[res.size()] );
  }

  public static boolean isCollection( final Feature f )
  {

    final IFeatureType featureType = f.getFeatureType();
    final IPropertyType[] properties = featureType.getProperties();

    if( properties.length > 1 )
      return false;

    for( final IPropertyType property : properties )
      if( property.isList() )
        return true;
    return false;
  }

  public static IFeatureType[] getFeatureTypeFromCollection( final Feature f )
  {
    final IFeatureType featureType = f.getFeatureType();
    final IPropertyType[] properties = featureType.getProperties();
    final IPropertyType property = featureType.getProperty( properties[0].getQName() );

    IFeatureType[] afT = null;
    if( property instanceof IRelationType )
    {

      final IFeatureType ft = ((IRelationType) property).getTargetFeatureType();
      afT = GMLSchemaUtilities.getSubstituts( ft, null, false, true );
    }
    return afT;
  }

  /**
   * Create properties by using the property-value of the given feature for each of the replace-tokens
   * 
   * @param tokens
   *            replace-tokens (tokenKey-featurePropertyName;...)
   */
  public static Properties createReplaceTokens( final Feature f, final String tokens )
  {
    final Properties properties = new Properties();
    final String[] strings = tokens.split( ";" );
    for( final String element : strings )
    {
      final String[] splits = element.split( "-" );

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
    final Object prop = feature.getProperty( property );

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
    for( final IPropertyType element : srcFTPs )
      try
      {
        FeatureHelper.copySimpleProperty( srcFE, targetFE, element );
      }
      catch( final Exception e )
      {
        multiException.addException( e );
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
  public static void copySimpleProperty( final Feature srcFE, final Feature targetFE, final IPropertyType property ) throws Exception
  {
    if( property instanceof IValuePropertyType )
    {
      final GMLWorkspace workspace = srcFE.getWorkspace();
      final String gmlVersion = workspace == null ? null : workspace.getGMLSchema().getGMLVersion();

      final IValuePropertyType pt = (IValuePropertyType) property;
      final Object valueOriginal = srcFE.getProperty( property );
      final Object cloneValue = FeatureHelper.cloneData( srcFE, targetFE, pt, valueOriginal, gmlVersion );
      targetFE.setProperty( pt, cloneValue );
    }
  }

  public static void copyNoRelationPropterty( final Feature srcFE, final Feature targetFE ) throws CloneNotSupportedException
  {
    final IFeatureType sourceFT = srcFE.getFeatureType();
    final IFeatureType targetFT = targetFE.getFeatureType();
    final String gmlVersion = srcFE.getWorkspace().getGMLSchema().getGMLVersion();
    if( !sourceFT.equals( targetFT ) )
      throw new CloneNotSupportedException( "source FeatureType=" + sourceFT.getQName() + " is not the same as target featureType=" + targetFT.getQName() );
    final IPropertyType[] properties = sourceFT.getProperties();
    for( final IPropertyType pt : properties )
      if( pt instanceof IValuePropertyType )
      {
        final IValuePropertyType vpt = (IValuePropertyType) pt;
        final IMarshallingTypeHandler sourceTH = vpt.getTypeHandler();
        final Object clonedProptery = sourceTH.cloneObject( srcFE.getProperty( vpt ), gmlVersion );
        targetFE.setProperty( pt, clonedProptery );
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

  /**
   * Same as {@link #addFeature(Feature, QName, QName, 0)}
   */
  public static Feature addFeature( final Feature feature, final QName listProperty, final QName newFeatureName ) throws GMLSchemaException
  {
    return addFeature( feature, listProperty, newFeatureName, 0 );
  }

  /**
   * Adds a new member to a property of the given feature. The property must be a feature list.
   * 
   * @param newFeatureName
   *            The QName of the featureType of the newly generated feature. If null, the target feature-type of the
   *            list is taken.
   * @return The new feature member
   */
  public static Feature addFeature( final Feature feature, final QName listProperty, final QName newFeatureName, final int depth ) throws GMLSchemaException
  {
    final FeatureList list = (FeatureList) feature.getProperty( listProperty );
    final Feature parentFeature = list.getParentFeature();
    final GMLWorkspace workspace = parentFeature.getWorkspace();

    final IRelationType parentFeatureTypeProperty = list.getParentFeatureTypeProperty();
    final IFeatureType targetFeatureType = parentFeatureTypeProperty.getTargetFeatureType();

    final IFeatureType newFeatureType;
    if( newFeatureName == null )
      newFeatureType = targetFeatureType;
    else
      newFeatureType = workspace.getGMLSchema().getFeatureType( newFeatureName );

    if( (newFeatureName != null) && !GMLSchemaUtilities.substitutes( newFeatureType, targetFeatureType.getQName() ) )
      throw new GMLSchemaException( "Type of new feature (" + newFeatureName + ") does not substitutes target feature type of the list: " + targetFeatureType.getQName() );

    final Feature newFeature = workspace.createFeature( parentFeature, parentFeatureTypeProperty, newFeatureType, depth );

    list.add( newFeature );
    return newFeature;
  }

  /**
   * Only works for non list feature property
   * 
   * @param feature
   *            feature which list property receive the new feature
   * @param listProperty
   *            the {@link QName} of the list property
   * @param featureProperties
   *            the property of the feature to be set before adding the feature to the list
   * @param featurePropQNames
   *            the {@link QName} of the feature property to be set before adding it the the
   * @param throws
   *            {@link IllegalArgumentException} if featureProperties and featurePropQNames are not all null or are not
   *            all non null with differen lengths
   */
  public static Feature addFeature( final Feature feature, final QName listProperty, final QName newFeatureName, final Object[] featureProperties, final QName[] featurePropQNames ) throws GMLSchemaException, IllegalArgumentException
  {
    if( ((featureProperties == null) & (featurePropQNames == null)) )
    {
      // okay
    }
    else if( (featureProperties != null) && (featurePropQNames != null) )
    {
      if( featureProperties.length != featurePropQNames.length )
        throw new IllegalArgumentException( "featurePropQName and featureProperties must have the same length" );

    }
    else
      throw new IllegalArgumentException( "featureProperties and FeaturePropQnames must be all null or all non null with" + "the same length" );
    final FeatureList list = (FeatureList) feature.getProperty( listProperty );
    // TODO Patrice to check can the feature(param) be different from the list property parent
    final Feature parentFeature = list.getParentFeature();
    final GMLWorkspace workspace = parentFeature.getWorkspace();

    final IRelationType parentFeatureTypeProperty = list.getParentFeatureTypeProperty();
    final IFeatureType targetFeatureType = parentFeatureTypeProperty.getTargetFeatureType();

    final IFeatureType newFeatureType;
    if( newFeatureName == null )
      newFeatureType = targetFeatureType;
    else
      newFeatureType = workspace.getGMLSchema().getFeatureType( newFeatureName );

    if( (newFeatureName != null) && !GMLSchemaUtilities.substitutes( newFeatureType, targetFeatureType.getQName() ) )
      throw new GMLSchemaException( "Type of new feature (" + newFeatureName + ") does not substitutes target feature type of the list: " + targetFeatureType.getQName() );

    final Feature newFeature = workspace.createFeature( parentFeature, parentFeatureTypeProperty, newFeatureType );
    for( int i = featureProperties.length - 1; i >= 0; i-- )
      newFeature.setProperty( featurePropQNames[i], featureProperties[i] );

    list.add( newFeature );

    return newFeature;
  }

  public static void setProperties( final Feature result, final Map<IPropertyType, Object> props )
  {
    for( final Map.Entry<IPropertyType, Object> entry : props.entrySet() )
      result.setProperty( entry.getKey(), entry.getValue() );
  }

  public static Feature resolveLink( final Feature feature, final QName qname )
  {
    return FeatureHelper.resolveLink( feature, qname, false );
  }

  /**
   * Returns a value of the given feature as feature. If it is a link, it will be resolved.
   * 
   * @param qname
   *            Must denote a property of type IRelationType of maxoccurs 1.
   * @param followXLinks
   *            If true and the property is an xlinked Feature, return the Feature where the xlink points to. Else the
   *            xlink itself is returned as feature.
   */
  public static Feature resolveLink( final Feature feature, final QName qname, final boolean followXLinks )
  {
    final IRelationType property = (IRelationType) feature.getFeatureType().getProperty( qname );
    final Object value = feature.getProperty( property );

    if( (value instanceof XLinkedFeature_Impl) && (followXLinks == true) )
      return ((XLinkedFeature_Impl) value).getFeature();

    if( value instanceof Feature )
      return (Feature) value;
    else /* Its a local link inside a xlinked-feature */
    if( feature instanceof XLinkedFeature_Impl )
    {
      final XLinkedFeature_Impl xlinkedFeature = (XLinkedFeature_Impl) feature;
      final String href = xlinkedFeature.getUri() + "#" + value;
      return new XLinkedFeature_Impl( feature, property, property.getTargetFeatureType(), href, "", "", "", "", "" );
    }
    else if( value == null )
      return null;
    else
      /* A normal local link inside the same workspace */
      return feature.getWorkspace().getFeature( (String) value );

  }

  /**
   * Resolves and adapts the linked feature. Note that the real feature is wrapped and return not the xlinked feature.
   * 
   * @param feature
   *            the link property holder
   * @param propertyQName
   *            the q-name of the link property
   * @param adapterTargetClass
   *            the class the link feature is to be adapted to
   * @throws IllegalArgumentException
   *             if any of the parameter is null
   * @throws IllegalStateException
   *             if xlink is broken (i.e. xlinked feature points to non existing real feature)
   * @return an adapter if the link feature or null if no linked feature is found or if the linked feature is not
   *         adaptable to the specified class
   */
  public static final <T> T resolveLink( final Feature feature, final QName propertyQName, final Class<T> adapterTargetClass )
  {
    if( (feature == null) || (propertyQName == null) || (adapterTargetClass == null) )
    {
      final String message = String.format( "Arguments must not be null : \n\tfeature = %s " + "\n\tpropertyQName = %s \n\tadapterTargetClass = %s\n\t", feature, propertyQName, adapterTargetClass );
      throw new IllegalArgumentException( message );
    }
    Feature propFeature = FeatureHelper.resolveLink( feature, propertyQName );
    if( propFeature == null )
      return null;
    else
    {
      if( propFeature instanceof XLinkedFeature_Impl )
        // here is also possible to get IllegalArgumentException, if (phantom) xlinked feature points to nothing
        propFeature = ((XLinkedFeature_Impl) propFeature).getFeature();
      final T adaptedFeature = (T) propFeature.getAdapter( adapterTargetClass );
      return adaptedFeature;
    }
  }

  public static final <T> T resolveLink( final IFeatureWrapper2 featureWrapper, final QName propertyQName, final Class<T> adapterTargetClass )
  {
    if( (featureWrapper == null) || (propertyQName == null) || (adapterTargetClass == null) )
    {
      final String message = String.format( "All argument must not be null:" + "\n\tfeatureWrapper=%s" + "\n\tpropertyQname = %s" + "\n\tadapterTargetClass = %s", featureWrapper, propertyQName, adapterTargetClass );
      throw new IllegalArgumentException( message );
    }

    final Feature wrappedFeature = featureWrapper.getFeature();
    final T resolvedLink = FeatureHelper.resolveLink( wrappedFeature, propertyQName, adapterTargetClass );
    return resolvedLink;
  }

  /**
   * set a workspace local link between 2 feature wrappers; i.e. the object feature is set as property with the given
   * name of the subject feature.<br/> <b>Note that no check is made to assert whether the property exists or is not a
   * list feature</b>
   * 
   * @param subjectFeature
   *            the feature wrapper whose property is to be set
   * @param propertyQName
   *            the q-name denoting the property type
   * @param objectFeature
   *            the feature to set as property
   * @throws IllegalArgumentException
   *             if subjectFeature or property q-name is null
   */
  public static final <T> void setLocalLink( final IFeatureWrapper2 subjectFeature, final QName propertyQName, final IFeatureWrapper2 objectFeature )
  {
    if( (subjectFeature == null) || (propertyQName == null) )
    {
      final String message = String.format( "Argument subjectFeature and propertyName " + "must not be null:" + "\n\tsubjectFeature=%s" + "propertyName=%s", subjectFeature, objectFeature );
      throw new IllegalArgumentException( message );
    }

    // get object id
    final String objectID = (objectFeature != null) ? objectFeature.getGmlID() : null;
    final Feature subjWF = subjectFeature.getFeature();
    subjWF.setProperty( propertyQName, objectID );
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

  public static void addChild( final Feature parentFE, final IRelationType rt, final String featureID )
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
      if( (rwString == null) || (rwString.length() == 0) || (hwString == null) || (hwString.length() == 0) )
        return GeometryFactory.createGM_Point( 0, 0, crs );

      final double rw = Double.parseDouble( rwString );
      final double hw = Double.parseDouble( hwString );

      return GeometryFactory.createGM_Point( rw, hw, crs );
    }

    if( typeHandler != null )
      try
      {
        return typeHandler.parseType( input[0] );
      }
      catch( final ParseException e )
      {
        e.printStackTrace();
      }

    return null;
  }

  /**
   * Retrieves a property as a feature.
   * <p>
   * If the property is not yet set, the feature is generated and set.
   * </p>
   * <p>
   * This method creates directly a feature of the target feature type of the given property.
   * </p>
   * 
   * @throws IllegalArgumentException
   *             If the target feature type of the given property is abstract.
   */
  public static Feature getSubFeature( final Feature parent, final QName propertyName )
  {
    final Object value = parent.getProperty( propertyName );
    if( value instanceof Feature )
      return (Feature) value;

    if( value instanceof String )
      return parent.getWorkspace().getFeature( (String) value );

    final IFeatureType parentType = parent.getFeatureType();
    final IPropertyType property = parentType.getProperty( propertyName );
    if( !(property instanceof IRelationType) )
      throw new IllegalArgumentException( "Property is no relation: " + propertyName );

    final IRelationType rt = (IRelationType) property;
    final IFeatureType targetFeatureType = rt.getTargetFeatureType();

    if( targetFeatureType.isAbstract() )
      throw new IllegalArgumentException( "Cannot instantiate an abstract feature" );

    // neues machen
    final GMLWorkspace workspace = parent.getWorkspace();
    final Feature newSubFeature = workspace.createFeature( parent, rt, targetFeatureType );
    parent.setProperty( propertyName, newSubFeature );
    return newSubFeature;
  }

  /**
   * @see #addProperty(Feature, IPropertyType, Object)
   */
  public static void addProperty( final Feature feature, final QName propertyName, final Object value )
  {
    final IPropertyType property = feature.getFeatureType().getProperty( propertyName );
    FeatureHelper.addProperty( feature, property, value );
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

  @SuppressWarnings("unchecked")
  public static final <T> T getFeature( final GMLWorkspace workspace, final Object linkOrFeature, final Class<T> targetAdapterClass )
  {
    if( (workspace == null) || (linkOrFeature == null) || (targetAdapterClass == null) )
    {
      final String message = null;
      throw new IllegalArgumentException( message );
    }
    final Feature feature = FeatureHelper.getFeature( workspace, linkOrFeature );
    if( feature == null )
      return null;
    else
      return (T) feature.getAdapter( targetAdapterClass );
  }

  /**
   * Returns the label of a feature.
   * <p>
   * The label is taken from the annotation on the feature type
   * </p>
   * <p>
   * Additionally, token replace will be performed on the annotation string.
   * </p>
   * <p>
   * The following tokens are supported:
   * <ul>
   * <li>${id}: the gml:id of the feature</li>
   * <li>${property:_qname_}: the value of the property _qname_ parsed as string (via its marshalling handler). _qname_
   * <li>${listProperty:_qname_;listindex}: Similar to ${property}, but the value is interpretated as List, and then
   * the list item with index listindex is returned. Syntax: namespace#localPart</li>
   * </ul>
   * </p>
   */
  public static String getAnnotationValue( final Feature feature, final String annotationKey )
  {
    final IFeatureType featureType = feature.getFeatureType();
    final IAnnotation annotation = AnnotationUtilities.getAnnotation( featureType );
    // this can happen when we import a shape-file.
    if( annotation == null )
      return "no-label";

    final String label = annotation.getValue( annotationKey );
    return FeatureHelper.tokenReplace( feature, label );
  }

  public static boolean hasReplaceTokens( final IFeatureType featureType, final String annotationKey )
  {
    final IAnnotation annotation = AnnotationUtilities.getAnnotation( featureType );
    if( annotation == null )
      return false;

    final String label = annotation.getValue( annotationKey );
    return label.contains( TokenReplacerEngine.TOKEN_START );
  }

  /** Performs the token replace for the methods {@link #getLabel(Feature)}, ... */
  public static String tokenReplace( final Feature feature, final String tokenString )
  {
    return FeatureHelper.FEATURE_TOKEN_REPLACE.replaceTokens( feature, tokenString );
  }

  /**
   * This function creates a feature list of the given rootFeature.
   */
  public static HashMap<QName, ArrayList<Feature>> sortType( final Feature rootFeature, final QName propertyQName )
  {
    /* Get a list of all features in the given property. */
    final FeatureList list = (FeatureList) rootFeature.getProperty( propertyQName );

    /* Create a map QName->Features. */
    final HashMap<QName, ArrayList<Feature>> featureMap = new HashMap<QName, ArrayList<Feature>>();

    for( final Object o : list )
    {
      /* Get the feature. */
      final Feature feature = (Feature) o;

      /* Get the qname of the feature. */
      final QName qname = feature.getFeatureType().getQName();

      /* Wenn der QName bereits in der Liste existiert, hänge das Feature an dessen Liste. */
      if( featureMap.containsKey( qname ) )
      {
        final ArrayList<Feature> sub_list = featureMap.get( qname );
        sub_list.add( feature );
        featureMap.put( qname, sub_list );
      }
      else
      {
        /* Add the qname as a new key, with a new List. */
        final ArrayList<Feature> sub_list = new ArrayList<Feature>();
        sub_list.add( feature );
        featureMap.put( qname, sub_list );
      }
    }

    return featureMap;
  }

  public static Object createLinkToID( final String id, final Feature parentFeature, final IRelationType parentRelation, final IFeatureType ft )
  {
    if( id == null )
      return null;

    if( id.startsWith( "#" ) )
      return id;

    return new XLinkedFeature_Impl( parentFeature, parentRelation, ft, id, "", "", "", "", "" );
  }

  /**
   * @author thuel2
   * @return <code>true</code> if <code>parent</code> is one of the ancestors of or equals
   *         <em>ALL</em> <code>children</code>
   */
  public static boolean isParentOfAllOrEquals( final Feature parent, final Feature[] children )
  {
    if( children.length < 1 )
      return false;
    boolean isParentOfAllOrEquals = true;
    for( final Feature child : children )
      isParentOfAllOrEquals = isParentOfAllOrEquals && FeatureHelper.isParentOrEquals( parent, child );
    return isParentOfAllOrEquals;
  }

  /**
   * @author thuel2
   * @return <code>true</code> if <code>parent</code> is one of the ancestors of <code>child</code> or equals
   *         <code>child</code>
   */
  public static boolean isParentOrEquals( final Feature parent, final Feature child )
  {
    if( (parent == null) || (child == null) )
      return false;
    if( parent.equals( child ) )
      return true;
    else
      return FeatureHelper.isParent( parent, child );
  }

  /**
   * @author thuel2
   * @return <code>true</code> if <code>parent</code> is one of the ancestors of <em>ALL</em> <code>children</code>
   */
  public static boolean isParentOfAll( final Feature parent, final Feature[] children )
  {
    if( children.length < 1 )
      return false;
    boolean isParentOffAll = true;
    for( final Feature child : children )
      isParentOffAll = isParentOffAll && FeatureHelper.isParent( parent, child );
    return isParentOffAll;
  }

  /**
   * @author thuel2
   * @return <code>true</code> if <code>parent</code> is one of the ancestors of <code>child</code> (in relation
   *         to <code>workspace</code>)
   */
  public static boolean isParent( final GMLWorkspace workspace, final Object parent, final Object child )
  {
    Feature parentFeat = null;

    if( parent instanceof Feature )
      parentFeat = (Feature) parent;
    else
      parentFeat = workspace.getFeature( (String) parent );

    Feature childFeat = null;
    if( child instanceof Feature )
      childFeat = (Feature) child;
    else
      childFeat = workspace.getFeature( (String) child );

    return FeatureHelper.isParent( parentFeat, childFeat );
  }

  /**
   * @author thuel2
   * @return <code>true</code> if <code>parent</code> is one of the ancestors of <code>child</code>
   */
  public static boolean isParent( final Feature parent, final Feature child )
  {
    if( (parent == null) || (child == null) )
      return false;
    else if( child.getParentRelation() != null )
    {
      final Feature childParent = child.getParent();
      final Feature childRoot = child.getWorkspace().getRootFeature();
      if( parent.equals( childParent ) )
        return true;
      else if( childParent.equals( childRoot ) )
        return false;
      else
        return FeatureHelper.isParent( parent, childParent );
    }
    else
      return false;
  }

  /**
   * Calculates the minimal envelope containing all envelopes of the given features.
   * 
   * @return <code>null</code> if none of the given features contains a valid envelope.
   */
  public static GM_Envelope getEnvelope( final Feature[] features )
  {
    GM_Envelope result = null;

    for( final Feature feature : features )
    {
      final GM_Envelope envelope = feature.getEnvelope();
      if( envelope != null )
        if( result == null )
          result = envelope;
        else
          result = result.getMerged( envelope );
    }

    return result;
  }

  public static Feature createFeatureForListProp( final FeatureList list, final QName newFeatureName, final int index ) throws GMLSchemaException
  {
    final Feature parentFeature = list.getParentFeature();
    final GMLWorkspace workspace = parentFeature.getWorkspace();

    final IRelationType parentRelation = list.getParentFeatureTypeProperty();
    final IFeatureType targetFeatureType = parentRelation.getTargetFeatureType();

    final IFeatureType newFeatureType;
    if( newFeatureName == null )
    {
      newFeatureType = targetFeatureType;
    }
    else
    {
      newFeatureType = workspace.getGMLSchema().getFeatureType( newFeatureName );
    }

    if( newFeatureName != null && !GMLSchemaUtilities.substitutes( newFeatureType, targetFeatureType.getQName() ) )
    {
      throw new GMLSchemaException( "Type of new feature (" + newFeatureName + ") does not substitutes target feature type of the list: " + targetFeatureType.getQName() );
    }

    final Feature newFeature = workspace.createFeature( parentFeature, parentRelation, newFeatureType );
    try
    {
      workspace.addFeatureAsComposition( parentFeature, parentRelation, index, newFeature );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
    return newFeature;
  }

  public static final Feature createFeatureWithId( final QName newFeatureQName, final Feature parentFeature, final QName propQName, final String gmlID ) throws IllegalArgumentException
  {
    Assert.isNotNull( parentFeature, "parentFeature" );
    Assert.isNotNull( propQName, "propQName" );
    Assert.isNotNull( newFeatureQName, "newFeatureQName" );
    Assert.isNotNull( gmlID );

    final GMLWorkspace workspace = parentFeature.getWorkspace();
    final IGMLSchema schema = workspace.getGMLSchema();
    final IFeatureType featureType = schema.getFeatureType( newFeatureQName );
    final IPropertyType parentPT = parentFeature.getFeatureType().getProperty( propQName );
    if( !(parentPT instanceof IRelationType) )
    {
      throw new IllegalArgumentException( "Property not a IRelationType=" + parentPT + " propQname=" + propQName );
    }

    // TOASK does not include the feature into any workspace

    final Feature created = FeatureFactory.createFeature( parentFeature, (IRelationType) parentPT, gmlID, featureType, true );

    try
    {
      if( parentPT.isList() )
      {
        // workspace.addFeatureAsAggregation(
        // parentFeature,//srcFE,
        // (IRelationType)parentPT,//linkProperty,
        // -1,//pos,
        // gmlID//featureID
        // );

        // FeatureList propList=
        // (FeatureList)parentFeature.getProperty( parentPT );
        // propList.add( created );

        workspace.addFeatureAsComposition( parentFeature, (IRelationType) parentPT, -1, created );
      }
      else
      {
        // TODO test this case
        parentFeature.setProperty( parentPT, created );
      }
    }
    catch( final Exception e )
    {
      throw new RuntimeException( "Could not add to the workspace", e );
    }

    return created;
  }

  /**
   * Converts a list of {@link IFeatureWrapper2}s to a list of features.
   */
  public static final List<Feature> toFeatureList( final Collection< ? extends IFeatureWrapper2> c )
  {
    final List<Feature> fl = new ArrayList<Feature>();
    if( c != null )
    {
      Feature f;
      for( final IFeatureWrapper2 fw : c )
      {
        f = fw.getFeature();
        if( f == null )
        {
          throw new IllegalArgumentException( "All feature wrapper must wrapp a non null feature:" + c );
        }
        fl.add( f );
      }
    }
    return fl;
  }

}