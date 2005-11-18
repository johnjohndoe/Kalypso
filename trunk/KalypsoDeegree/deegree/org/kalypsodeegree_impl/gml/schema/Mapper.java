package org.kalypsodeegree_impl.gml.schema;

import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * mapping between xml-typenames and java-classnames for GML-geometry types and XMLSCHEMA-simple types
 * 
 * @author doemming
 */
public class Mapper
{
  private static final SimpleDateFormat XML_DATETIME_FORMAT = new SimpleDateFormat( "yyyy-MM-dd'T'HH:mm:ss" );

  private static final SimpleDateFormat XML_DATE_FORMAT = new SimpleDateFormat( "yyyy-MM-dd" );

  public static String mapGMLSchemaType2JavaType( String name )
  {
    if( "AbstractFeatureType".equals( name ) )
      return "org.kalypsodeegree.model.feature.Feature";

    if( "FeatureAssociationType".equals( name ) )
      return "FeatureAssociationType";

    return null;
  }

  public static String mapXMLSchemaType2JavaType( String name ) throws Exception
  {
    if( "integer".equals( name ) )
      return "java.lang.Integer";

    if( "int".equals( name ) )
      return "java.lang.Integer";

    if( "string".equals( name ) )
      return "java.lang.String";

    // TODO: also support date and time XML-Formats
    if( "dateTime".equals( name ) )
      return "java.util.Date";

    if( "date".equals( name ) )
      return DateWithoutTime.class.getName();

    if( "boolean".equals( name ) )
      return "java.lang.Boolean";

    if( "float".equals( name ) )
      return "java.lang.Float";

    if( "double".equals( name ) )
      return "java.lang.Double";

    if( "long".equals( name ) )
      return "java.lang.Long";
    if( "decimal".equals( name ) )
      return "java.lang.Float";

    // TODO: map them all over registry

    throw new Exception( "unsupported Type:" + name );
  }

  public static String mapJavaValueToXml( final Object value )
  {
    if( value == null )
      return "";
    
    if( value instanceof DateWithoutTime )
      return XML_DATE_FORMAT.format( (DateWithoutTime)value );

    if( value instanceof Date )
      return XML_DATETIME_FORMAT.format( (Date)value );

    if( value instanceof Number )
      // TODO: use a special (xml-conform) formatting?
      return value.toString();

    return value.toString();
  }

  /**
   * @param value
   * @throws Exception
   */
  public static Object mapXMLValueToJava( String value, String type ) throws Exception
  {
    if( "java.lang.String".equals( type ) )
      return value;
    if( "java.lang.Float".equals( type ) )
      return new Float( value );
    if( "java.lang.Double".equals( type ) )
      return new Double( value );
    if( "java.lang.Integer".equals( type ) )
    {
      // shapefiles give string like "10.0"
      double doubleValue = Double.parseDouble( value );
      Integer integer = new Integer( (int)doubleValue );
      if( integer.intValue() != doubleValue )
        throw new Exception( "no valid int value :" + value );
      return integer;
    }
    if( "java.lang.Long".equals( type ) )
    {
      // shapefiles give strings like "10.0"
      double doubleValue = Double.parseDouble( value );
      Long longValue = new Long( (long)doubleValue );
      if( longValue.longValue() != doubleValue )
        throw new Exception( "no valid long value :" + value );
      return longValue;
    }
    if( "java.lang.Boolean".equals( type ) )
    {
      if( "true".equals( value ) || "1".equals( value ) )
        return new Boolean( true );
      return new Boolean( false );
    }
    if( "java.util.Date".equals( type ) )
      return XML_DATETIME_FORMAT.parseObject( value );

    if( DateWithoutTime.class.getName().equals( type ) )
      return XML_DATE_FORMAT.parseObject( value );

    throw new Exception( "unknown XML type: " + type + "  for value: " + value );
  }

  public static Object defaultValueforJavaType( final String type, final boolean createGeometry )
  {
    // TODO try to get default value from schema !
    if( "java.util.Date".equals( type ) )
      return new Date( 0 );
    if( "DateWithoutTime.class.getName()".equals( type ) )
      return new DateWithoutTime();
    if( "java.lang.Boolean".equals( type ) )
      return Boolean.FALSE;
    if( "java.lang.Float".equals( type ) )
      return new Float( 0 );
    if( "java.lang.Integer".equals( type ) )
      return new Integer( 0 );
    if( "java.lang.String".equals( type ) )
      return "";
    if( "java.lang.Double".equals( type ) )
      return new Double( 0.0 );
    if( "java.lang.Long".equals( type ) )
      return new Long( 0 );

    if( !createGeometry )
      return null;

    // TODO create default geo properties

    return null;
  }
}