package org.deegree_impl.gml.schema;

import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * mapping between xml-typenames and java-classnames for GML-geometry types and
 * XMLSCHEMA-simple types
 * 
 * @author doemming
 */
public class Mapper
{

  private static final SimpleDateFormat XML_DATE_FORMAT = new SimpleDateFormat(
      "yyyy-MM-dd'T'HH:mm:ss" );

  public static String mapGMLSchemaType2JavaType( String name )
  {
    if( "GeometryPropertyType".equals( name ) )
      return "org.deegree.model.geometry.GM_Object";

    if( "PointPropertyType".equals( name ) )
      return "org.deegree.model.geometry.GM_Point";

    if( "MultiPointPropertyType".equals( name ) )
      return "org.deegree.model.geometry.GM_MultiPoint";

    if( "PolygonPropertyType".equals( name ) )
      return "org.deegree.model.geometry.GM_Polygon";

    if( "MultiPolygonPropertyType".equals( name ) )
      return "org.deegree.model.geometry.GM_MultiSurface";

    if( "LineStringPropertyType".equals( name ) )
      return "org.deegree.model.geometry.GM_LineString";

    if( "MultiLineStringPropertyType".equals( name ) )
      return "org.deegree.model.geometry.GM_MultiCurve";

    if( "AbstractFeatureType".equals( name ) )
      return "org.deegree.model.feature.Feature";

    //    System.out.println( "add mapping for " + name + " in " +
    // Mapper.class.toString() );
    return null;
  }

  public static String mapXMLSchemaType2JavaType( String name ) throws Exception
  {
    if( "integer".equals( name ) )
      return "java.lang.Integer";

    if( "string".equals( name ) )
      return "java.lang.String";

    // TODO: also support date and time XML-Formats
    if( "dateTime".equals( name ) )
      return "java.util.Date";

    if( "boolean".equals( name ) )
      return "java.lang.Boolean";

    if( "float".equals( name ) )
      return "java.lang.Float";

    if( "double".equals( name ) )
      return "java.lang.Double";

    // TODO: map them all over registry

    throw new Exception( "unsupported Type:" + name );
  }

  public static String mapJavaValueToXml( Object value, String xmlType )
  {
    if( value instanceof Date )
      return XML_DATE_FORMAT.format( (Date)value );
    else if( value instanceof Number )
      // TODO: use a special (xml-conform) formatting?
      return value.toString();

    return value.toString();
  }

  /**
   * @param value
   * @return @throws
   *         Exception
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
      double intValue = Double.parseDouble( value );
      Integer integer = new Integer( (int)intValue );
      if( integer.intValue() != intValue )
        throw new Exception( "no valid int value :" + value );
      return integer;
    }
    if( "java.lang.Boolean".equals( type ) )
      return new Boolean( value );
    if( "java.util.Date".equals( type ) )
      return XML_DATE_FORMAT.parseObject( value );
    throw new Exception( "unknown XML type :" + type + "  for value: " + value );
  }
}