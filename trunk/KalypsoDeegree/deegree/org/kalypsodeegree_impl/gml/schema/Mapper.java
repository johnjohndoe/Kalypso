package org.deegree_impl.gml.schema;

/**
 * mapping between xml-typenames and java-classnames for GML-geometry types and
 * XMLSCHEMA-simple types
 * 
 * @author doemming
 */
public class Mapper
{

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

//    System.out.println( "add mapping for " + name + " in " + Mapper.class.toString() );
    return null;
  }

  public static String mapXMLSchemaType2JavaType( String name ) throws Exception
  {
    if( "integer".equals( name ) )
      return "java.lang.Integer";

    if( "string".equals( name ) )
      return "java.lang.String";

    if( "date".equals( name ) )
      return "java.lang.Date";

    if( "boolean".equals( name ) )
      return "java.lang.Boolean";

    if( "float".equals( name ) )
      return "java.lang.Float";

    if( "double".equals( name ) )
      return "java.lang.Double";

    // TODO: map them all over registry

    //   if( "observation".equals( name ) )
    //    return "org.kalypso.ogc.sensor.ObservationSource";

    throw new Exception( "unsupported Type:" + name );
  }
}