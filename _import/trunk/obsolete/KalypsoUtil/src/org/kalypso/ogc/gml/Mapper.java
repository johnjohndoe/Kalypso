/*
 * Created on Jun 26, 2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package org.kalypso.ogc.gml;

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

      System.out.println("add mapping for "+name+" in "+ Mapper.class.toString());
      return null;
	}

	public static String mapXMLSchemaType2JavaType( String name )
	    throws Exception
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
	
	    throw new Exception( "unsupported Type:" + name );
	}
	
	public static String mapCustomTypeToJavaType(String name)
	{
		if("rhb".equals(name))
		 return "org.kalypsoNA.Rhb";
		return null;
	}
}
