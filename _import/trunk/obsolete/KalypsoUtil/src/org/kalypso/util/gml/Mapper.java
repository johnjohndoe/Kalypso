/*
 * Created on Jun 26, 2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package org.kalypso.util.gml;

public class Mapper
{

	public static String mapGMLSchemaType2JavaType( String name )
	{
	    if( "GeometryPropertyType".equals( name ) )
	        return "org.deegree.model.geometry.GM_Object";
	
	    if( "PointPropertyType".equals( name ) )
	        return "org.deegree.model.geometry.GM_Point";
	
	    if( "PolygonPropertyType".equals( name ) )
	        return "org.deegree.model.geometry.GM_Polygon";
	
	    if( "LineStringPropertyType".equals( name ) )
	        return "org.deegree.model.geometry.GM_LineString";
	
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
