package org.kalypsodeegree_impl.gml.schema;

import java.util.Map;

import org.kalypsodeegree.model.feature.Annotation;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;

/**
 * FeatureTypeProperty that represents a property with a enumeration restriction
 * 
 * @author doemming
 */
public class EnumerationFeatureTypeProperty implements FeatureTypeProperty, Validator
{

  private final String m_name;

  private final String m_namespace;

  private final String m_type;

  private final Object[] m_enumeration;

  private final boolean m_isNullable;

  private final Map m_annotation;

  public EnumerationFeatureTypeProperty( String name, String namespace, String type,
      boolean isNullable, Object[] enumeration,Map annotation )
  {
    m_annotation=annotation;
    m_name = name;
    m_namespace = namespace;
    m_type = type;
    m_isNullable = isNullable;
    m_enumeration = enumeration;
  }

  public Object[] getEnumeration()
  {
    return m_enumeration;
  }

  public String getName()
  {
    return m_name;
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypsodeegree.model.feature.FeatureTypeProperty#isNullable()
   */
  public boolean isNullable()
  {
    return m_isNullable;
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypsodeegree.model.feature.FeatureTypeProperty#getType()
   */
  public String getType()
  {
    return m_type;
  }

  /*
   * (non-Javadoc)
   * 
   * @see de.tuhh.wb.jm.schema.Validator#isValid(java.lang.Object)
   */
  public boolean isValid( Object object )
  {
    if( object == null && !m_isNullable )
      return false;

    for( int i = 0; i < m_enumeration.length; i++ )
    {
      if( m_enumeration.equals( object.toString() ) )
        return true;
    }

    return false;
  }

  public String getNamespace()
  {
    return m_namespace;
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureTypeProperty#isGeometryProperty()
   */
  public boolean isGeometryProperty()
  {
    return false;
  }

  public Annotation getAnnotation( String lang )
  {
    return (Annotation)m_annotation.get(lang);
  }
 
  public Map getAnnotationMap()
  {
    return m_annotation;
  }
}