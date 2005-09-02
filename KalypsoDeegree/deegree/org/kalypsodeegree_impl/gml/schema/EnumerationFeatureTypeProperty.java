package org.kalypsodeegree_impl.gml.schema;

import java.util.Map;

import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree_impl.model.feature.AbstractFeatureType;

/**
 * FeatureTypeProperty that represents a property with a enumeration restriction
 * 
 * @author doemming
 */
public class EnumerationFeatureTypeProperty extends AbstractFeatureType implements FeatureTypeProperty, Validator
{
  private final String m_type;

  private final Object[] m_enumeration;

  private final boolean m_isNullable;

  public EnumerationFeatureTypeProperty( String name, String namespace, String type, boolean isNullable,
      Object[] enumeration, Map annotation )
  {
    super( name, namespace, annotation );
    m_type = type;
    m_isNullable = isNullable;
    m_enumeration = enumeration;
  }

  public Object[] getEnumeration()
  {
    return m_enumeration;
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

}