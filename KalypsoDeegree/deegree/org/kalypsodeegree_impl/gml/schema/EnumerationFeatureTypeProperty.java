package org.deegree_impl.gml.schema;


import org.deegree.model.feature.FeatureTypeProperty;


/**
 * FeatureTypeProperty that represents a property with a enumeration restriction
 * @author doemming
 */

public class EnumerationFeatureTypeProperty implements FeatureTypeProperty, Validator
{
    private String m_name;
    private String m_type;
    private Object[] m_enumeration;
    private boolean m_isNullable;

    public EnumerationFeatureTypeProperty( String name, String type, boolean isNullable, Object[] enumeration )
    {
        m_name = name;
        m_type = type;
        m_isNullable = isNullable;
        m_enumeration = enumeration;
    }

    public Object[] getEnumeration(  )
    {
        return m_enumeration;
    }

    
    public String getName(  )
    {
        return m_name;
    }

    /* (non-Javadoc)
     * @see org.deegree.model.feature.FeatureTypeProperty#isNullable()
     */
    public boolean isNullable(  )
    {
        return m_isNullable;
    }

    /* (non-Javadoc)
     * @see org.deegree.model.feature.FeatureTypeProperty#getType()
     */
    public String getType(  )
    {
        return m_type;
    }

    /* (non-Javadoc)
     * @see de.tuhh.wb.jm.schema.Validator#isValid(java.lang.Object)
     */
    public boolean isValid( Object object )
    {
        if( object == null && !m_isNullable )
            return false;

        for( int i = 0; i < m_enumeration.length; i++ )
        {
            if( m_enumeration.equals( object.toString(  ) ) )
                return true;
        }

        return false;
    }
}
