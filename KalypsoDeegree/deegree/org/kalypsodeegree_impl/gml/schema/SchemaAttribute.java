package org.deegree_impl.gml.schema;

import org.w3c.dom.Node;


/**
 * Helper class to handle schema attributes with values that are references
 *
 * @author doemming
 */
public class SchemaAttribute
{
    GMLSchema m_Schema = null;
    String m_NS = null;
    String m_Name = null;
    String m_Value = null;
    String m_ValueNS = null;

    public SchemaAttribute( GMLSchema schema, Node atributeNode )
    {
        m_Schema = schema;

        m_NS = atributeNode.getNamespaceURI(  );
        m_Name = atributeNode.getLocalName(  ); //ref

        String value = atributeNode.getNodeValue(  );
        String[] strings = value.split( ":" );

        if( strings.length == 2 )
        {
            m_ValueNS = m_Schema.getNameSpace( strings[0] );
            m_Value = strings[1];
        }
        else
        {
            m_ValueNS = m_Schema.getDefaultNS(  );
            m_Value = value;
        }
    }
 
    public String getNS(  )
    {
        return m_NS;
    }

    public String getName(  )
    {
        return m_Name;
    }

    public String getValue(  )
    {
        return m_Value;
    }

    public String getValueNS(  )
    {
        return m_ValueNS;
    }

    public String toString(  )
    {
        return getClass().toString() + m_NS + ":" + m_Name + "='" + m_ValueNS + ":" + m_Value + "'";
    }
}
