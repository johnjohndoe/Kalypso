package org.deegree_impl.gml.schema;

import java.util.HashMap;

import org.deegree.model.feature.FeatureType;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;


/**
 *  This is a XML-schema convenient class 
 *  for generating GML-FeatureType elements from a XML schema
 *
 * @author doemming
 */
public class GMLSchema
{
    private Document m_schemaDoc = null;
    private HashMap m_ns = new HashMap(  );
    private String m_targetNS = null;
    private FeatureType[] m_featureTypes;

    public GMLSchema( Document schema ) throws Exception
    {
        m_schemaDoc = schema;
        setNameSpaces(  );
        m_featureTypes = GMLSchemaFactory.createFeatureTypes(this );
    }

    public String getDefaultNS(  )
    {
        return getNameSpace( "xmlns" );
    }

    public FeatureType[] getFeatureTypes(  )
    {
        return m_featureTypes;
    }

    public String getNameSpace( String xmlns )
    {
        return (String)m_ns.get( xmlns );
    }

    public Document getSchema(  )
    {
        return m_schemaDoc;
    }

    public String getTargetNS(  )
    {
        return m_targetNS;
    }

    private void setNameSpaces(  )
    {
        try
        {
            m_targetNS = XMLHelper.getAttributeValue( m_schemaDoc.getDocumentElement(  ), "targetNamespace" );
        }
        catch( Exception e )
        {
          e.printStackTrace();
         }

        Node node = m_schemaDoc.getDocumentElement(  );
        NamedNodeMap nodeMap = node.getAttributes(  );
 
        for( int i = 0; i < nodeMap.getLength(  ); i++ )
        {
            try
            {
                Node attribute = nodeMap.item( i );
                if( "http://www.w3.org/2000/xmlns/".equals( attribute.getNamespaceURI(  ) ) )
                {
                     m_ns.put( attribute.getLocalName(  ), attribute.getNodeValue(  ) );
                }
            }
            catch( Exception e )
            {
                // nothing
            }
        }
    }


}
