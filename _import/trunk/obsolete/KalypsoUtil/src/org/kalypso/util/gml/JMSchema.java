/** TODO: license definieren
*/

package org.kalypso.util.gml;

import java.util.HashMap;

import org.deegree.model.feature.FeatureType;
import org.kalypso.util.xml.XMLTools;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;


/**
 *  This is a XML-schema convenient class 
 *  for generating GML-FeatureType elements from a XML schema
 *
 * @author doemming@tuhh.de
 */
public class JMSchema
{
    private Document mySchema = null;
    private HashMap ns = new HashMap(  );
    private String myTargetNS = null;
    private FeatureType[] myFeatureTypes;

    public JMSchema( Document schema ) throws Exception
    {
        mySchema = schema;
        setNameSpaces(  );
        myFeatureTypes = JMSchemaFactory.createFeatureTypes(this );
    }

    public String getDefaultNS(  )
    {
        return getNameSpace( "xmlns" );
    }

    public FeatureType[] getFeatureTypes(  )
    {
        return myFeatureTypes;
    }

    public String getNameSpace( String xmlns )
    {
        return (String)ns.get( xmlns );
    }

    public Document getSchema(  )
    {
        return mySchema;
    }

    public String getTargetNS(  )
    {
        return myTargetNS;
    }

    private void setNameSpaces(  )
    {
        try
        {
            myTargetNS = XMLTools.getAttributeValue( mySchema.getDocumentElement(  ), "targetNamespace" );
        }
        catch( Exception e )
        {
          e.printStackTrace();
         }

        Node node = mySchema.getDocumentElement(  );
        NamedNodeMap nodeMap = node.getAttributes(  );
        System.out.println( "NamesSpaces: " );

        for( int i = 0; i < nodeMap.getLength(  ); i++ )
        {
            try
            {
                Node attribute = nodeMap.item( i );
                if( "http://www.w3.org/2000/xmlns/".equals( attribute.getNamespaceURI(  ) ) )
                {
                     ns.put( attribute.getLocalName(  ), attribute.getNodeValue(  ) );
                    System.out.println( attribute.getLocalName(  ) + " -> " + attribute.getNodeValue(  ) );
                }
            }
            catch( Exception e )
            {
                // nothing
            }
        }
    }


}
