/** TODO: license definieren
*/

package org.kalypso.util.gml;

import org.w3c.dom.Node;


/**
 * DOCUMENT ME!
 *
 * @author $author$
 */
public class JMAttribute
{
    JMSchema mySchema = null;
    String myNS = null;
    String myName = null;
    String myValue = null;
    String myValueNS = null;

    public JMAttribute( JMSchema schema, Node node )
    {
        mySchema = schema;

        myNS = node.getNamespaceURI(  );
        myName = node.getLocalName(  ); //ref

        String value = node.getNodeValue(  );
        String[] strings = value.split( ":" );

        if( strings.length == 2 )
        {
            myValueNS = mySchema.getNameSpace( strings[0] );
            myValue = strings[1];
        }
        else
        {
            myValueNS = mySchema.getDefaultNS(  ); //myNS;
            myValue = value;
        }

        //	Debug.println(toString());
    }

    public String getNS(  )
    {
        return myNS;
    }

    public String getName(  )
    {
        return myName;
    }

    public String getValue(  )
    {
        return myValue;
    }

    public String getValueNS(  )
    {
        return myValueNS;
    }

    public String toString(  )
    {
        return "JMSchemaType @" + myNS + ":" + myName + "='" + myValueNS + ":" + myValue + "'";
    }
}
