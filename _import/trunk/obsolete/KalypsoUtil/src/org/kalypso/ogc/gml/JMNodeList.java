/** TODO: license definieren
*/

package org.kalypso.ogc.gml;

import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import java.util.ArrayList;
import java.util.List;


/**
 * DOCUMENT ME!
 *
 * @author $author$
 */
public class JMNodeList implements NodeList
{
    private final List nodes = new ArrayList(  );

    public JMNodeList(  )
    {
      // nix
    }

    public JMNodeList( NodeList nl )
    {
        add( nl );
    }

    public int getLength(  )
    {
        return nodes.size(  );
    }

    public void add( NodeList nl )
    {
        for( int i = 0; i < nl.getLength(  ); i++ )
            nodes.add( nl.item( i ) );
    }

    public void add( Node node )
    {
        nodes.add( node );
    }

    public Node item( int pos )
    {
        return (Node)nodes.get( pos );
    }
}
