
package org.deegree_impl.gml.schema;

import java.util.ArrayList;
import java.util.List;

import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * 
 * @author doemming
 */
public class NodeList_Impl implements NodeList
{
  private final List nodes = new ArrayList();

  public NodeList_Impl()
  {
  // nothing
  }

  public NodeList_Impl( NodeList nl )
  {
    add( nl );
  }

  public int getLength()
  {
    return nodes.size();
  }

  public void add( NodeList nl )
  {
    for( int i = 0; i < nl.getLength(); i++ )
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