/*----------------    FILE HEADER  ------------------------------------------

 This file is part of deegree.
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon Fitzke/Fretter/Poth GbR
 http://www.lat-lon.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 Andreas Poth
 lat/lon Fitzke/Fretter/Poth GbR
 Meckenheimer Allee 176
 53115 Bonn
 Germany
 E-Mail: poth@lat-lon.de

 Jens Fitzke
 Department of Geography
 University of Bonn
 Meckenheimer Allee 166
 53115 Bonn
 Germany
 E-Mail: jens.fitzke@uni-bonn.de

 
 ---------------------------------------------------------------------------*/
package org.deegree_impl.services.gazetteer;

import org.deegree.services.gazetteer.Node;
import org.deegree.services.gazetteer.Tree;

/**
 * @author Axel Schaefer
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version
 */
public class GazetteerResponseTree implements Tree
{
  private org.deegree.services.gazetteer.Node root = null;

  /**
   * 
   * @param id
   */
  public GazetteerResponseTree( String id )
  {
    this.root = new Node_Impl( id, 0, null, null );
  }

  /**
   * sets the node at the given id with the given data
   * 
   * @param id
   *          the id of the node, which has to be set
   * @param data
   *          the data to be set at the specified node
   */
  public void setDataAtId( String id, Object data )
  {
    Node node = getNodeAtId( root, id );
    node.setData( data );
  }

  /**
   * returns the node at the specified ID.
   * 
   * @param id
   *          the id of the node
   * @return an instance of Node
   */
  public Node getNodeAtId( String id )
  {
    return getNodeAtId( root, id );
  }

  /**
   * returns the node at the specified ID
   *  
   */
  private Node getNodeAtId( Node node, String id )
  {
    Node nd = null;

    if( node.getId().equals( id ) )
    {
      nd = node;
    }
    else
    {
      // if node is not found, search in childnodes
      if( !node.isLeaf() )
      {
        Node[] nodes = node.getChildren();

        for( int i = 0; i < nodes.length; i++ )
        {
          nd = getNodeAtId( nodes[i], id );

          if( nd != null )
          {
            break;
          }
        }
      }
    }

    return nd;
  }

  /**
   * traverses the tree in preorder starting at the root
   */
  public void traversePreorder()
  {

    traversePreorder( root );
  }

  /**
   * traverses the tree in preorder starting at the passed node
   */
  public void traversePreorder( Node node )
  {

    Node[] nodes = node.getChildren();
    if( nodes != null )
    {
      for( int i = 0; i < nodes.length; i++ )
      {
        traversePreorder( nodes[i] );
      }
    }
  }

  /**
   * 
   * @param node
   */
  public void action( Node node )
  {

  }

  /**
   * 
   * 
   * @return
   */
  public Node getRoot()
  {
    return root;
  }
}