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
package org.deegree.services.gazetteer;

/**
 * @author Axel Schaefer
 * @version
 */
public interface Node {

    /**
     * returns the ID of the node
     * @return
     */
    String getId();

    /**
     * returns the level of the tree, the node is in
     * @return
     */
    int getLevel();

    /**
     * returns the data of the node (only if it is a leaf)
     * @return
     */
    Object getData();
    
    void setData(Object data);    

    /**
     * returns the child-nodes, if available.
     * @return
     */
    Node[] getChildren();

	/**
	 * adds a child-node to the node.
	 * @param node
	 */
    void appendChild(Node node);
    
    Node removeChild(String id);
    
    void removeChild(Node node);
    
    /**
     * checks, if the node has child-notes
     * @return boolean. true, if node is a leaf.
     */
    boolean isLeaf();

}
