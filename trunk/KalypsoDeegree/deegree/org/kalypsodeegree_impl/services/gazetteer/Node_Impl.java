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

import java.util.ArrayList;

import org.deegree.services.gazetteer.*;

/**
 * @author Axel Schaefer
 * @version
 */
public class Node_Impl implements Node {

    private String id = "";
    private int level = 0;
    private Object data = null;
    private ArrayList nodes = null;

    /**
     * default constructor.
     *
     */
    public Node_Impl() {
        this.nodes = new ArrayList(1000);
    }

    /**
     * 
     * @param id
     * @param level
     * @param data
     * @param nodes
     */
    public Node_Impl(String id, int level, Object data, Node[] nodes) {
        this();
        setId(id);
        setLevel(level);
        setData(data);
        if (nodes != null && nodes.length > 0) {
            setNodes(nodes);
        }
    }

    /**
     * (non-Javadoc)
     * @see org.deegree.services.gazetteer.Node#getId()
     */
    public String getId() {
        return this.id;
    }

    /**
     * @see org.deegree_impl.services.gazetteer.Node_Impl#getId()
     * @param id
     */
    public void setId(String id) {
        this.id = id;
    }

    /**
     * (non-Javadoc)
     * @see org.deegree.services.gazetteer.Node#getLevel()
     */
    public int getLevel() {
        return this.level;
    }

    /**
     * @see org.deegree_impl.services.gazetteer.Node_Impl#getLevel()
     * @param level
     */
    public void setLevel(int level) {
        this.level = level;
    }

    /**
     * (non-Javadoc)
     * @see org.deegree.services.gazetteer.Node#getData()
     */
    public Object getData() {
        return this.data;
    }

    /**
     * @see org.deegree_impl.services.gazetteer.Node_Impl#getData()
     * @param data
     */
    public void setData(Object data) {
        this.data = data;
    }

    /**
     * returns true if the node is a leaf
     */
    public boolean isLeaf() {
        return nodes.size() == 0;
    }

    /**
     * returns all child nodes of this node
     */
    public Node[] getChildren() {
        return (Node[]) nodes.toArray(new Node[nodes.size()]);
    }

    /**
     * 
     * @param node
     */
    public void appendChild(Node node) {        
        this.nodes.add(node);
    }
    
    public Node removeChild(String id) {
        for (int i = 0; i < nodes.size(); i++) {
            if ( ((Node)nodes.get(i)).getId().equals( id ) ) {
                return (Node_Impl)nodes.remove(i);
            }
        }
        return null;
    }
    
    public void removeChild(Node node) {
        nodes.remove( node );
    }

    /**
     * 
     * @param nodes
     * @gee
     */
    public void setNodes(Node[] nodes) {
        this.nodes.clear();

        if (nodes != null) {
            for (int i = 0; i < nodes.length; i++) {
                this.nodes.add(nodes[i]);
            }
        }
    }

    
}
