/*
 * Created on 04.08.2004
 *
 */
package org.kalypso.editor.styleeditor.dialogs;

import java.util.ArrayList;

/**
 * @author Administrator
 */
public class FilterDialogTreeNode {
	
	public static final int ROOT_TYPE= 0;
	public static final int LOGICAL_NODE_TYPE= 1;	
	public static final int COMPARISON_NODE_TYPE= 2;
	public static final int FEATUREID_NODE_TYPE= 3;
	
	// or PARAMETER_TYPE SUCH AS LITERAL, PROPERTY_NAME
	public static final int PARAMETER_TYPE = 4;

	public FilterDialogTreeNode parent = null;

	public ArrayList children = null;

	private String name = null;

	private int type = -1; 

	private FilterDialogTreeNode() {}
	
	public FilterDialogTreeNode(String string, int type) {		
		this.name = string;
		this.type = type;
		
		if(type == ROOT_TYPE)
			createRoot(string);		
	}


	public String getName()
	{
		return name;
	}

	private void createRoot(String name) {
		parent = new FilterDialogTreeNode();
		parent.name = name;
		parent.type = ROOT_TYPE;
		children = new ArrayList();
		children.add(parent);
	}

	public void addNode(FilterDialogTreeNode node) {
		if (children == null) {
			children = new ArrayList();
		}
		node.parent = this;
		children.add(node);
	}
	
	public void removeNode(FilterDialogTreeNode node)
	{
		if(children != null)
		{
			children.remove(node);
		}
	}
	public Object[] getChildren() {
		if (children == null) {
			children = new ArrayList();
		}
		return children.toArray();
	}

	public int getType() {
		return type;
	}

	public FilterDialogTreeNode getParent() {
		return parent;
	}
}

