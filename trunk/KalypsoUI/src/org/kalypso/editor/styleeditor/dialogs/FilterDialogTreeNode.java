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

	public static final String NODE_TYPE = "node";

	public static final String TEXT_TYPE = "text";

	public FilterDialogTreeNode root = null;

	public ArrayList children = null;

	private String mTag = null;

	private String mType = null; // node or text

	public FilterDialogTreeNode(String string, boolean b) {
		mTag = string;
		createRoot();
	}

	public FilterDialogTreeNode(String string, String type) {
		mTag = string;
		mType = type;
	}

	public String getTag() {
		return mTag;
	}

	public void createRoot() {
		root = new FilterDialogTreeNode("<Html>", "node");
		children = new ArrayList();
		children.add(root);
	}

	public void addNode(FilterDialogTreeNode node) {
		if (children == null) {
			children = new ArrayList();
		}

		children.add(node);
	}

	public Object[] getChildren() {
		return getContents().toArray();
	}

	private ArrayList getContents() {
		if (children == null) {
			children = new ArrayList();
		}

		return children;
	}

	public String getType() {
		return mType;
	}

	public FilterDialogTreeNode getRoot() {
		return root;
	}
}

