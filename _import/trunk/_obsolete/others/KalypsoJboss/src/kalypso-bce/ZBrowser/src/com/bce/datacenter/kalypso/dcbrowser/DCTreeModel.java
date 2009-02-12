package com.bce.datacenter.kalypso.dcbrowser;

import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeNode;

import com.bce.datacenter.kalypso.Level;

/**
 * @author schlienger
 *
 */
public class DCTreeModel extends DefaultTreeModel
{
	/**
	 * Default constructor: builds the tree from the root Level
	 */
	public DCTreeModel()
	{
		this( Level.getRoot() );
	}
	
	/**
	 * @param root
	 */
	public DCTreeModel(TreeNode root)
	{
		super(root);
	}
}
