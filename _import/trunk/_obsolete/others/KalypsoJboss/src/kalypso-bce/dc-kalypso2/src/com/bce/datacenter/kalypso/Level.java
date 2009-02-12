package com.bce.datacenter.kalypso;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;
import java.util.Vector;

import javax.swing.tree.TreeNode;

import com.bce.datacenter.ingres.Database;
import com.bce.datacenter.ingres.Persistent;

/**
 * A Level is an abstract element of the tree hierarchy of the datacenter. A Level
 * belongs to exactly one parent, except for the root that has no parent. Thus a given
 * Level may have zero, one or more child Levels.
 *
 * @author Marc Schlienger
 */
public class Level extends Persistent implements TreeNode {
	private static Level m_rootLevel = null;
	/** parent level */
	private Level m_parent = null;

	/** array of child levels, used as simple caching solution */
	private List m_childs = null;

	/** array of containers, used as simple caching solution */
	private List m_objects = null;

	/** some arbitrary description */
	private String m_description;

	/** name as shown in the tree */
	private String m_name;

	/** ref of parent level */
	private int m_parentRef;

	/**
	 * constructs a Level based on an identifier with the database
	 *
	 * @param id level identifier as existing in the database
	 */
	public Level(int id) {
		super(id, true);

	}

	public int getParentLevelID() {
		return m_parentRef;
	}
	/**
	 * Constructor with parameters
	 *
	 * @param id
	 * @param name
	 * @param desc
	 * @param parentRef
	 */
	public Level(int id, String name, String desc, int parentRef) {
		super(id, false);

		m_name = name;
		m_description = desc;
		m_parentRef = parentRef;
	}

	/**
	 * Returns the root level of the hierarchy
	 *
	 * @return the very root level
	 */
	public static Level getRoot() {
		if (m_rootLevel == null) {

			try {
				Statement st = Database.getConnection().createStatement();

				// the root has no parent
				ResultSet rs =
					st.executeQuery(
						"SELECT LVLID FROM DC_TREELEVEL WHERE PARENTLEVEL = 0");

				rs.next();

				int id = rs.getInt(1);

				Level level = null;

				if (rs.getObject(1) != null)
					level = new Level(rs.getInt(1));

				Database.commit();

				m_rootLevel = level;
			} catch (SQLException e) {
				e.printStackTrace(System.out);

				Database.rollback();
			}
		}
		return m_rootLevel;
	}

	/**
	 * @see javax.swing.tree.TreeNode#getAllowsChildren()
	 */
	public boolean getAllowsChildren() {
		return false;
	}

	/**
	 * @see javax.swing.tree.TreeNode#getChildAt(int)
	 */
	public TreeNode getChildAt(int childIndex) {
		return (TreeNode) getChildLevels().get(childIndex);
	}

	/**
	 * @see javax.swing.tree.TreeNode#getChildCount()
	 */
	public int getChildCount() {
		return getChildLevels().size();
	}

	/**
	 * Returns the child levels
	 *
	 * @return
	 */
	public List getChildLevels() {
		if (m_childs != null)
			return m_childs;

		try {
			m_childs = new Vector();

			PreparedStatement stmt =
				Database.getConnection().prepareStatement(
					"SELECT * FROM DC_TREELEVEL WHERE PARENTLEVEL = ? ORDER BY LEVELNAME ASC");
			stmt.setInt(1, m_ID);

			ResultSet rs = stmt.executeQuery();

			while (rs.next()) {
				Level l =
					new Level(
						rs.getInt(1),
						rs.getString(2),
						rs.getString(3),
						rs.getInt(4));

				m_childs.add(l);
			}

			rs.close();
			Database.commit();

			m_childs.addAll(getObjects());
		} catch (SQLException e) {
			e.printStackTrace();

			Database.rollback();
		}

		return m_childs;
	}

	/**
	 * Returns the description
	 *
	 * @return
	 */
	public String getDescription() {
		return m_description;
	}

	/**
	 * @see javax.swing.tree.TreeNode#getIndex(javax.swing.tree.TreeNode)
	 */
	public int getIndex(TreeNode node) {
		return getChildLevels().indexOf(node);
	}

	/**
	 * @see javax.swing.tree.TreeNode#isLeaf()
	 */
	public boolean isLeaf() {
		return getChildLevels().size() == 0;
	}

	/**
	 * Returns the name
	 *
	 * @return
	 */
	public String getName() {
		return m_name;
	}

	/**
	 * Returns the child objects
	 *
	 * @return
	 */
	public List getObjects() {
		if (m_objects != null)
			return m_objects;

		m_objects = DataObjectFactory.loadChannels(m_ID);

		return m_objects;
	}

	/**
	 * @see javax.swing.tree.TreeNode#getParent()
	 */
	public TreeNode getParent() {
		return getParentLevel();
	}

	/**
	 * Returns the parent
	 *
	 * @return
	 */
	public Level getParentLevel() {
		if (m_parentRef == 0)
			return null;
		else if (m_parent == null)
			m_parent = new Level(m_parentRef);

		return m_parent;
	}

	/**
	 * Returns the path (that is concatenation of path of parent level + this name)
	 *
	 * @return path of this level,
	 */
	public String getPathName() {
		Level parent = getParentLevel();

		if (parent != null)
			return parent.getPathName() + '/' + m_name;
		else
			return '/' + m_name;
	}

	/**
	 * @see javax.swing.tree.TreeNode#children()
	 */
	public Enumeration children() {
		return Collections.enumeration(getChildLevels());
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return m_name;
	}

	/**
	 * read from db and init members
	 */
	protected void dbRead() {
		try {
			PreparedStatement stmt =
				Database.getConnection().prepareStatement(
					"SELECT LEVELNAME, PARENTLEVEL, LEVELDESCRIPTION FROM DC_TREELEVEL WHERE LVLID = ?");

			stmt.setInt(1, m_ID);

			ResultSet set = stmt.executeQuery();

			set.next();

			m_name = set.getString(1);
			m_parentRef = set.getInt(2);
			m_description = set.getString(3);

			set.close();
			stmt.close();

			Database.commit();
		} catch (SQLException e) {
			e.printStackTrace();

			Database.rollback();
		}
	}
	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object object) {
		if (object == null)
			return false;
		if (!(object instanceof Level))
			return false;
		return ((Level) object).getID() == getID();
	}

}
