package com.bce.datacenter.kalypso;

import com.bce.datacenter.ingres.Database;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import java.util.Enumeration;
import java.util.List;
import java.util.Vector;
import javax.swing.tree.TreeNode;

/**
 * Channel
 *
 * @author schlienger
 */
public class Channel extends DataObject implements TreeNode {
	/** separate identifier used for user own needs */
	private String m_identifier;

	/** timeseries belonging to this channel */
	private List m_timeseries = null;
	private int m_typeRef;
	private int m_unitRef;
	private List m_tables = null;

	/**
	 * Constructor for existing object to build from database
	 *
	 * @param id internal object id
	 */
	public Channel(int id) {
		super(id);
	}

	/**
	 * Consructor with parameters
	 *
	 * @param id
	 * @param name
	 * @param desc
	 * @param identifier
	 * @param ownerRef
	 * @param typeRef
	 * @param unitRef
	 */
	public Channel(
		int id,
		String name,
		String desc,
		String identifier,
		int ownerRef,
		int typeRef,
		int unitRef) {
		super(id, name, desc);

		m_identifier = identifier;
		m_ownerRef = ownerRef;
		m_typeRef = typeRef;
		m_unitRef = unitRef;
	}

	/**
	 * Returns the list of tables that should receive external data from
	 * sensors.
	 *
	 * @return
	 */
	public List getTableNames() throws SQLException {
		if (m_tables == null) {
			String sql =
				"SELECT dataTableName FROM TS_TIMESERIES WHERE is_Recipient = 1 and channel_ref = "
					+ m_ID;
			ResultSet rs =
				Database.getConnection().createStatement().executeQuery(sql);

			m_tables = new Vector();

			while (rs.next())
				m_tables.add(rs.getString(1));

			rs.close();

			Database.commit();
		}

		return m_tables;
	}

	/**
	 * Returns the timeseries of that channel
	 *
	 * @return timeseries
	 */
	public List getTimeseries() {
		if (m_timeseries != null) {
			return m_timeseries;
		}

		m_timeseries = Timeserie.dbReadAll(m_ID);

		return m_timeseries;
	}

	public int getChildPos(Timeserie timeserie) {
		List list = getTimeseries();
		int result = -1;
		for (int i = 0; i < list.size(); i++)
			if (timeserie.equals(list.get(i)))
				result = i;
		return result;
	}
	/**
	 * Looks up the database for a channel according to the given identifier
	 *
	 * @param stmt
	 * @param identifier
	 * @return @throws
	 *         SQLException
	 */
	public static Channel findChannel(final String identifier)
		throws SQLException {
		String sql = "SELECT ID FROM TS_CHANNEL WHERE IDENTIFIER = ?";

		PreparedStatement stmt = Database.getConnection().prepareStatement(sql);

		stmt.setString(1, identifier);

		ResultSet rs = stmt.executeQuery();

		rs.next();

		Channel c = null;

		if (rs.getObject(1) != null) {
			c = new Channel(rs.getInt(1));
		}

		stmt.close();

		Database.commit();

		return c;
	}

	/**
	 * read from db to init members
	 */
	protected void dbRead() {
		try {
			PreparedStatement stmt =
				Database.getConnection().prepareStatement(
					"SELECT NAME, DESCRIPTION, IDENTIFIER, OWNER_REF, TYPE_REF, UNIT_REF FROM TS_CHANNEL WHERE ID = ?");

			stmt.setInt(1, m_ID);

			ResultSet rs = stmt.executeQuery();

			rs.next();

			m_name = rs.getString(1);
			m_description = rs.getString(2);
			m_identifier = rs.getString(3);
			m_ownerRef = rs.getInt(4);
			m_typeRef = rs.getInt(5);
			m_unitRef = rs.getInt(6);

			rs.close();
			stmt.close();

			Database.commit();
		} catch (SQLException e) {
			e.printStackTrace(System.out);

			Database.rollback();
		}
	}
	/* (non-Javadoc)
	 * @see javax.swing.tree.TreeNode#children()
	 */
	public Enumeration children() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.swing.tree.TreeNode#getAllowsChildren()
	 */
	public boolean getAllowsChildren() {
		return false;
	}

	/* (non-Javadoc)
	 * @see javax.swing.tree.TreeNode#getChildAt(int)
	 */
	public TreeNode getChildAt(int arg0) {

		return null;
	}

	/* (non-Javadoc)
	 * @see javax.swing.tree.TreeNode#getChildCount()
	 */
	public int getChildCount() {

		return 0;
	}

	/* (non-Javadoc)
	 * @see javax.swing.tree.TreeNode#getIndex(javax.swing.tree.TreeNode)
	 */
	public int getIndex(TreeNode arg0) {

		return 0;
	}

	/* (non-Javadoc)
	 * @see javax.swing.tree.TreeNode#getParent()
	 */
	public TreeNode getParent() {
		return (Level) getOwner();
	}

	/* (non-Javadoc)
	 * @see javax.swing.tree.TreeNode#isLeaf()
	 */
	public boolean isLeaf() {
		// TODO Auto-generated method stub
		return true;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object object) {
		if (object == null)
			return false;
		if (!(object instanceof Channel))
			return false;
		return ((Channel) object).getID() == getID();
	}

}
