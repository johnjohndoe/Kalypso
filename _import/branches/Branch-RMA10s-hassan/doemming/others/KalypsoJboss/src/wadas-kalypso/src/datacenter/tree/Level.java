package datacenter.tree;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Vector;

import datacenter.persistent.*;

/**
 * A Level is an abstract element of the tree hierarchy of the datacenter. A Level
 * belongs to exactly one parent, except for the root that has no parent. Thus a given
 * Level may have zero, one or more child Levels.
 *
 * NOTE: objects of this class can only be read from the database, it is not possible to create
 * new database objects of this type.
 *
 * @author Marc Schlienger
 * @see Container
 * @see Persistent
 */
public class Level extends Persistent
{
	/** name as shown in the tree
	 */
	private String m_Name;
	/** some arbitrary description
	 */
	private String m_Description;
	/** reference to parent level
	 */
	private int m_ParentLevelRef;
	/** array of child levels, used as simple caching solution
	 */
	private Level[] m_levels = null;
	/** array of containers, used as simple caching solution
	 */
	private Container[] m_objs = null;


	/**
	 * constructs a Level based on an identifier with the database
	 *
	 * @param id	level identifier as existing in the database
	 */
	public Level(int id)
	{
		super(id);
	}

	/**
	 * Gets the root level of the hierarchy
	 *
	 * NOTE: the root level should have a parentref of 0.
	 *
	 * @return the very root level
	 */
	public static Level GetRootLevel()
	{
		try
		{
			/* check if there's a root level */
			java.sql.Statement st = Database.getConnection().createStatement();

			ResultSet set = st.executeQuery("SELECT COUNT(*) FROM DC_TREELEVEL WHERE PARENTLEVEL = 0");

			set.next();

			int i = set.getInt(1);

			if( i == 0 )
			{
				return null;
			}

			set.close();

			Level theRoot = new Level(1);

			theRoot.PLoad();

			return theRoot;
		}
		catch(Exception e)
		{
			e.printStackTrace(System.out);

			return null;
		}
	}

	public String GetLevelName()
	{
		return m_Name;
	}

	public String toString()
	{
		return "Level: " + m_Name;
	}

	/**
	 * @return path of this level, that is concatenation of path of parent level + this level name
	 */
	public String GetPathName()
	{
		Level parent = GetParentLevel();

		if( parent == null )
			return parent.GetPathName() + '/' + m_Name;
		else
			return '/' + m_Name;
	}

	public String GetLevelDesc()
	{
		return m_Description;
	}

	/**
	 * Get the parent of that level
	 *
	 * @return parent Level, null if no parent (should only be the case for the root level).
	 */
	public Level GetParentLevel()
	{
		Level parent = null;

		if(m_ParentLevelRef != 0)
		{
			try{
			parent = new Level(m_ParentLevelRef);
			parent.PLoad();
			}catch(PersistentException e)
			{
				e.printStackTrace(System.out);
			}
		}

		return parent;
	}


	/**
	 * builds an array of the child levels
	 * @return array of child Levels
	 */
	public Level[] GetChildLevels()
	{
		if( m_levels != null )
			return m_levels;

		try
		{
			Vector v = new Vector();

			m_stmt = Database.getConnection().prepareStatement("SELECT LVLID FROM DC_TREELEVEL WHERE PARENTLEVEL = ? ORDER BY LEVELNAME ASC");
			m_stmt.setInt(1, m_ID);

			ResultSet set = m_stmt.executeQuery();

			while( set.next() )
			{
				Level l = new Level(set.getInt(1));
				l.PLoad();

				v.add(l);
			}

			set.close();
			Database.getConnection().commit();

			m_levels = (Level[])v.toArray(new Level[0]);
		}
		catch(Exception e)
		{
			e.printStackTrace(System.out);
		}

		return m_levels;
	}


	/**
	 * Returns an array of the objects in this level
	 * @return array of Container
	 */
	public Container[] GetObjects()
	{
		if( m_objs != null )
			return m_objs;

		try
		{
			Vector v = new Vector();

			m_stmt = Database.getConnection().prepareStatement("SELECT COID FROM DC_CONTAINEROBJECT WHERE LVLID = ? ORDER BY OBJNAME ASC");
			m_stmt.setInt(1, m_ID);
			ResultSet set = m_stmt.executeQuery();

			while( set.next() == true )
			{
				Container c = new Container(set.getInt(1));
				c.PLoad();

				v.add(c);
			}

			set.close();
			Database.getConnection().commit();

			m_objs = (Container[])v.toArray(new Container[0]);
		}
		catch(Exception e)
		{
			e.printStackTrace(System.out);
		}

		return m_objs;
	}


	/**
	 * read from db and init members
	 */
	protected void DBRead()
	{
		try
		{
			m_stmt = Database.getConnection().prepareStatement("SELECT LEVELNAME, PARENTLEVEL, LEVELDESCRIPTION FROM DC_TREELEVEL WHERE LVLID = ?");

			m_stmt.setInt(1, m_ID);

			ResultSet set = m_stmt.executeQuery();

			set.next();

			m_Name = set.getString(1);
			m_ParentLevelRef = set.getInt(2);
			m_Description = set.getString(3);

			set.close();
		}
		catch(Exception e)
		{
			e.printStackTrace(System.out);
		}

		super.DBRead();
	}
}
