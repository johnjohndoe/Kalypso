package datacenter.persistent;

import java.sql.*;


/**
 * Convenience database connection class
 *
 * @author Marc Schlienger
 */
public class Database
{
	private static String m_user;
	private static String m_password;

	private static String m_driver;
	private static String m_url;

	private static Connection m_connection = null;

	/**
	 * initialise this Database connection
	 *
	 * @param driver	driver name (example: ca.edbc.jdbc.EdbcDriver)
	 * @param url		url of the database (example: jdbc:edbc://PC070:ME7/BCE_PC070::dbflomatis/INGRES)
	 * @param user		user name for connection
	 * @param password	user password for connection
	 */
	public static void init(String driver, String url, String user, String password) throws Exception
	{
		m_driver = driver;
		m_url = url;
		m_user = user;
		m_password = password;

		if( m_connection == null )
		{
			Class.forName(m_driver).newInstance();
			m_connection = DriverManager.getConnection(m_url, m_user, m_password);
			m_connection.setAutoCommit(false);
		}
	}

	/**
	 * @return sql connection to database
	 */
	public static Connection getConnection()
	{
		return m_connection;
	}

	public static void commit()
	{
		if( m_connection != null)
		{
			try
			{
				m_connection.commit();

			}
			catch(SQLException e)
			{
				e.printStackTrace();
			}
		}
	}

	public static void rollback()
	{
		if( m_connection != null)
		{
			try
			{
				m_connection.rollback();
			}
			catch(SQLException e)
			{
				e.printStackTrace();
			}
		}
	}

	public static boolean tableExists(String tableName)
	{
		try
		{
			tableName = tableName.toUpperCase();

			Statement st = m_connection.createStatement();

			ResultSet set = st.executeQuery("SELECT COUNT(*) FROM IITABLES WHERE UPPERCASE(TABLE_NAME) = " + tableName);

			set.next();

			if( set.getInt(1) > 0)
				return true;
			else
				return false;
		}
		catch(SQLException e)
		{
			return false;
		}
	}
}
