package datacenter.zeitreihen;

import com.ca.openroad.COMException;
import com.ca.openroad.ParameterData;
import com.ca.openroad.RemoteServer;


/**
 * This class is used as a helper class to perform calls on the openroad application server
 * <br>
 * IMPORTANT NOTE: it doesn't work yet!!!
 *
 * @author Marc Schlienger
 */
public final class ORZeitreihenServer
{
	private static String m_akaName = null;
	private static RemoteServer m_rso = null;

	/**
	 * always call this method when you want to start using other methods from this class
	 */
	public static void init(String akaName)
	{
		m_akaName = akaName;
	}

	/**
	 * impemented using the singleton pattern.
	 * <br>
	 * gets a connection to the remoteserver
	 */
	protected static RemoteServer getServer()
	{
		if( m_rso == null )
		{
			try
			{
				m_rso = new RemoteServer();

				m_rso.connect(m_akaName, null, null);
			}
			catch(COMException e)
			{
				e.printStackTrace(System.out);
				m_rso = null;
			}
		}

		return m_rso;
	}


	/**
	 * imports timeseries from a file using the application server provided functionality.
	 * @param tableName	name of the timeseries table to import in
	 * @param sourceType	type of source we are dealing with
	 * @param sourceID	identifier
	 * @param unitid	unit ref
	 * @param fileName	filename where to import from
	 * @param fromDate	from date, can be null
	 * @param toDate	to date, can be null
	 * @param bReplace	specify whether or not to replace original timeseries if duplicates are found
	 * @param status	override status found in import file if specified, can be null
	 * @param separator	separator of items in import file, can be null (default: comma <,>)
	 * @return amount of rows imported
	 */
	public static int FastImport(
				String tableName,
				int sourceType,
				int sourceID,
				int unitid,
				String fileName,
				java.sql.Date fromDate,
				java.sql.Date toDate,
				boolean bReplace,
				String status,
				String separator)
	{
		int impCount = 0;
		ParameterData byValpdo = null;
		ParameterData byRefpdo = null;

		try
		{
			byValpdo = new ParameterData();
		    byRefpdo = new ParameterData();

	        byValpdo.declareAttr("tableName",   "STRING");
	        byValpdo.declareAttr("sourceType", "INTEGER");
	        byValpdo.declareAttr("sourceID", "INTEGER");
	        byValpdo.declareAttr("unitid", "INTEGER");
	        byValpdo.declareAttr("fileName", "STRING");
	        byValpdo.declareAttr("fromDate", "DATE");
	        byValpdo.declareAttr("toDate", "DATE");
	        byValpdo.declareAttr("bReplace", "INTEGER");
	        byValpdo.declareAttr("status", "STRING");
	        byValpdo.declareAttr("separator", "STRING");

	        byRefpdo.declareAttr("impCount",  "INTEGER");

			byValpdo.setString("tableName", tableName);
	        byValpdo.setInt("sourceType", sourceType);
	        byValpdo.setInt("sourceID", sourceID);
	        byValpdo.setInt("unitid", unitid);
	        byValpdo.setString("fileName", fileName);
	        byValpdo.setDate("fromDate", fromDate);
	        byValpdo.setDate("toDate", toDate);

	        if( bReplace == true )
		        byValpdo.setInt("bReplace", 1);
		    else
		    	byValpdo.setInt("bReplace", 0);

		    if( status == null )
	        	byValpdo.setString("status", "");
	        else
		        byValpdo.setString("status", status);

		    if( separator == null )
		    	byValpdo.setString("separator", "");
		    else
		    	byValpdo.setString("separator", separator);

	      	getServer().callProc("ImportFromFile", byValpdo, byRefpdo);

	      	impCount = byRefpdo.getInt("impCount");
        }
        catch(COMException e)
        {
        	e.printStackTrace(System.out);
        	impCount = -1;
        }
        finally
        {
            if (byValpdo != null)
                byValpdo.release();

            if (byRefpdo != null)
                byRefpdo.release();

            return impCount;
        }
	}

	/**
	 * exports timeseries to a file using the application server provided functionality.
	 * @param tableName	name of the timeseries table to export from
	 * @param sourceType	type of source we are dealing with
	 * @param sourceID	identifier
	 * @param unitid	unit ref
	 * @param fileName	filename where to export to
	 * @param fromDate	from date, can be null
	 * @param toDate	to date, can be null
	 * @param separator	separator of items in export file, can be null (default: comma <,>)
	 * @return amount of rows exported
	 */
	public static int FastExport(
			String tableName,
			int sourceType,
			int sourceID,
			int unitid,
			String fileName,
			java.sql.Date fromDate,
			java.sql.Date toDate,
			String separator )
	{
		int lineCount = 0;
		ParameterData byValpdo = null;
		ParameterData byRefpdo = null;

		try
		{
			byValpdo = new ParameterData();
		    byRefpdo = new ParameterData();

	        byValpdo.declareAttr("tableName",   "STRING");
	        byValpdo.declareAttr("sourceType", "INTEGER");
	        byValpdo.declareAttr("sourceID", "INTEGER");
	        byValpdo.declareAttr("unitid", "INTEGER");
	        byValpdo.declareAttr("fileName", "STRING");
	        byValpdo.declareAttr("fromDate", "DATE");
	        byValpdo.declareAttr("toDate", "DATE");
	        byValpdo.declareAttr("separator", "STRING");

	        byRefpdo.declareAttr("lineCount",  "INTEGER");

			byValpdo.setString("tableName", tableName);
	        byValpdo.setInt("sourceType", sourceType);
	        byValpdo.setInt("sourceID", sourceID);
	        byValpdo.setInt("unitid", unitid);
	        byValpdo.setString("fileName", fileName);
	        byValpdo.setDate("fromDate", fromDate);
	        byValpdo.setDate("toDate", toDate);

		    if( separator == null )
		    	byValpdo.setString("separator", "");
		    else
		    	byValpdo.setString("separator", separator);

	      	getServer().callProc("ExportToFile", byValpdo, byRefpdo);

	      	lineCount = byRefpdo.getInt("lineCount");
        }
        catch(COMException e)
        {
        	e.printStackTrace(System.out);

        	lineCount = -1;
        }
        finally
        {
            if (byValpdo != null)
                byValpdo.release();

            if (byRefpdo != null)
                byRefpdo.release();

            return lineCount;
        }
	}

	/**
	 * releases resources
	 */
	protected void finalize()
	{
		m_rso.release();
	}
}
