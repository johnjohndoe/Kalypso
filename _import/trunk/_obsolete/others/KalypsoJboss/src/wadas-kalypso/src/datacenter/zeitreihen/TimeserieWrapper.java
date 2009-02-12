package datacenter.zeitreihen;



import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.sql.Date;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.text.SimpleDateFormat;
import java.util.StringTokenizer;
import java.util.TimeZone;

import com.braju.format.Format;
import com.braju.format.Parameters;

import datacenter.persistent.Database;



/**

 * wraps all kind of timeseries that can be owned by a channel or a computation.

 * Channels own work, original, and computed timeseries.

 *

 * NOTE: TimeserieWrapper are not stored as is into the database, they just represent some

 * common business logic upon timeseries.

 *

 * @author Marc Schlienger

 * @see Channel

 * @see Computation

 */

public class TimeserieWrapper

{

    private static SimpleDateFormat gmtDateFormat=null;

    static

    {

	gmtDateFormat=new SimpleDateFormat("yyyy_MM_dd HH:mm");

	gmtDateFormat.setTimeZone(TimeZone.getTimeZone("GMT+0"));

    }

	/**

	 * name of the table into which this wrapper will write/read timeseries

	 */

	private String m_TableName;

	/**

	 * defines what kind of timeserie is being wrapped

	 */

	private String m_SourceType;

	/**

	 * identifier of the owner of the timeseries: can be a channel, or a computation

	 */

	private int m_SourceID;

	/**

	 * simple information provided for this wrapper

	 */

	private String m_Info;



	private PreparedStatement m_stmt = null;



	/**

	 * constructs a wrapper

	 *

	 * @param tableName	name of the table where timeseries are stored

	 * @param sourceType

	 * @param sourceID

	 * @param info

	 * @param timefrom

	 * @param timeto

	 */

	public TimeserieWrapper(	String tableName,

								String sourceType,

								int sourceID,

								String info )

	{

		m_TableName = tableName;

		m_SourceType = sourceType;	/* timeseries type: work, original, aggregation, computation */

		m_SourceID = sourceID;	/* identifier of the owner (channelid, aggregationid, and so on) */



		m_Info = info;

	}





	/**

	 * returns the mindate of the wrapped timeseries

	 *

	 * @return min date, or null if error occurs

	 */

	public Date GetRealBegin()

	{

	    Date d = null;



		try{



		Statement st = Database.getConnection().createStatement();



		ResultSet set = st.executeQuery("SELECT MIN(TSTIME) FROM " + m_TableName);



		set.next();



		d = set.getDate(1);



		}catch(SQLException e)

		{

			e.printStackTrace(System.out);

      		}

		

		Database.commit();



		return d;

	}



	/**

	 * returns the max date of the wrapped timeseries

	 *

	 * @return max date, or null if error occurs

	 */

	public Date GetRealEnd()

	{

	    Date d = null;



		try{



		Statement st = Database.getConnection().createStatement();



		ResultSet set = st.executeQuery("SELECT MAX(TSTIME) FROM " + m_TableName);



		set.next();



		d = set.getDate(1);



		}catch(SQLException e)

		{

			e.printStackTrace(System.out);

		}



		Database.commit();



		return d;

	}





	public String GetInfo()

	{

		return m_Info;

	}



	public int GetSourceID()

	{

		return m_SourceID;

	}



	public String GetTableName()

	{

		return m_TableName;

	}



	public String GetSourceType()

	{

		return m_SourceType;

	}





	/**

	 * imports timeseries from the given file into table owned by this wrapper

	 *

	 * @param filename		pathname of the file to import

	 * @param from			date from which to import (if null import from beginning)

	 * @param to			date to which to import (if null import until end)

	 * @param bReplace		boolean specifying how to handle duplicates (if true, existing values will be replaced by imported ones)

	 * @param status		status flag that will be used for the flag column (if null uses the imported one)

	 * @param separator	date, value, flag tokens separator (if null assumes it is the comma character)

	 * @param ddateFormatPattern	date format as specified in simpleDateFormat (if null uses the locale default)

	 * @return the amount of imported rows, if some error occurs it returns -1.

	 * @see SimpleDateFormat

	 */

	public int ImportFromFile(String filename, Date from, Date to, boolean bReplace,

							   String status, String separator, String dateFormatPattern)

	{

		try{



			// get the name of the timeseries table

			String tabname = GetTableName();



			// build temporary table for importing from file

			String str = "DECLARE GLOBAL TEMPORARY TABLE SESSION.TMP_GTS ";

			str += "(TSTIME DATE NOT NULL, VALUE FLOAT NOT NULL, FLAG VARCHAR(10) NOT NULL) ";

			str += " ON COMMIT PRESERVE ROWS WITH NORECOVERY";



			Statement st = Database.getConnection().createStatement();

			st.execute(str);

			st.close();



			try{



				// copy from file

				FileReader fileReader = new FileReader(filename);

				BufferedReader bufReader = new BufferedReader(fileReader);

				String s = bufReader.readLine();



				int lineNb = 0;



				// provide default comma separator if no specified

				if( separator == null )

					separator = ",";



				// provide date parsing functionality

				SimpleDateFormat sdf = null;



				if( dateFormatPattern != null )

					sdf = new SimpleDateFormat(dateFormatPattern);	// parameter defined

				else

					sdf = new SimpleDateFormat();	// default



				Date d = null;

				boolean doIt = false;

				boolean stop = false;



				// parse input file and extract timeseries

				while( (s != null) && (stop == false) )

				{

					lineNb++;



					StringTokenizer tok = new StringTokenizer(s, separator);



					// build insert statement

					str = "INSERT INTO SESSION.TMP_GTS VALUES(?,";



					// fetch datetime

					if( tok.hasMoreElements() )

					{

						d = new java.sql.Date(sdf.parse(tok.nextToken()).getTime());

					}

					else throw new Exception("File " + filename + " has an invalid format (no datetime information) at line " + lineNb);



					// fetch value

					if( tok.hasMoreElements() )

					{

						str += tok.nextToken() + ",'";

					}

					else throw new Exception("File " + filename + " has an invalid format (no value information) at line " + lineNb);



					// fetch flag

					if( tok.hasMoreElements() )

					{

						str += tok.nextToken();

					}

					else throw new Exception("File " + filename + " has an invalid format (no flag information) at line " + lineNb);



					// finish build of insert

					str += "')";



					// insert date only if necessary

					if( from != null )

					{

						if( d.compareTo(from) >= 0 )

						{

							if( to != null )

							{

								if( d.compareTo(to) <= 0 )

								{

									doIt = true;	// insert if >= from and <= to

								}

								else

								{

									stop = true;	// > to, so no need to go further

								}

							}

							else

							{

								doIt = true;	// insert if >= from (to is null)

							}

						}

					}

					else

					{

						if( to != null )

						{

							if( d.compareTo(to) <= 0 )

							{

								doIt = true;	// insert if <= to (from is null)

							}

							else

							{

								stop = true;	// > to, so no need to go further

							}

						}

						else

						{

							doIt = true;	// insert if from and to are null

						}

					}



					if( doIt )

					{

						// prepare statement

						m_stmt = Database.getConnection().prepareStatement(str);

						m_stmt.setDate(1, d);

						m_stmt.execute();

						m_stmt.close();



						doIt = false;

					}



					// go to next line

					s = bufReader.readLine();

				}



				bufReader.close();



			}

			catch(FileNotFoundException e)

			{

				e.printStackTrace(System.out);

				return -1;

			}

			catch(IOException e)

			{

				e.printStackTrace(System.out);

				return -1;

			}



			// check for duplicates

			if( bReplace == true )

			{

				/* user wants to replace existing timeseries if duplicates are found

				so let's remove from our current timeseries table

				*/



				str = "DELETE FROM " + tabname + " WHERE TSTIME IN (SELECT TSTIME FROM SESSION.TMP_GTS)";

			}

			else

			{

				/* user wants to keep original timeseries

				so let's remove from the import table

				*/

				str = "DELETE FROM SESSION.TMP_GTS WHERE TSTIME IN (SELECT TSTIME FROM " + tabname + ")";

			}



			// remove duplicates where apropriate

			st.execute(str);

			Database.getConnection().commit();



			// modify status if necessary

			if( status != null )

			{

				if( status.toUpperCase() != "X" )

				{

					str = "UPDATE SESSION.TMP_GTS SET FLAG = '" + status + "'";



					st.execute(str);

				}

			}



			// check size of set to import

			str = "SELECT COUNT(*) FROM SESSION.TMP_GTS";

			ResultSet set = st.executeQuery(str);



			set.next();



			int impCount = set.getInt(1);



			// no journaling on the timeseries table

			str = "SET NOJOURNALING ON " + tabname;

			st.execute(str);



			// insert values in table

			str = "INSERT INTO " + tabname + " SELECT * FROM SESSION.TMP_GTS";

			st.execute(str);



			// set journaling on table

			str = "SET JOURNALING ON " + tabname;

			st.execute(str);



			// we can now remove temp table

			str = "DROP TABLE SESSION.TMP_GTS";

			st.execute(str);



			Database.getConnection().commit();



			return impCount;



		}

		catch(SQLException e)

		{

			e.printStackTrace(System.out);

			return -1;

		}

		catch(Exception e)

		{

			e.printStackTrace(System.out);

			return -1;

		}

	}



	/**

	 * exports the timeserie owned by this wrapper to a file

	 *

	 * @param filename		pathname of the file to create

	 * @param from			date from which to export (if null export from beginning)

	 * @param to			date up to export (if null export until end)

	 * @param separator	string representing the separator between the tokens: date, value, and flag. (if null default is comma)

	 * @param dateFormatPattern	string such as in SimpleDateFormat representing the format of the date. (if null uses the locale default)

	 * @return amount of lines written if successfull, otherwise -1

	*/

	public int ExportToFile(String filename, Date from, Date to, String separator, String dateFormatPattern )

	{

		try

		{



		    int line = 0;



			Statement st = Database.getConnection().createStatement();



			// get the name of the timeseries table

			String tabname = GetTableName();



			/* prepare statement for extracting desired timeseries

			   and create a temp table with these timeseries

			*/

			String str = "SELECT TSTIME, VALUE, FLAG FROM " + tabname;



			String tmp_stmt = "";



			if(from != null)

			{

				/* the where clause contains just one from-to time range */



				// time-from

			    tmp_stmt = "( TSTIME >= '" + gmtDateFormat.format(from) + "'";

				

				// look if time-to is specified

				if( to == null )

				{

					// no time-to, so close the parenthesis and stop processing

					tmp_stmt += ")";

				}

				else

				{

					// add time-to specification

					tmp_stmt += " AND TSTIME <= '" + gmtDateFormat.format(to) + "')";

				}

			}

			else

			{

				if( to != null )

				{

					tmp_stmt = "TSTIME <= '" + gmtDateFormat.format(to) + "'";

				}

			}





			// look if some where clause has been created

			if( tmp_stmt != "" )

			{

				// add where clause

				str += " WHERE " + tmp_stmt;

			}



			ResultSet set = st.executeQuery(str);



			try{



				if(separator == null)

					separator = ",";



				FileWriter fw = new FileWriter(filename);



				SimpleDateFormat sdf = null;



				if( dateFormatPattern != null )

					sdf = new SimpleDateFormat(dateFormatPattern);

				else

					sdf = new SimpleDateFormat();



				while( set.next() )

				{

					Format.fprintf(		fw,

										"%s" + separator + "%s" + separator + "%s\n",

										new Parameters(sdf.format( set.getDate(1))).add(set.getString(2)).add(set.getString(3)) );



					line++;

				}



				fw.close();



			}

			catch(IOException e)

			{

				e.printStackTrace(System.out);

			}



			st.close();

			Database.getConnection().commit();



			return line;

		}

		catch(SQLException e)

		{

			e.printStackTrace(System.out);

			return -1;



		}

	}

}

