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
package org.deegree_impl.io;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Types;

import org.deegree.tools.IDGenerator;

/**
 * Factory for <tt>IDGenerator</tt> -instances. The generated instance is
 * suitable for the database used and for the configuration (i.e. table and
 * field type).
 * <p>
 * 
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$ $Date$
 */
public class IDGeneratorFactory
{

  public static IDGenerator createIDGenerator( Connection con, String tableName, String fieldName )
      throws SQLException
  {

    // retrieve type of field
    Statement stmt = con.createStatement();
    ResultSet rs = stmt.executeQuery( "SELECT " + fieldName + " FROM " + tableName );
    ResultSetMetaData rsmd = rs.getMetaData();
    int type = rsmd.getColumnType( 1 );
    rs.close();
    stmt.close();
    boolean isNumeric = false;

    switch( type )
    {
    case Types.INTEGER:
    case Types.BIGINT:
    case Types.DECIMAL:
    case Types.NUMERIC:
    case Types.SMALLINT:
    case Types.TINYINT:
    {
      isNumeric = true;
      break;
    }
    case Types.CHAR:
    case Types.LONGVARCHAR:
    case Types.VARCHAR:
    {
      isNumeric = false;
      break;
    }
    default:
    {
      throw new SQLException( "Cannot create IDGenerator for table '" + tableName + "' and field '"
          + fieldName + "'. Only integer and alphanumeric " + "fields are supported" );
    }
    }

    // find suitable instance of IDGenerator
    //		DatabaseMetaData dbmd = con.getMetaData ();
    //		String dbName = dbmd.getDatabaseProductName();
    //		String dbVersion = dbmd.getDatabaseProductVersion();
    //		String driverName = dbmd.getDriverName();
    //		String driverVersion = dbmd.getDriverVersion();
    //		
    //		System.out.println ("dbName : " + dbName);
    //		System.out.println ("dbVersion : " + dbVersion);
    //		System.out.println ("driverName : " + driverName);
    //		System.out.println ("driverVersion: " + driverVersion);

    IDGenerator idGenerator = new GenericSQLIDGenerator( con, tableName, fieldName, type, isNumeric );
    return idGenerator;
  }
}