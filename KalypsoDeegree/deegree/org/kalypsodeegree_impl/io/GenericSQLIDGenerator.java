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
import java.sql.SQLException;
import java.sql.Statement;

import org.deegree.tools.IDGenerator;

/**
 * Primary key generator for generic JDBC-connections.
 * <p>
 * NOTE: At the moment, every application has to take care of locking the table
 *       to prevent problems in multithreaded or multihosted applications. 
 * <p>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider</a>
 * @version $Revision$ $Date$
 */
class GenericSQLIDGenerator implements IDGenerator {

	Connection con;
	int fieldType;
	boolean isNumeric;
	String tableName;
	String fieldName;

	/**
	 * Constructs a new GenericSQLIDGenerator.
	 * <p>
	 * @param con
	 * @param fieldType
	 */
	GenericSQLIDGenerator (Connection con, String tableName, String fieldName,
							int fieldType, boolean isNumeric) {
		this.con = con;
		this.fieldName = fieldName;
		this.tableName = tableName;		
		this.fieldType = fieldType;
		this.isNumeric = isNumeric;
	}

	/**
	 * Returns the successor to the given id (string). Valid characters in the
	 * id-string are: 0-9, a-z, A-Z. Every other character may result
	 * in an exception. 
	 * <p>
	 * @param lastId
	 * @return
	 */
	private String incrementId (String lastId) {
		char [] chars = lastId.toCharArray();
		for (int i = chars.length - 1; i >= 0; i--) {
			char c = chars [i];
			if (c >= '0' && c <= '9') {
				if (c == '9') {
					c = 'a';
				} else {
					c++;
				}
			} else if (c >= 'a' && c <= 'z') {
				if (c == 'z') {
					c = 'A';
				} else {
					c++;
				}
			} else if (c >= 'A' && c <= 'Z') {
				if (c == 'Z') {
					c = '0';
				} else {
					c++;
				}
			}
			chars [i] = c;
			if (c != '0') {
				break;
			}
		}
		String newId = new String (chars);
		if (chars [0] == '0') {
			newId = '1' + newId;
		}
		return newId;
	}

	/**
	 * Generates a new id, suitable as a primary key for the next dataset.
	 * <p>
	 * @return Id, the object type depends on the database field used as
	 *         primary key 
	 */
	public Object generateUniqueId () throws SQLException {

		Object id = null;

		// retrieve last id 
		Statement stmt = con.createStatement();
		ResultSet rs = stmt.executeQuery ("SELECT MAX(" + fieldName + ") FROM " + tableName);
		
		if (rs.next ()) {
			if (isNumeric) {
				id = new Integer (rs.getInt (1) + 1);
			} else {
				String oldId = rs.getString (1);
				if (oldId != null) {
					id = incrementId (oldId);
				} else {
					id = "0";
				}				
			}	
		} else {
			if (isNumeric) {
				id = new Integer (0);			
			} else {
				id = "0";				
			}
		}
		rs.close ();
		stmt.close ();		
		return id;
	}
}
