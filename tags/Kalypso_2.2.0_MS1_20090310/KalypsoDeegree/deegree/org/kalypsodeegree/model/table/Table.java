/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always. 
 * 
 * If you intend to use this software in other ways than in kalypso 
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree, 
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree.model.table;

public interface Table
{

  /**
   * returns the name of the table. If the table hasn't a name an empty string ("") will be returned.
   */
  String getTableName();

  /**
   * @see Table#getTableName()
   */
  void setTableName( String tableName );

  /**
   * returns the value of the table field indexed by <tt>row</tt> and <tt>col</tt>
   */
  Object getValueAt( int row, int col );

  /**
   * set a value at the table field indexed by <tt>row</tt> and <tt>col</tt>
   */
  void setValueAt( Object value, int row, int col );

  /**
   * returns the data of the row'th row of the table
   */
  Object[] getRow( int row );

  /**
   * sets the data of the row'th row
   */
  void setRow( Object[] data, int row ) throws TableException;

  /**
   * appends a row to the table and sets its data
   */
  void appendRow( Object[] data ) throws TableException;

  /**
   * returns the number rows of the table
   */
  int getRowCount();

  /**
   * adds a new column to the table
   */
  void addColumn( String name, String type );

  /**
   * returns the number columns of the table
   */
  int getColumnCount();

  /**
   * returns the names of all table columns. If a column hasn't a name a empty String ("") will be returned.
   */
  String[] getColumnNames();

  /**
   * returns the name of the specified column. If a column hasn't a name a empty String ("") will be returned.
   */
  String getColumnName( int col );

  /**
   * returns the names of all column types. For each column a type (name of a java class) has to be defined.
   */
  String[] getColumnTypes();

  /**
   * returns the name of the type of the specifies column. For each column a type (name of a java class) has to be
   * defined.
   */
  String getColumnType( int col );

  /**
   * sets the type of a column. the implementing class have to ensure that this is a valid operation
   */
  void setColumnType( int col, String type ) throws TableException;

  /**
   * sets the name of a column.
   */
  void setColumnName( int col, String name );

  /**
   * removes a row from the table
   */
  Object[] removeRow( int index );

  /**
   * returns the index of the submitted columns name. If no column with that name if present -1 will be returned.
   */
  int getColumnIndex( String columnName );

}