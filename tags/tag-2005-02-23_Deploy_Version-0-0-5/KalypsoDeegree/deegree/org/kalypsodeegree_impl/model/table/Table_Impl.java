/*--------------- Kalypso-Deegree-Header ------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

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

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 
 history:
  
 Files in this package are originally taken from deegree and modified here
 to fit in kalypso. As goals of kalypso differ from that one in deegree
 interface-compatibility to deegree is wanted but not retained always. 
     
 If you intend to use this software in other ways than in kalypso 
 (e.g. OGC-web services), you should consider the latest version of deegree,
 see http://www.deegree.org .

 all modifications are licensed as deegree, 
 original copyright:
 
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon GmbH
 http://www.lat-lon.de
 
---------------------------------------------------------------------------------------------------*/
package org.deegree_impl.model.table;

import java.util.ArrayList;
import java.util.HashMap;

import org.deegree.model.table.Table;
import org.deegree.model.table.TableException;

/**
 * 
 * 
 * <p>
 * -----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public class Table_Impl implements Table
{

  private String tableName = "";

  private ArrayList rows = null;

  private String[] columnNames = null;

  private String[] columnTypes = null;

  private HashMap columnNamesMap = new HashMap();

  /**
   * @param tableName
   * @param columnNames
   * @param columnTypes
   * @throws TableException
   */
  public Table_Impl( String tableName, String[] columnNames, String[] columnTypes )
      throws TableException
  {
    setTableName( tableName );

    if( columnTypes == null )
      throw new TableException( "Invalid column types. Column types = null" );

    for( int i = 0; i < columnTypes.length; i++ )
    {
      if( columnTypes[i].length() < 3 )
      {
        throw new TableException( "Invalid column type: " + columnTypes[i] );
      }
    }
    this.columnTypes = columnTypes;

    if( columnNames == null )
    {
      this.columnNames = new String[columnTypes.length];
      for( int i = 0; i < this.columnNames.length; i++ )
      {
        this.columnNames[i] = "";
      }
    }
    else
    {
      this.columnNames = new String[columnNames.length];
      for( int i = 0; i < columnNames.length; i++ )
      {
        this.columnNames[i] = columnNames[i].toUpperCase();
        columnNamesMap.put( this.columnNames[i], new Integer( i ) );
      }
    }

    if( columnTypes.length != this.columnNames.length )
    {
      throw new TableException( "column names and types are not of the " + "same length" );
    }

    rows = new ArrayList( 1000 );
  }

  /**
   * @param tableName
   * @param columnNames
   * @param columnTypes
   * @param data
   * @throws TableException
   */
  public Table_Impl( String tableName, String[] columnNames, String[] columnTypes, Object[][] data )
      throws TableException
  {
    this( tableName, columnNames, columnTypes );

    rows = new ArrayList( data.length );
    for( int i = 0; i < data.length; i++ )
    {
      appendRow( data[i] );
    }
  }

  /**
   * @param tableName
   * @param columnNames
   * @param columnTypes
   * @param initialCapacity
   * @throws TableException
   */
  public Table_Impl( String tableName, String[] columnNames, String[] columnTypes,
      int initialCapacity ) throws TableException
  {
    this( tableName, columnNames, columnTypes );
    rows.ensureCapacity( initialCapacity );
  }

  /**
   * returns the name of the table. If the table hasn't a name an empty string
   * ("") will be returned.
   */
  public String getTableName()
  {
    return tableName;
  }

  /**
   * @see org.deegree_impl.model.table.Table_Impl#getTableName()
   */
  public void setTableName( String tableName )
  {
    this.tableName = tableName;
  }

  /**
   * returns the value of the table field indexed by <tt>row</tt> and
   * <tt>col</tt>
   */
  public Object getValueAt( int row, int col )
  {
    ArrayList tmp = (ArrayList)rows.get( row );
    return tmp.get( col );
  }

  /**
   * set a value at the table field indexed by <tt>row</tt> and <tt>col</tt>
   */
  public void setValueAt( Object value, int row, int col )
  {
    ArrayList tmp = (ArrayList)rows.get( row );
    tmp.set( col, value );
  }

  /**
   * returns the data of the row'th row of the table
   */
  public Object[] getRow( int row )
  {
    ArrayList tmp = (ArrayList)rows.get( row );
    return tmp.toArray();
  }

  /**
   * sets the data of the row'th row
   */
  public void setRow( Object[] data, int row ) throws TableException
  {
    if( this.getColumnCount() != data.length )
    {
      throw new TableException( "submitted row doesn't have the same length"
          + " as the table has columns." );
    }
    ArrayList tmp = (ArrayList)rows.get( row );

    for( int i = 0; i < data.length; i++ )
    {
      tmp.set( i, data[i] );
    }
  }

  /**
   * appends a row to the table and sets its data
   */
  public void appendRow( Object[] data ) throws TableException
  {
    if( this.getColumnCount() != data.length )
    {
      throw new TableException( "submitted row doesn't have the same length"
          + " as the table has columns." );
    }
    ArrayList tmp = new ArrayList( data.length );
    for( int i = 0; i < data.length; i++ )
    {
      tmp.add( data[i] );
    }
    rows.add( tmp );
  }

  /**
   * returns the number rows of the table
   */
  public int getRowCount()
  {
    return rows.size();
  }

  /**
   * adds a new column to the table. for this a computional expensive operation
   * this method should be used with care.
   */
  public void addColumn( String name, String type )
  {
    String[] s1 = new String[columnNames.length + 1];
    String[] s2 = new String[columnNames.length + 1];
    for( int i = 0; i < columnNames.length; i++ )
    {
      s1[i] = columnNames[i];
      s2[i] = columnTypes[i];
    }
    s1[s1.length - 1] = name;
    s2[s2.length - 1] = type;
    columnNames = s1;
    columnTypes = s2;

    for( int i = 0; i < rows.size(); i++ )
    {
      ArrayList tmp = (ArrayList)rows.get( i );
      tmp.add( "" );
    }

  }

  /**
   * returns the number columns of the table
   */
  public int getColumnCount()
  {
    return columnNames.length;
  }

  /**
   * returns the names of all table columns. If a column hasn't a name a empty
   * String ("") will be returned.
   */
  public String[] getColumnNames()
  {
    return columnNames;
  }

  /**
   * returns the name of the specified column. If a column hasn't a name a empty
   * String ("") will be returned.
   */
  public String getColumnName( int col )
  {
    return columnNames[col];
  }

  /**
   * returns the names of all column types. For each column a type (name of a
   * java class) has to be defined.
   */
  public String[] getColumnTypes()
  {
    return columnTypes;
  }

  /**
   * returns the name of the type of the specifies column. For each column a
   * type (name of a java class) has to be defined.
   */
  public String getColumnType( int col )
  {
    return columnTypes[col];
  }

  /**
   * sets the type of a column.
   */
  public void setColumnType( int col, String type ) throws TableException
  {
    columnTypes[col] = type;
    try
    {
      Class cl = Class.forName( type );
      for( int i = 0; i < getRowCount(); i++ )
      {
        Object o = getValueAt( i, col );
        if( o != null )
        {
          if( !cl.isAssignableFrom( o.getClass() ) )
          {
            throw new TableException( "" );
          }
        }
      }
    }
    catch( Exception e )
    {
      throw new TableException( "column type can not be changed to " + type + " because: " + e );
    }
  }

  /**
   * sets the name of a column.
   */
  public void setColumnName( int col, String name )
  {
    columnNames[col] = name;
  }

  /**
   * removes a row from the table
   */
  public Object[] removeRow( int index )
  {
    ArrayList list = (ArrayList)rows.remove( index );
    return list.toArray();
  }

  /**
   * export the table as a instance of <tt>javax.swing.table.TableModel</tt>.
   * The instance will be created using the
   * <tt>javax.swing.table.DefaultTableModel</tt>
   */
  public javax.swing.table.TableModel exportAsJDKTableModel()
  {
    return null;
  }

  /**
   * returns the index of the submitted columns name. If no column with that
   * name if present -1 will be returned. the test is not case sensitive
   *  
   */
  public int getColumnIndex( String columnName )
  {
    Integer index = (Integer)columnNamesMap.get( columnName.toUpperCase() );
    return index.intValue();
  }

  public String toString()
  {
    StringBuffer sb = new StringBuffer( 100000 );
    for( int i = 0; i < getRowCount(); i++ )
    {
      sb.append( "row: " + i );
      for( int c = 0; c < getColumnCount(); c++ )
      {
        sb.append( getColumnName( c ) + ": " + getValueAt( i, c ) );
      }
    }
    return sb.toString();
  }
}