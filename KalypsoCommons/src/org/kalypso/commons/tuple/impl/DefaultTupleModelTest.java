/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.commons.tuple.impl;

import java.util.Date;
import java.util.Set;

import junit.framework.TestCase;

/**
 * @author schlienger
 */
public class DefaultTupleModelTest extends TestCase
{
  private DefaultTupleModel<DateKey, SimpleColumnKey> m_model;

  private DateKey m_row1;

  private DateKey m_row2;

  private SimpleColumnKey m_col1;

  private SimpleColumnKey m_col2;

  private SimpleColumnKey m_col3;

  @Override
  protected void setUp( ) throws Exception
  {
    m_model = new DefaultTupleModel<DateKey, SimpleColumnKey>();

    m_row1 = new DateKey( new Date() );
    m_row2 = new DateKey( new Date() );

    m_col1 = new SimpleColumnKey( "col1", Integer.class );
    m_col2 = new SimpleColumnKey( "col2", Double.class );
    m_col3 = new SimpleColumnKey( "col3", String.class );

    m_model.setValue( new Integer( 0 ), m_row1, m_col1 );
    m_model.setValue( new Double( 0 ), m_row1, m_col2 );
    m_model.setValue( "0", m_row1, m_col3 );

    m_model.setValue( new Integer( 1 ), m_row2, m_col1 );
    m_model.setValue( new Double( 1 ), m_row2, m_col2 );
    m_model.setValue( "1", m_row2, m_col3 );
  }

  /*
   * Test method for 'org.kalypso.tuple.DefaultTupleModel.getColumnCount()'
   */
  public void testGetColumnCount( )
  {
    assertEquals( 3, m_model.getColumnCount() );
  }

  /*
   * Test method for 'org.kalypso.tuple.DefaultTupleModel.getRowCount()'
   */
  public void testGetRowCount( )
  {
    assertEquals( 2, m_model.getRowCount() );
  }

  /*
   * Test method for 'org.kalypso.tuple.DefaultTupleModel.getColumnKeySet()'
   */
  public void testGetColumnKeySet( )
  {
    Set<SimpleColumnKey> set = m_model.getColumnKeySet();
    assertTrue( set.contains( m_col1 ) );
    assertTrue( set.contains( m_col2 ) );
    assertTrue( set.contains( m_col3 ) );
  }

  /*
   * Test method for 'org.kalypso.tuple.DefaultTupleModel.getRowKeySet()'
   */
  public void testGetRowKeySet( )
  {
    Set<DateKey> set = m_model.getRowKeySet();
    assertTrue( set.contains( m_row1 ) );
    assertTrue( set.contains( m_row2 ) );
  }

  /*
   * Test method for 'org.kalypso.tuple.DefaultTupleModel.getValue(IKey, IColumnKey)'
   */
  public void testGetValue( )
  {
    assertEquals( new Integer( 0 ), m_model.getValue( m_row1, m_col1 ) );
    assertEquals( new Double( 0 ), m_model.getValue( m_row1, m_col2 ) );
    assertEquals( "0", m_model.getValue( m_row1, m_col3 ) );

    assertEquals( new Integer( 1 ), m_model.getValue( m_row2, m_col1 ) );
    assertEquals( new Double( 1 ), m_model.getValue( m_row2, m_col2 ) );
    assertEquals( "1", m_model.getValue( m_row2, m_col3 ) );
  }

  /*
   * Test method for 'org.kalypso.tuple.DefaultTupleModel.setValue(Object, IKey, IColumnKey)'
   */
  public void testSetValue( )
  {
    // setting value to null
    m_model.setValue( null, m_row1, m_col1 );
    assertNull( m_model.getValue( m_row1, m_col1 ) );

    // setting value for new row and new col, not present yet
    SimpleColumnKey col4 = new SimpleColumnKey( "col4", Date.class );
    DateKey row3 = new DateKey( new Date() );
    Date d = new Date();
    m_model.setValue( d, row3, col4 );
    assertEquals( d, m_model.getValue( row3, col4 ) );

    // setting invalid value because of class incompatiblity
    try
    {
      m_model.setValue( d, m_row1, m_col1 );
      fail( "should detect incompatible classes" );
    }
    catch( IllegalArgumentException e )
    {
      // ok
    }
  }

  /*
   * Test method for 'org.kalypso.tuple.DefaultTupleModel.hasValue(IKey, IColumnKey)'
   */
  public void testHasValue( )
  {
    // value present
    assertTrue( m_model.hasValue( m_row1, m_col1 ) );

    // setting value to null
    m_model.setValue( null, m_row1, m_col1 );
    assertFalse( m_model.hasValue( m_row1, m_col1 ) );
  }

  /*
   * Test method for 'org.kalypso.tuple.DefaultTupleModel.removeValue(IKey, IColumnKey)'
   */
  public void testRemoveValue( )
  {
    // value present
    assertTrue( m_model.hasValue( m_row1, m_col1 ) );

    // setting value to null
    m_model.removeValue( m_row1, m_col1 );
    assertFalse( m_model.hasValue( m_row1, m_col1 ) );
  }

  /*
   * Test method for 'org.kalypso.tuple.DefaultTupleModel.deleteColumn(IColumnKey)'
   */
  public void testDeleteColumn( )
  {
    m_model.deleteColumn( m_col2 );

    assertEquals( 2, m_model.getColumnCount() );

    assertNull( m_model.getValue( m_row1, m_col2 ) );
  }

  /*
   * Test method for 'org.kalypso.tuple.DefaultTupleModel.deleteRow(IKey)'
   */
  public void testDeleteRow( )
  {
    m_model.deleteRow( m_row2 );
    
    assertEquals( 1, m_model.getRowCount() );
    assertNull( m_model.getValue( m_row2, m_col1 ) );
  }
}
