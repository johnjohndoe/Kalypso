package com.bce.datacenter.db.timeseries;

import java.sql.SQLException;
import java.util.Iterator;
import java.util.List;

import com.bce.datacenter.AbstractTest;

/**
 * @author ingres
 */
public class ChannelTest extends AbstractTest
{
  public void testGetTableNames( ) throws SQLException
  {
    final Channel c = new Channel( ingres.getConnection(), 164 );

    final List<String> names = c.getTableNames();

    assertEquals( names.size(), 3 );

    final Iterator<String> it = names.iterator();

    final String firstName = it.next().toString();

    assertNotSame( firstName, "" );

    System.out.println( "TableName: " + firstName );

    final String secondName = it.next().toString();

    assertNotSame( secondName, "" );

    System.out.println( "TableName: " + secondName );
  }

  public void testFindChannel( ) throws SQLException
  {
    final Channel c = Channel.findChannel( ingres.getConnection(), "777" );

    assertNotNull( c );

    assertEquals( c.getID(), 164 );
  }
}