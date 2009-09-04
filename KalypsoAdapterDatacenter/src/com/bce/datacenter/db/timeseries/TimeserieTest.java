package com.bce.datacenter.db.timeseries;

import java.sql.SQLException;

import com.bce.datacenter.AbstractTest;
import com.bce.datacenter.db.common.Level;
import com.bce.datacenter.db.persistent.Persistent;

/**
 * @author ingres
 */
public class TimeserieTest extends AbstractTest
{
  public void testFindTimeserie( ) throws SQLException
  {
    Timeserie ts = Timeserie.findTimeserie( ingres.getConnection(), "Z_191_191" );

    assertNotNull( ts );

    System.out.println( "Timeserie: " + ts );

    Channel c = ts.getChannel();

    assertNotNull( c );

    System.out.println( "Channel: " + c );

    Persistent p = c.getOwner();
    assertNotNull( p );
    assertTrue( p instanceof Level );

    Level level = (Level) p;
    System.out.println( "Level: " + level + " owner: " + level.getParentRef() );

    Level parent = level.getParentLevel();
    assertNotNull( parent );
    System.out.println( "Parent Level: " + parent + " owner: "
        + parent.getParentRef() );
  }

  public void testExportToFile( ) throws SQLException
  {
    Timeserie ts = Timeserie.findTimeserie( ingres.getConnection(), "Z_767_784" );

    assertNotNull( ts );

    ts.ExportToFile( "C:/temp/zeitreihe.txt", ts.getRealBegin(), ts
        .getRealEnd(), "\t", "yyyy MM dd HH mm" );
  }
}