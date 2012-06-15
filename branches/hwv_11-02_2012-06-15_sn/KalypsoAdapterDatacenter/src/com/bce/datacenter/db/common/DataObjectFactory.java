package com.bce.datacenter.db.common;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.List;
import java.util.Vector;

import com.bce.datacenter.db.persistent.Persistent;
import com.bce.datacenter.db.timeseries.Channel;

/**
 * Factory for DataObject subclasses
 * 
 * @author schlienger
 */
public class DataObjectFactory
{
  /**
   * Loads all channels belonging to the given owner
   * 
   * @param con
   * @param ownerRef
   *          owner id
   * @return all channels belonging to the given owner
   */
  public static List<Channel> loadChannels( final Connection con, final int ownerRef )
  {
    final Vector<Channel> v = new Vector<Channel>();

    try
    {
      final PreparedStatement stmt = con.prepareStatement( "SELECT ID, NAME, DESCRIPTION, IDENTIFIER, TYPE_REF, UNIT_REF FROM TS_CHANNEL WHERE OWNER_REF = ? ORDER BY NAME ASC" );

      stmt.setInt( 1, ownerRef );

      final ResultSet set = stmt.executeQuery();

      while( set.next() == true )
      {
        final Channel c = new Channel( con, set.getInt( 1 ), set.getString( 2 ), set.getString( 3 ), set.getString( 4 ), ownerRef, set.getInt( 5 ), set.getInt( 6 ) );

        v.add( c );
      }

      set.close();
      stmt.close();

      con.commit();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    return v;
  }

  /**
   * Returns an owner Object from the database according to its identifier
   * 
   * @param con
   * @param id
   * @return an owner Object from the database according to its identifier
   */
  public static Persistent getOwner( final Connection con, final int id )
  {
    return new Level( con, id );
  }
}