package com.bce.datacenter.kalypso;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.List;
import java.util.Vector;

import com.bce.datacenter.ingres.Database;
import com.bce.datacenter.ingres.Persistent;


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
	 * @param ownerRef owner id
	 * @return
	 */
    public static List loadChannels( int ownerRef )
    {
        Vector v = new Vector(  );

        try
        {
            PreparedStatement stmt = Database.getConnection(  )
                                             .prepareStatement( "SELECT ID, NAME, DESCRIPTION, IDENTIFIER, TYPE_REF, UNIT_REF FROM TS_CHANNEL WHERE OWNER_REF = ? ORDER BY NAME ASC" );

            stmt.setInt( 1, ownerRef );

            ResultSet set = stmt.executeQuery(  );

            while( set.next(  ) == true )
            {
                Channel c = new Channel( set.getInt( 1 ), set.getString( 2 ),
                        set.getString( 3 ), set.getString( 4 ), ownerRef,
                        set.getInt( 5 ), set.getInt( 6 ) );

                v.add( c );
            }

            set.close(  );
            stmt.close();
            
            Database.getConnection(  ).commit(  );
        }
        catch( Exception e )
        {
            e.printStackTrace( System.out );
        }

        return v;
    }

    /**
     * Creates an owner Object from the database according to its identifier
     *
     * @param id
     * @return
     */
    public static Persistent getOwner( int id )
    {
        return new Level( id );
    }
}
