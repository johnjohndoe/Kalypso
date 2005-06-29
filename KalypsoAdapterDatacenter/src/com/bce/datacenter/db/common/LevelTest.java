package com.bce.datacenter.db.common;

import java.util.Iterator;
import java.util.List;

import com.bce.datacenter.AbstractTest;


/**
 * @author ingres
 */
public class LevelTest extends AbstractTest
{
  public void testGetRoot( ) 
  {
    Level root = Level.getRoot( ingres.getConnection() );

    assertNotNull( root );

    assertTrue( root.getChildCount() > 0 );

    List childs = root.getChildLevels();

    Iterator it = childs.iterator();

    while( it.hasNext() )
    {
      Level level = (Level) it.next();

      assertNotNull( level );

      System.out.println( level );
    }
  }
}