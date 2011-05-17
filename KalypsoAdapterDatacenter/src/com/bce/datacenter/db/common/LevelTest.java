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
    final Level root = Level.getRoot( ingres.getConnection() );

    assertNotNull( root );

    assertTrue( root.getChildCount() > 0 );

    final List<Level> childs = root.getChildLevels();

    final Iterator<Level> it = childs.iterator();

    while( it.hasNext() )
    {
      final Level level = it.next();

      assertNotNull( level );

      System.out.println( level );
    }
  }
}