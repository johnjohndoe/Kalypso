package org.kalypso.repository.conf.test;

import org.kalypso.java.xml.DomHelper.DomLoadException;
import org.kalypso.repository.conf.RepositoryConfigItem;

import junit.framework.TestCase;

/**
 * @author schlienger
 */
public class RepositoryConfigItemTest extends TestCase
{
  public void testSaveState() throws DomLoadException
  {
    final RepositoryConfigItem item = new RepositoryConfigItem( "foo", "bar", true );
    
    final String state = item.saveState();
    System.out.println( state );
    
    final RepositoryConfigItem item2 = RepositoryConfigItem.restore( state );
    
    assertNotNull( item2 );
    
    final String state2 = item2.saveState();
    System.out.println( state2 );
    
    assertEquals( state, state2 );
  }
}
