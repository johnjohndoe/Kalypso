package org.kalypso.psiadapter.repository;

import junit.framework.TestCase;

import org.kalypso.repository.IRepository;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;

/**
 * PSICompactRepositoryTest
 * 
 * @author sm9
 */
public class PSICompactRepositoryTest extends TestCase
{

  public void testGetChildren( ) throws RepositoryException, ClassNotFoundException
  {
    System.out.println( "Start listing for getChildren");
    
    IRepository rep = PSICompactRepositoryFactory.getRepository();
    
    IRepositoryItem[] items = rep.getChildren();
    
    for( int i = 0; i < items.length; i++ )
    {
      IRepositoryItem item = items[i];
      
      System.out.println( item );
    }
    
    System.out.println( "Stop listing for getChildren");
  }
}
