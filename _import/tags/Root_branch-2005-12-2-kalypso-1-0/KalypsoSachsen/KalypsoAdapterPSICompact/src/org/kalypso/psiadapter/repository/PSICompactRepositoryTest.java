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

  public void testGetChildren() throws RepositoryException
  {
    System.out.println( "Start listing for getChildren" );

    final IRepository rep = PSICompactRepositoryFactory.getRepository();

    recursiveGetChildren( rep.getChildren() );

    System.out.println( "Stop listing for getChildren" );
  }

  /**
   * Goes recursively through the repository
   * 
   * @param items
   * @throws RepositoryException
   */
  private void recursiveGetChildren( final IRepositoryItem[] items ) throws RepositoryException
  {
    for( int i = 0; i < items.length; i++ )
    {
      System.out.println( "Item: " + items[i].getName() + " ID: " + items[i].getIdentifier() );

      if( items[i].hasChildren() )
        recursiveGetChildren( items[i].getChildren() );
    }
  }
}
