package org.kalypso.repository.virtual.test;

import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;
import org.kalypso.repository.virtual.VirtualRepository;

import junit.framework.TestCase;

/**
 * VirtualRepositoryTest
 * 
 * @author schlienger
 */
public class VirtualRepositoryTest extends TestCase
{
  private VirtualRepository m_vr;

  protected void setUp( ) throws Exception
  {
    super.setUp();
    
    m_vr = new VirtualRepository( null, "F:/kalypso/data/conf/IObservationService/virtual-zml-repository.xml", true );
  }

  public void testGetChildren( ) throws RepositoryException
  {
    System.out.println( "Listing:" );
      
    IRepositoryItem[] items = m_vr.getChildren();
    
    for( int i = 0; i < items.length; i++ )
    {
      outputChildren( items[i], "|" );
    }
    
    System.out.println( "Listing finished." );
  }
  
  private void outputChildren( final IRepositoryItem item, final String tab ) throws RepositoryException
  {
    System.out.println( tab + item.getName() + " - " + item.getIdentifier() );
    
    if( item.hasChildren() )
    {
      final IRepositoryItem[] items = item.getChildren();
      for( int i = 0; i < items.length; i++ )
      {
        outputChildren( items[i], tab + "  " );
      }
    }
  }
}
