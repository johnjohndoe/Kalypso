package org.kalypso.repository.file.test;

import java.io.File;
import java.net.MalformedURLException;

import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;
import org.kalypso.repository.file.FileRepository;

import junit.framework.TestCase;

/**
 * @author schlienger
 */
public class FileRepositoryTest extends TestCase
{
  private FileRepository m_rep;
  private File m_userHome;

  protected void setUp() throws Exception
  {
    super.setUp();
    
    final String home = System.getProperty( "user.home" );
    m_userHome = new File( home );
    
    m_rep = new FileRepository( m_userHome.getParent(), true );
  }

  public void testFindItem() throws MalformedURLException, RepositoryException
  {
    final IRepositoryItem item = m_rep.findItem( m_userHome.toURL().toExternalForm() );
    
    assertNotNull( item );
    
    System.out.println( "Found item: " + item );
  }
  
  public void testGetChildren() throws RepositoryException
  {
    output( m_rep, "" );  
  }
  
  private void output( final IRepositoryItem item, final String space ) throws RepositoryException
  {
    System.out.println( item.getIdentifier() );
    
    final IRepositoryItem[] items = item.getChildren();
    for( int i = 0; i < items.length; i++ )
    {
      output( items[i], space + " " );
    }
  }
}
