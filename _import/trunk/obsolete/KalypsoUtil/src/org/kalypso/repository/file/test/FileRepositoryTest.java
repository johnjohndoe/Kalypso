package org.kalypso.repository.file.test;

import java.io.File;

import junit.framework.TestCase;

import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;
import org.kalypso.repository.file.FileRepository;

/**
 * @author schlienger
 */
public class FileRepositoryTest extends TestCase
{
  private FileRepository m_rep;
  private File m_root;

  protected void setUp() throws Exception
  {
    super.setUp();
    
    // just something
    final String home = "C:/temp/deploy/export";
    m_root = new File( home );
    
    m_rep = new FileRepository( null, m_root.getParent(), "test", true );
  }

  public void testFindItem() throws RepositoryException
  {
    final IRepositoryItem item = m_rep.findItem( "test://export/logs.zip" );
    
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
