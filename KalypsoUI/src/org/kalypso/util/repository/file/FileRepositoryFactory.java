package org.kalypso.util.repository.file;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.util.repository.IRepository;
import org.kalypso.util.repository.IRepositoryFactory;

/**
 * @author schlienger
 *
 */
public class FileRepositoryFactory implements IRepositoryFactory
{
  private String m_location = null;
  
  /**
   * @see org.kalypso.util.repository.IRepositoryFactory#configureRepository(org.eclipse.swt.widgets.Shell, org.kalypso.util.repository.IRepository)
   */
  public boolean configureRepository( final Shell shell, IRepository rep )
  {
    DirectoryDialog fileDlg = new DirectoryDialog( shell, SWT.OPEN );

    if( rep != null )
       fileDlg.setFilterPath( rep.getLocation() );
    
    String path = fileDlg.open();

    if( path != null )
    {
      m_location = path;
      
      return true;
    }
    
    return false;
  }

  /**
   * @see org.kalypso.util.repository.IRepositoryFactory#createRepository()
   */
  public IRepository createRepository()
  {
    // TODO: set the FileFilter here so that only ZML files are visible 
    return new FileRepository( m_location, null );
  }
}
