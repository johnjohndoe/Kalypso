package org.kalypso.ogc.sensor.file;

import java.io.FileFilter;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.kalypso.java.io.MultipleWildCardFileFilter;
import org.kalypso.ogc.sensor.zml.repository.ZmlObservationRepository;
import org.kalypso.util.repository.IRepository;
import org.kalypso.util.repository.IRepositoryFactory;

/**
 * @author schlienger
 */
public class ObservationFileRepositoryFactory implements IRepositoryFactory
{
  private String m_location = null;
  
  /**
   * @see org.kalypso.util.repository.IRepositoryFactory#configureRepository(org.kalypso.util.repository.IRepository)
   */
  public boolean configureRepository( IRepository rep )
  {
    final Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
    final DirectoryDialog fileDlg = new DirectoryDialog( shell, SWT.OPEN );

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
    String[] ZML_FILES = {"*.zml"};
    FileFilter filter = new MultipleWildCardFileFilter( ZML_FILES, false, true, false );
    
    return new ZmlObservationRepository( m_location, filter );
  }
}
