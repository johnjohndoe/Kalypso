package org.kalypso.ogc.sensor.file;

import java.io.FileFilter;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.kalypso.java.io.filter.MultipleWildCardFileFilter;
import org.kalypso.ogc.sensor.zml.repository.ZmlObservationRepository;
import org.kalypso.repository.AbstractRepositoryFactory;
import org.kalypso.repository.IRepository;

/**
 * @author schlienger
 */
public class ObservationFileRepositoryFactory extends AbstractRepositoryFactory
{
  public ObservationFileRepositoryFactory( final String conf )
  {
    super( conf );
  }
  
  /**
   * @see org.kalypso.repository.IRepositoryFactory#configureRepository(org.kalypso.repository.IRepository)
   */
  public boolean configureRepository( final IRepository rep )
  {
    final Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
    final DirectoryDialog fileDlg = new DirectoryDialog( shell, SWT.OPEN );

    if( rep != null )
       fileDlg.setFilterPath( rep.getLocation() );
    else if( m_configuration != null )
      fileDlg.setFilterPath( m_configuration );
    
    final String path = fileDlg.open();

    if( path != null )
    {
      m_configuration = path;
      
      return true;
    }
    
    return false;
  }

  /**
   * @see org.kalypso.repository.IRepositoryFactory#createRepository()
   */
  public IRepository createRepository()
  {
    final FileFilter filter = new MultipleWildCardFileFilter( ZmlObservationRepository.ZML_FILES, false, true, false );
    
    return new ZmlObservationRepository( m_configuration, filter );
  }
}
