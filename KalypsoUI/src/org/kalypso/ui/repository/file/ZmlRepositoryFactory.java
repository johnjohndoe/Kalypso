package org.kalypso.ui.repository.file;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.kalypso.java.io.filter.MultipleWildCardFileFilter;
import org.kalypso.ogc.sensor.zml.repository.ZmlObservationRepository;
import org.kalypso.repository.AbstractRepositoryFactory;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.RepositoryException;

/**
 * A GUI oriented ZmlRepository factory.
 * 
 * @author schlienger
 */
public class ZmlRepositoryFactory extends AbstractRepositoryFactory
{
  private String m_location = "";
  private String m_filters = "";

  /**
   * @see org.kalypso.repository.IRepositoryFactory#configureRepository()
   */
  public boolean configureRepository(  )
  {
    final Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
    //final DirectoryDialog fileDlg = new DirectoryDialog( shell, SWT.OPEN );

    //if( getConfiguration() != null )
    //  fileDlg.setFilterPath( getConfiguration() );

    //final String path = fileDlg.open();

    //if( path != null )
    //{
    //  MessageDialog.
//      setConfiguration( path );
//
//      return true;
//    }
//
//    return false;
    
    final ZmlRepositoryConfigDialog dlg = new ZmlRepositoryConfigDialog( shell, "", "" );
    if( dlg.open() == SWT.OK )
    {
      m_location = dlg.getLocation();
      m_filters = dlg.getFilters();
      
      return true;
    }
    
    return false;
  }

  /**
   * @see org.kalypso.repository.IRepositoryFactory#createRepository()
   */
  public IRepository createRepository()
  {
    final String[] exts = m_filters.split(",");
    final MultipleWildCardFileFilter filter = new MultipleWildCardFileFilter( exts, false, true, false );

    return new ZmlObservationRepository( m_location, isReadOnly(), filter );
  }
}