package org.kalypso.ui.repository.file;

import java.io.FileFilter;

import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.kalypso.java.io.filter.AcceptAllFileFilter;
import org.kalypso.java.io.filter.MultipleWildCardFileFilter;
import org.kalypso.ogc.sensor.zml.repository.ZmlObservationRepository;
import org.kalypso.repository.AbstractRepositoryFactory;
import org.kalypso.repository.IRepository;

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
   
    final ZmlRepositoryConfigDialog dlg = new ZmlRepositoryConfigDialog( shell, "", "" );
    
    final int res = dlg.open();
    
    if( res == Window.OK )
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
    final FileFilter filter;
    
    if( m_filters.length() == 0 )
      filter = new AcceptAllFileFilter();
    else
    {
      final String[] exts = m_filters.split(",");
      filter = new MultipleWildCardFileFilter( exts, false, true, false );
    }

    return new ZmlObservationRepository( m_location, isReadOnly(), filter );
  }
}