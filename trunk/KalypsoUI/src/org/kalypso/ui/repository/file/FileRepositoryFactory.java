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
import org.kalypso.ui.repository.dialogs.FileRepositoryConfigDialog;

/**
 * A GUI oriented File-<code>RepositoryFactory</code>. Please note that this factory
 * currently creates a <code>ZmlObservationRepository</code>. This could be changed
 * to some other subclass of <code>FileRepository</code> as long as the constructor
 * sticks to the arguments used here. To achieve more flexibility, this class could
 * be improved so that the concrete <code>FileRepository</code> class to instantiate
 * could be parametrised. 
 * 
 * @author schlienger
 */
public class FileRepositoryFactory extends AbstractRepositoryFactory
{
  private final static String SEPARATOR = "#";
  
  /**
   * @see org.kalypso.repository.IRepositoryFactory#configureRepository()
   */
  public boolean configureRepository(  )
  {
    final Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
   
    final FileRepositoryConfigDialog dlg = new FileRepositoryConfigDialog( shell, "", "", "" );
    
    final int res = dlg.open();
    
    boolean b = false;
    
    if( res == Window.OK )
    {
      // update configuration to stay consistent
      setConfiguration( dlg.getLocation() + SEPARATOR + dlg.getIdentifier() + SEPARATOR + dlg.getFilters() );
      
      b = true;
    }
    
    dlg.dispose();
    
    return b;
  }
  
  /**
   * @see org.kalypso.repository.IRepositoryFactory#createRepository()
   */
  public IRepository createRepository()
  {
    final FileFilter filter;
    
    final String[] splits = getConfiguration().split( SEPARATOR );
    
    if( splits.length == 2 || splits[2].length() == 0 )
      filter = new AcceptAllFileFilter();
    else
    {
      final String[] exts = splits[2].split(",");
      filter = new MultipleWildCardFileFilter( exts, false, true, false );
    }

    // could be improved: instead of directly instantiating we could use class loading...
    return new ZmlObservationRepository( this, splits[0], splits[1], isReadOnly(), filter );
  }
}