/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ui.views.map;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IStorage;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IViewSite;
import org.kalypso.ui.editor.mapeditor.AbstractMapPart;

/**
 * <p>
 * View on a {@link org.kalypso.ogc.gml.mapmodel.IMapModell}.
 * </p>
 * <p>
 * Shows a map of all themes. The sources of the themes can be edited.
 * </p>
 * 
 * @author Stefan Kurzbach
 * @author Gernot Belger
 */
public class MapView extends AbstractMapPart implements IViewPart
{
  public static final String ID = "org.kalypso.ui.views.mapView";

  private static final String SAVE_MAP_ON_CLOSE = "saveMapOnClose";

  private static final String RELOAD_MAP_ON_OPEN = "reloadMapOnOpen";

  private static final String MEMENTO_FILE = "mapFile";

  private String m_memento_file;

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPartControl( final Composite parent )
  {
    super.createPartControl( parent );

    // Stefan: Now we can restore the file if the map is configured to do so      
    final String reloadOnOpen = getConfigurationElement().getAttribute( RELOAD_MAP_ON_OPEN );
    if( m_memento_file!= null && "true".equals( reloadOnOpen ) )
    {
      final IFile file = ResourcesPlugin.getWorkspace().getRoot().getFile( new Path( m_memento_file ) );
      setFile( file );
      if( file != null )
      {
        startLoadJob( file );
      }
    }
  }

  public void init( final IViewSite site, final IMemento memento )
  {
    init( site );
    m_memento_file = memento.getString( MEMENTO_FILE );    
  }

  /**
   * @see org.eclipse.ui.part.ViewPart#saveState(org.eclipse.ui.IMemento)
   */
  public void saveState( final IMemento memento )
  {
    final IFile file = getFile();
    if( file != null )
    {
      final String string = file.getFullPath().toString();
      memento.putString( MEMENTO_FILE, string );
    }
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.AbstractMapPart#dispose()
   */
  @Override
  public void dispose( )
  {
    final String saveOnCloseString = getConfigurationElement().getAttribute( SAVE_MAP_ON_CLOSE );
    if( "true".equals( saveOnCloseString ) )
    {
      final IFile file = getFile();
      saveMap( file );
    }
    super.dispose();
  }

  private void saveMap( final IFile file )
  {
    try
    {
      saveMap( new NullProgressMonitor(), file );
    }
    catch( final CoreException e )
    {
      ErrorDialog.openError( getSite().getShell(), "Fehler", "Karte konnte nicht gespeichert werden.", e.getStatus() );
    }
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.AbstractMapPart#startLoadJob(org.eclipse.core.resources.IStorage)
   */
  @Override
  public void startLoadJob( final IStorage storage )
  {
    final IFile file = getFile();
    if( file != null && !file.equals( storage ) )
    {
      saveMap( file );
    }
    super.startLoadJob( storage );
  }
}