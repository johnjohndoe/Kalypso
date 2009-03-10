/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.ui.editor.featureeditor;

import java.net.URL;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.commons.command.DefaultCommandManager;
import org.kalypso.template.featureview.Featuretemplate;
import org.kalypso.util.command.JobExclusiveCommandTarget;

/**
 * @author Stefan Kurzbach
 */
public class FeatureTemplateView extends ViewPart
{
  public static final String ID = "org.kalypso.ui.views.featuretemplateview"; //$NON-NLS-1$

// private static final String MEMENTO_FILE = "file";

// private static final String RELOAD_MAP_ON_OPEN = "reloadMapOnOpen";

  protected final JobExclusiveCommandTarget m_commandTarget = new JobExclusiveCommandTarget( new DefaultCommandManager(), null );

  private final FeatureTemplateviewer m_templateviewer = new FeatureTemplateviewer( m_commandTarget, 0, 0 );

  private String m_partName;

  /**
   * @see org.eclipse.ui.part.ViewPart#init(org.eclipse.ui.IViewSite, org.eclipse.ui.IMemento)
   */
  @Override
  public void init( final IViewSite site, final IMemento memento ) throws PartInitException
  {
    super.init( site, memento );

// if( memento != null )
// {
// final String fullPath = memento.getString( MEMENTO_FILE );
// if( fullPath != null )
// {
// final IPath path = Path.fromPortableString( fullPath );
// m_file = ResourcesPlugin.getWorkspace().getRoot().getFile( path );
// }
// }
  }

  /**
   * @see org.eclipse.ui.part.ViewPart#saveState(org.eclipse.ui.IMemento)
   */
  @Override
  public void saveState( final IMemento memento )
  {
// if( m_file != null )
// {
// final IPath fullPath = m_file.getFullPath();
// if( fullPath != null )
// memento.putString( MEMENTO_FILE, fullPath.toPortableString() );
// }
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPartControl( final Composite parent )
  {
    // TODO: add listener to data, in order to show in title if data is dirty

    m_templateviewer.createControls( parent, SWT.NONE );

// // Stefan: Now we can restore the file if the view is configured to do so
// final String reloadOnOpen = getConfigurationElement().getAttribute( RELOAD_MAP_ON_OPEN );
// if( m_file != null && "true".equals( reloadOnOpen ) )
// {
// loadFromTemplate( m_file );
// }

    final IActionBars actionBars = getViewSite().getActionBars();
    actionBars.setGlobalActionHandler( ActionFactory.UNDO.getId(), m_commandTarget.undoAction );
    actionBars.setGlobalActionHandler( ActionFactory.REDO.getId(), m_commandTarget.redoAction );
    actionBars.updateActionBars();
  }

  /**
   * @exception SWTException
   *              <ul>
   *              <li>ERROR_THREAD_INVALID_ACCESS - if not called from the thread that created the parent</li>
   *              </ul>
   */
  public void setTemplate( final Featuretemplate template, final URL context, final String featurePath, final String href, final String linkType )
  {
    m_templateviewer.setTemplate( template, context, featurePath, href, linkType );

    final String title = template.getViewtitle();
    if( title.length() > 0 )
    {
      setPartName( title );
    }

    final String partName = template.getName();

    if( partName != null )
      setCustomName( partName );

  }

  public void setCustomName( final String name )
  {
    m_partName = name;
    final IWorkbench workbench = getSite().getWorkbenchWindow().getWorkbench();
    if( !workbench.isClosing() )
    {
      workbench.getDisplay().asyncExec( new Runnable()
      {
        @SuppressWarnings("synthetic-access")//$NON-NLS-1$
        public void run( )
        {
          setPartName( m_partName );
        }
      } );
    }
  }

  @Override
  public void dispose( )
  {
    m_commandTarget.dispose();
    if( m_templateviewer != null )
    {
      m_templateviewer.dispose();
    }
    super.dispose();
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
   */
  @Override
  public void setFocus( )
  {
    final Control control = m_templateviewer.getControl();
    if( control != null )
    {
      control.setFocus();
    }
  }

}
