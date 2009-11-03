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
package org.kalypso.model.product.application;

import java.net.URL;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.IExecutionListener;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.application.IWorkbenchConfigurer;
import org.eclipse.ui.application.IWorkbenchWindowConfigurer;
import org.eclipse.ui.application.WorkbenchAdvisor;
import org.eclipse.ui.application.WorkbenchWindowAdvisor;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.internal.ide.IDEInternalWorkbenchImages;
import org.eclipse.ui.internal.ide.IDEWorkbenchPlugin;
import org.kalypso.afgui.perspective.Perspective;
import org.kalypso.ogc.gml.map.handlers.listener.MapScreenshotExecuteListener;
import org.osgi.framework.Bundle;

/**
 * @author Holger Albert
 */
@SuppressWarnings("restriction")
public class KalypsoModelWorkbenchAdvisor extends WorkbenchAdvisor
{
  /**
   * Restricted access?
   */
  private final boolean m_restrictedAccess;

  /**
   * This is an execute listener.
   */
  private final IExecutionListener m_executeListener;

  /**
   * @param restrictedAccess
   *          See
   *          {@link KalypsoModelWorkbenchWindowAdvisor#KalypsoModelWorkbenchWindowAdvisor(KalypsoModelWorkbenchAdvisor, IWorkbenchWindowConfigurer, boolean)}
   */
  public KalypsoModelWorkbenchAdvisor( final boolean restrictedAccess )
  {
    m_restrictedAccess = restrictedAccess;
    m_executeListener = new MapScreenshotExecuteListener();
  }

  /**
   * @see org.eclipse.ui.application.WorkbenchAdvisor#getInitialWindowPerspectiveId()
   */
  @Override
  public String getInitialWindowPerspectiveId( )
  {
    return Perspective.ID;
  }

  /**
   * @see org.eclipse.ui.application.WorkbenchAdvisor#createWorkbenchWindowAdvisor(org.eclipse.ui.application.IWorkbenchWindowConfigurer)
   */
  @Override
  public WorkbenchWindowAdvisor createWorkbenchWindowAdvisor( final IWorkbenchWindowConfigurer configurer )
  {
    return new KalypsoModelWorkbenchWindowAdvisor( configurer, m_restrictedAccess );
  }

  /**
   * @see org.eclipse.ui.application.WorkbenchAdvisor#preStartup()
   */
  @Override
  public void preStartup( )
  {
    IDE.registerAdapters();
  }

  /**
   * @see org.eclipse.ui.application.WorkbenchAdvisor#postStartup()
   */
  @Override
  public void postStartup( )
  {
    super.postStartup();

    /* Add a command listener for the screenshot command of the map. */
    final ICommandService service = (ICommandService) PlatformUI.getWorkbench().getService( ICommandService.class );
    final Command command = service.getCommand( "org.kalypso.ogc.gml.map.Screenshot" ); //$NON-NLS-1$
    command.addExecutionListener( m_executeListener );
  }

  /**
   * @see org.eclipse.ui.application.WorkbenchAdvisor#preShutdown()
   */
  @Override
  public boolean preShutdown( )
  {
    /* Remove the execute listener for the srcreenshot command. */
    final ICommandService service = (ICommandService) PlatformUI.getWorkbench().getService( ICommandService.class );
    final Command command = service.getCommand( "org.kalypso.ogc.gml.map.Screenshot" ); //$NON-NLS-1$
    command.removeExecutionListener( m_executeListener );

    return super.preShutdown();
  }

  /**
   * @see org.eclipse.ui.application.WorkbenchAdvisor#initialize(org.eclipse.ui.application.IWorkbenchConfigurer)
   */
  @Override
  public void initialize( IWorkbenchConfigurer configurer )
  {
    configurer.setSaveAndRestore( true );
    declareWorkbenchImages();
  }

  /**
   * @see org.eclipse.ui.application.WorkbenchAdvisor#getDefaultPageInput()
   */
  @Override
  public IAdaptable getDefaultPageInput( )
  {
    return new NavigatorRoot();
  }

  /**
   * Declares all IDE-specific workbench images. This includes both "shared" images (named in {@link IDE.SharedImages})
   * and internal images (named in {@link org.eclipse.ui.internal.ide.IDEInternalWorkbenchImages}).
   * 
   * @see org.eclipse.ui.internal.ide.IDEWorkbenchAdvisor#declareImage
   */
  protected void declareWorkbenchImages( )
  {
    final String ICONS_PATH = "$nl$/icons/full/"; //$NON-NLS-1$
    final String PATH_ELOCALTOOL = ICONS_PATH + "elcl16/"; // Enabled //$NON-NLS-1$
    final String PATH_DLOCALTOOL = ICONS_PATH + "dlcl16/"; // Disabled //$NON-NLS-1$
    final String PATH_ETOOL = ICONS_PATH + "etool16/"; // Enabled toolbar //$NON-NLS-1$
    final String PATH_DTOOL = ICONS_PATH + "dtool16/"; // Disabled toolbar //$NON-NLS-1$
    final String PATH_OBJECT = ICONS_PATH + "obj16/"; // Model object //$NON-NLS-1$
    final String PATH_WIZBAN = ICONS_PATH + "wizban/"; // Wizard //$NON-NLS-1$

    Bundle ideBundle = Platform.getBundle( IDEWorkbenchPlugin.IDE_WORKBENCH );
    declareWorkbenchImage( ideBundle, IDEInternalWorkbenchImages.IMG_ETOOL_BUILD_EXEC, PATH_ETOOL + "build_exec.gif", false ); //$NON-NLS-1$
    declareWorkbenchImage( ideBundle, IDEInternalWorkbenchImages.IMG_ETOOL_BUILD_EXEC_HOVER, PATH_ETOOL + "build_exec.gif", false ); //$NON-NLS-1$
    declareWorkbenchImage( ideBundle, IDEInternalWorkbenchImages.IMG_ETOOL_BUILD_EXEC_DISABLED, PATH_DTOOL + "build_exec.gif", false ); //$NON-NLS-1$
    declareWorkbenchImage( ideBundle, IDEInternalWorkbenchImages.IMG_ETOOL_SEARCH_SRC, PATH_ETOOL + "search_src.gif", false ); //$NON-NLS-1$
    declareWorkbenchImage( ideBundle, IDEInternalWorkbenchImages.IMG_ETOOL_SEARCH_SRC_HOVER, PATH_ETOOL + "search_src.gif", false ); //$NON-NLS-1$
    declareWorkbenchImage( ideBundle, IDEInternalWorkbenchImages.IMG_ETOOL_SEARCH_SRC_DISABLED, PATH_DTOOL + "search_src.gif", false ); //$NON-NLS-1$
    declareWorkbenchImage( ideBundle, IDEInternalWorkbenchImages.IMG_ETOOL_NEXT_NAV, PATH_ETOOL + "next_nav.gif", false ); //$NON-NLS-1$
    declareWorkbenchImage( ideBundle, IDEInternalWorkbenchImages.IMG_ETOOL_PREVIOUS_NAV, PATH_ETOOL + "prev_nav.gif", false ); //$NON-NLS-1$
    declareWorkbenchImage( ideBundle, IDEInternalWorkbenchImages.IMG_WIZBAN_NEWPRJ_WIZ, PATH_WIZBAN + "newprj_wiz.png", false ); //$NON-NLS-1$
    declareWorkbenchImage( ideBundle, IDEInternalWorkbenchImages.IMG_WIZBAN_NEWFOLDER_WIZ, PATH_WIZBAN + "newfolder_wiz.png", false ); //$NON-NLS-1$
    declareWorkbenchImage( ideBundle, IDEInternalWorkbenchImages.IMG_WIZBAN_NEWFILE_WIZ, PATH_WIZBAN + "newfile_wiz.png", false ); //$NON-NLS-1$
    declareWorkbenchImage( ideBundle, IDEInternalWorkbenchImages.IMG_WIZBAN_IMPORTDIR_WIZ, PATH_WIZBAN + "importdir_wiz.png", false ); //$NON-NLS-1$
    declareWorkbenchImage( ideBundle, IDEInternalWorkbenchImages.IMG_WIZBAN_IMPORTZIP_WIZ, PATH_WIZBAN + "importzip_wiz.png", false ); //$NON-NLS-1$
    declareWorkbenchImage( ideBundle, IDEInternalWorkbenchImages.IMG_WIZBAN_EXPORTDIR_WIZ, PATH_WIZBAN + "exportdir_wiz.png", false ); //$NON-NLS-1$
    declareWorkbenchImage( ideBundle, IDEInternalWorkbenchImages.IMG_WIZBAN_EXPORTZIP_WIZ, PATH_WIZBAN + "exportzip_wiz.png", false ); //$NON-NLS-1$
    declareWorkbenchImage( ideBundle, IDEInternalWorkbenchImages.IMG_WIZBAN_RESOURCEWORKINGSET_WIZ, PATH_WIZBAN + "workset_wiz.png", false ); //$NON-NLS-1$
    declareWorkbenchImage( ideBundle, IDEInternalWorkbenchImages.IMG_DLGBAN_SAVEAS_DLG, PATH_WIZBAN + "saveas_wiz.png", false ); //$NON-NLS-1$
    declareWorkbenchImage( ideBundle, IDEInternalWorkbenchImages.IMG_DLGBAN_QUICKFIX_DLG, PATH_WIZBAN + "quick_fix.png", false ); //$NON-NLS-1$
    declareWorkbenchImage( ideBundle, IDE.SharedImages.IMG_OBJ_PROJECT, PATH_OBJECT + "prj_obj.gif", true ); //$NON-NLS-1$
    declareWorkbenchImage( ideBundle, IDE.SharedImages.IMG_OBJ_PROJECT_CLOSED, PATH_OBJECT + "cprj_obj.gif", true ); //$NON-NLS-1$
    declareWorkbenchImage( ideBundle, IDE.SharedImages.IMG_OPEN_MARKER, PATH_ELOCALTOOL + "gotoobj_tsk.gif", true ); //$NON-NLS-1$
    declareWorkbenchImage( ideBundle, IDEInternalWorkbenchImages.IMG_ELCL_QUICK_FIX_ENABLED, PATH_ELOCALTOOL + "smartmode_co.gif", true ); //$NON-NLS-1$
    declareWorkbenchImage( ideBundle, IDEInternalWorkbenchImages.IMG_DLCL_QUICK_FIX_DISABLED, PATH_DLOCALTOOL + "smartmode_co.gif", true ); //$NON-NLS-1$
    declareWorkbenchImage( ideBundle, IDE.SharedImages.IMG_OBJS_TASK_TSK, PATH_OBJECT + "taskmrk_tsk.gif", true ); //$NON-NLS-1$
    declareWorkbenchImage( ideBundle, IDE.SharedImages.IMG_OBJS_BKMRK_TSK, PATH_OBJECT + "bkmrk_tsk.gif", true ); //$NON-NLS-1$
    declareWorkbenchImage( ideBundle, IDEInternalWorkbenchImages.IMG_OBJS_COMPLETE_TSK, PATH_OBJECT + "complete_tsk.gif", true ); //$NON-NLS-1$
    declareWorkbenchImage( ideBundle, IDEInternalWorkbenchImages.IMG_OBJS_INCOMPLETE_TSK, PATH_OBJECT + "incomplete_tsk.gif", true ); //$NON-NLS-1$
    declareWorkbenchImage( ideBundle, IDEInternalWorkbenchImages.IMG_OBJS_WELCOME_ITEM, PATH_OBJECT + "welcome_item.gif", true ); //$NON-NLS-1$
    declareWorkbenchImage( ideBundle, IDEInternalWorkbenchImages.IMG_OBJS_WELCOME_BANNER, PATH_OBJECT + "welcome_banner.gif", true ); //$NON-NLS-1$
    declareWorkbenchImage( ideBundle, IDEInternalWorkbenchImages.IMG_OBJS_ERROR_PATH, PATH_OBJECT + "error_tsk.gif", true ); //$NON-NLS-1$
    declareWorkbenchImage( ideBundle, IDEInternalWorkbenchImages.IMG_OBJS_WARNING_PATH, PATH_OBJECT + "warn_tsk.gif", true ); //$NON-NLS-1$
    declareWorkbenchImage( ideBundle, IDEInternalWorkbenchImages.IMG_OBJS_INFO_PATH, PATH_OBJECT + "info_tsk.gif", true ); //$NON-NLS-1$
    declareWorkbenchImage( ideBundle, IDEInternalWorkbenchImages.IMG_LCL_FLAT_LAYOUT, PATH_ELOCALTOOL + "flatLayout.gif", true ); //$NON-NLS-1$
    declareWorkbenchImage( ideBundle, IDEInternalWorkbenchImages.IMG_LCL_HIERARCHICAL_LAYOUT, PATH_ELOCALTOOL + "hierarchicalLayout.gif", true ); //$NON-NLS-1$
    declareWorkbenchImage( ideBundle, IDEInternalWorkbenchImages.IMG_ETOOL_PROBLEM_CATEGORY, PATH_ETOOL + "problem_category.gif", true ); //$NON-NLS-1$
  }

  /**
   * Declares an IDE-specific workbench image.
   * 
   * @param symbolicName
   *          the symbolic name of the image
   * @param path
   *          the path of the image file; this path is relative to the base of the IDE plug-in
   * @param shared
   *          <code>true</code> if this is a shared image, and <code>false</code> if this is not a shared image
   * @see org.eclipse.ui.internal.ide.IDEWorkbenchAdvisor#declareImage
   */
  private void declareWorkbenchImage( Bundle ideBundle, String symbolicName, String path, boolean shared )
  {
    URL url = FileLocator.find( ideBundle, new Path( path ), null );
    ImageDescriptor desc = ImageDescriptor.createFromURL( url );
    getWorkbenchConfigurer().declareImage( symbolicName, desc, shared );
  }
}
