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
package org.kalypso.project.database.client.ui.project.list.internal;

import java.util.Map;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.IPageChangedListener;
import org.eclipse.jface.dialogs.PageChangedEvent;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.WizardNewProjectCreationPage;
import org.kalypso.contribs.eclipse.core.resources.ProjectTemplate;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.wizard.IUpdateable;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.project.database.client.KalypsoProjectDatabaseClient;
import org.kalypso.project.database.client.ui.project.wizard.create.WizardCreateProject;
import org.kalypso.project.database.common.nature.IRemoteProjectPreferences;
import org.kalypso.project.database.common.nature.RemoteProjectNature;
import org.kalypso.project.database.sei.beans.KalypsoProjectBean;

/**
 * @author kuch
 */
public class RemoteProjectHelper
{
  public static void importRemoteProject( final ProjectTemplate[] templates, final Map<ProjectTemplate, KalypsoProjectBean> mapping )
  {
    final WizardCreateProject wizard = new WizardCreateProject( templates, new String[] {} );
    wizard.init( PlatformUI.getWorkbench(), null );

    final WizardDialog2 dialog = new WizardDialog2( null, wizard );
    dialog.setRememberSize( true );

    dialog.addPageChangedListener( new IPageChangedListener()
    {
      public void pageChanged( final PageChangedEvent event )
      {
        final Object page = event.getSelectedPage();

        if( page instanceof IUpdateable )
        {
          final IUpdateable update = (IUpdateable) page;
          update.update();
        }
        else if( page instanceof WizardNewProjectCreationPage )
        {
          final WizardNewProjectCreationPage myPage = (WizardNewProjectCreationPage) page;
          final Composite myParent = (Composite) myPage.getControl();
          final Control[] children = myParent.getChildren();

          /* project name */
          final Composite subChildOne = (Composite) children[0];
          final Control[] subChildrenOne = subChildOne.getChildren();
          subChildrenOne[1].setEnabled( false );

          /* working sets */
          final Composite subChildTwo = (Composite) children[2];
          final Control[] subChildrenTwo = subChildTwo.getChildren();
          final Composite subSubChildTwo = (Composite) subChildrenTwo[0];
          final Control[] subSubChildrenTwo = subSubChildTwo.getChildren();
          subSubChildrenTwo[0].setEnabled( false );
        }
      }
    } );

    dialog.open();

    try
    {
      final IProject project = wizard.getNewProject();
      final IProjectNature nature = project.getNature( RemoteProjectNature.NATURE_ID );
      if( nature instanceof RemoteProjectNature )
      {
        // bad hack
        final KalypsoProjectBean bean = mapping.get( wizard.getSelectedTemplate() );

        final RemoteProjectNature remote = (RemoteProjectNature) nature;
        final IRemoteProjectPreferences preferences = remote.getRemotePreferences( project, null );
        preferences.setVersion( bean.getProjectVersion() );
        preferences.setIsOnServer( Boolean.TRUE );
      }
    }
    catch( final CoreException e1 )
    {
      KalypsoProjectDatabaseClient.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e1 ) );
    }

  }
}
