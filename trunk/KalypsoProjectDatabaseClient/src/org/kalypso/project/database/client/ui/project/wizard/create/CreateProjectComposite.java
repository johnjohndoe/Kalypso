/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.project.database.client.ui.project.wizard.create;

import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.IPageChangedListener;
import org.eclipse.jface.dialogs.PageChangedEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.WizardNewProjectCreationPage;
import org.eclipse.ui.forms.events.HyperlinkAdapter;
import org.eclipse.ui.forms.events.HyperlinkEvent;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ImageHyperlink;
import org.kalypso.contribs.eclipse.core.resources.ProjectTemplate;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.wizard.IUpdateable;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.project.database.client.KalypsoProjectDatabaseClient;
import org.kalypso.project.database.client.core.interfaces.IProjectDatabaseFilter;
import org.kalypso.project.database.client.core.model.ProjectDatabaseModel;
import org.kalypso.project.database.client.core.model.ProjectHandler;
import org.kalypso.project.database.common.nature.IRemoteProjectPreferences;
import org.kalypso.project.database.common.nature.RemoteProjectNature;
import org.kalypso.project.database.sei.beans.KalypsoProjectBean;

/**
 * Composite for calling the new project wizard
 * 
 * @author Dirk Kuch
 */
public class CreateProjectComposite extends Composite
{
  private static Image IMG_CREATE = new Image( null, CreateProjectComposite.class.getResourceAsStream( "icons/create.gif" ) );

  private final FormToolkit m_toolkit;

  protected final String[] m_remote;

  protected final String[] m_natures;

  protected final String m_remoteCommitType;

  /**
   * @param remote
   *          project templates (project type ids) hosted by the project model data service
   * @param natures
   *          natures which will be added to the new create project
   * @param remoteCommitType
   *          as which {@link KalypsoProjectBean}.m_projectType will a newly created project committed
   */
  public CreateProjectComposite( final Composite parent, final FormToolkit toolkit, final String[] remote, final String[] natures, final String remoteCommitType )
  {
    super( parent, SWT.NULL );
    m_toolkit = toolkit;
    m_remote = remote;
    m_natures = natures;
    m_remoteCommitType = remoteCommitType;

    update();
  }

  /**
   * @see org.eclipse.swt.widgets.Control#update()
   */
  @Override
  public void update( )
  {
    if( this.isDisposed() )
      return;

    final ImageHyperlink lnkCreateProject = m_toolkit.createImageHyperlink( this, SWT.NULL );
    lnkCreateProject.setImage( IMG_CREATE );
    lnkCreateProject.setText( "Neues Projekt erzeugen" );

    lnkCreateProject.addHyperlinkListener( new HyperlinkAdapter()
    {
      /**
       * @see org.eclipse.ui.forms.events.HyperlinkAdapter#linkActivated(org.eclipse.ui.forms.events.HyperlinkEvent)
       */
      @Override
      public void linkActivated( final HyperlinkEvent e )
      {

        final ProjectDatabaseModel model = KalypsoProjectDatabaseClient.getDefault().getProjectDatabaseModel();

        final ProjectHandler[] projects = model.getProjects( new IProjectDatabaseFilter()
        {
          @Override
          public boolean select( final ProjectHandler handler )
          {
            if( !handler.isRemote() )
              return false;

            final KalypsoProjectBean bean = handler.getBean();
            final String type = bean.getProjectType();

            return ArrayUtils.contains( m_remote, type );
          }
        } );

        final List<ProjectTemplate> templates = new ArrayList<ProjectTemplate>();

        for( final ProjectHandler handler : projects )
        {
          if( handler.isRemote() )
          {
            try
            {
              final KalypsoProjectBean bean = handler.getBean();
              final ProjectTemplate template = new ProjectTemplate( bean.getName(), bean.getUnixName(), bean.getDescription(), null, bean.getUrl() );

              templates.add( template );
            }
            catch( final MalformedURLException e1 )
            {
              KalypsoProjectDatabaseClient.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e1 ) );
            }
          }
        }

        final WizardCreateProject wizard = new WizardCreateProject( templates.toArray( new ProjectTemplate[] {} ), m_natures );
        wizard.init( PlatformUI.getWorkbench(), null );
        wizard.setActivateScenarioOnPerformFinish( false );

        final WizardDialog2 dialog = new WizardDialog2( null, wizard );
        dialog.setRememberSize( true );

        dialog.addPageChangedListener( new IPageChangedListener()
        {
          private boolean resetProjectName = true;

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
              if( resetProjectName )
              {
                final WizardNewProjectCreationPage myPage = (WizardNewProjectCreationPage) page;
                DisableCreateProjectWizardPageElements.disableElementsForProjectCreation( myPage );

                resetProjectName = false;
              }
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
            final RemoteProjectNature remote = (RemoteProjectNature) nature;
            final IRemoteProjectPreferences preferences = remote.getRemotePreferences( project, null );
            preferences.setVersion( -1 );
            preferences.setIsOnServer( Boolean.FALSE );
            preferences.setProjectType( m_remoteCommitType );
          }
        }
        catch( final CoreException e1 )
        {
          KalypsoProjectDatabaseClient.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e1 ) );
        }

      }
    } );

    m_toolkit.adapt( this );
    this.layout();
  }
}
