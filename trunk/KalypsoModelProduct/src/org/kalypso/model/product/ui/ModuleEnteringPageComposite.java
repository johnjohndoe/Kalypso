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
package org.kalypso.model.product.ui;

import java.awt.Point;
import java.net.URL;

import org.eclipse.core.runtime.Assert;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.widgets.ExpandableComposite;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.afgui.extension.IEnteringPageWizardDelegate;
import org.kalypso.afgui.extension.IKalypsoModuleEnteringPageHandler;
import org.kalypso.afgui.extension.IKalypsoModulePageHandler;
import org.kalypso.afgui.extension.IKalypsoProjectOpenAction;
import org.kalypso.afgui.extension.INewProjectWizard;
import org.kalypso.afgui.extension.IProjectDatabaseFilter;
import org.kalypso.contribs.eclipse.swt.canvas.DefaultContentArea;
import org.kalypso.contribs.eclipse.swt.canvas.ImageCanvas2;
import org.kalypso.contribs.eclipse.ui.controls.ScrolledSection;
import org.kalypso.model.product.KalypsoModelProductPlugin;
import org.kalypso.model.product.utils.MyColors;
import org.kalypso.model.product.utils.MyFonts;
import org.kalypso.project.database.client.core.utils.ProjectDatabaseServerUtils;
import org.kalypso.project.database.client.ui.project.database.ProjectDatabaseComposite;
import org.kalypso.project.database.client.ui.project.status.ProjectDatabaseServerStatusComposite;
import org.kalypso.project.database.client.ui.project.wizard.create.CreateProjectComposite;
import org.kalypso.project.database.client.ui.project.wizard.imp.ImportProjectComposite;
import org.kalypso.project.database.client.ui.project.wizard.imp.SpecialImportProjectComposite;

/**
 * @author Dirk Kuch
 */
public class ModuleEnteringPageComposite extends Composite
{
  protected final IKalypsoModulePageHandler m_pageHandler;

  protected final IKalypsoModuleEnteringPageHandler m_enteringPage;

  private static final Color COLOR_BOX = new Color( null, 0x7f, 0xb2, 0x99 );

  public ModuleEnteringPageComposite( final Composite parent, final int style, final IKalypsoModuleEnteringPageHandler enteringPage, final IKalypsoModulePageHandler pageHandler )
  {
    super( parent, style );

    Assert.isNotNull( enteringPage );
    Assert.isNotNull( pageHandler );

    m_enteringPage = enteringPage;
    m_pageHandler = pageHandler;

    final GridLayout layout = new GridLayout( 2, false );
    layout.horizontalSpacing = 100;
    layout.verticalSpacing = 25;
    layout.marginWidth = 75;

    this.setLayout( layout );

    update();
  }

  @Override
  public void update( )
  {

    final FormToolkit toolkit = KalypsoModelProductPlugin.getFormToolkit();

    /* header */
    // icon / button
    final ImageCanvas2 headerCanvas = new ImageCanvas2( this, SWT.NO_REDRAW_RESIZE );
    final GridData headerIconData = new GridData( GridData.FILL, GridData.FILL, true, false, 2, 0 );
    headerIconData.heightHint = headerIconData.minimumHeight = 110;
    headerCanvas.setLayoutData( headerIconData );

    final DefaultContentArea headerContent = new DefaultContentArea()
    {
      @Override
      public Point getContentAreaAnchorPoint( )
      {
        return new Point( 5, 40 );
      }
    };

    headerContent.setText( m_enteringPage.getHeader(), MyFonts.WELCOME_PAGE_HEADING, MyColors.COLOR_WELCOME_PAGE_HEADING, SWT.RIGHT );
    headerCanvas.addContentArea( headerContent );

    /* left pane */
    final Composite leftPane = toolkit.createComposite( this, SWT.NONE );
    leftPane.setLayout( new GridLayout() );
    final GridData leftGridData = new GridData( GridData.FILL, GridData.FILL, false, true );
    leftGridData.widthHint = leftGridData.minimumWidth = 400;
    leftPane.setLayoutData( leftGridData );
    leftPane.setBackground( COLOR_BOX );

    /* right pane */
    final Composite rightPane = toolkit.createComposite( this, SWT.NONE );
    rightPane.setLayout( new GridLayout() );
    rightPane.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );
    rightPane.setBackground( COLOR_BOX );

    renderListOfProjects( leftPane, toolkit );
    renderProjectInfo( rightPane, toolkit );
  }

  private void renderProjectInfo( final Composite body, final FormToolkit toolkit )
  {
    final Browser browser = new Browser( body, SWT.NULL );
    browser.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );

    try
    {
      final URL url = m_enteringPage.getInfoURL();
      browser.setUrl( url.toExternalForm() );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  private void renderListOfProjects( final Composite body, final FormToolkit toolkit )
  {
    // list of projects
    final ScrolledSection sectionProjects = new ScrolledSection( body, toolkit, ExpandableComposite.TITLE_BAR, true );
    final Composite bodyProjects = sectionProjects.setup( "Projekte:", new GridData( GridData.FILL, GridData.FILL, true, true ), new GridData( GridData.FILL, GridData.FILL, true, true ) );
    final GridLayout layout = new GridLayout( 2, true );
    layout.verticalSpacing = layout.marginWidth = 0;
    bodyProjects.setLayout( layout );
    bodyProjects.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    final IProjectDatabaseFilter handler = m_enteringPage.getDatabaseFilter();
    final IKalypsoProjectOpenAction openAction = m_enteringPage.getProjectOpenAction();

    final ProjectDatabaseComposite projects = new ProjectDatabaseComposite( bodyProjects, toolkit, handler, openAction, KalypsoModelProductPlugin.getDefault().isExpert() );
    projects.setLayout( new GridLayout() );
    projects.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true, 2, 0 ) );

    final IEnteringPageWizardDelegate projectDelegate = new IEnteringPageWizardDelegate()
    {

      @Override
      public Image getImage( )
      {
        return CreateProjectComposite.IMG_ADD_PROJECT;
      }

      @Override
      public String getRemoteCommitType( )
      {
        return m_enteringPage.getRemoteCommitType();
      }

      @Override
      public INewProjectWizard getWizard( )
      {
        return m_enteringPage.getProjectWizard();
      }
    };

    final CreateProjectComposite projectTemplate = new CreateProjectComposite( "Neues Projekt anlegen", bodyProjects, toolkit, projectDelegate );
    projectTemplate.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    final ImportProjectComposite projectImport = new ImportProjectComposite( bodyProjects, toolkit );
    projectImport.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    if( m_enteringPage.hasDemoProjectWizard() )
    {
      final IEnteringPageWizardDelegate demoDelegate = new IEnteringPageWizardDelegate()
      {

        @Override
        public Image getImage( )
        {
          return CreateProjectComposite.IMG_EXTRACT_DEMO;
        }

        @Override
        public String getRemoteCommitType( )
        {
          return m_enteringPage.getRemoteCommitType();
        }

        @Override
        public INewProjectWizard getWizard( )
        {
          return m_enteringPage.getDemoProjectWizard();
        }
      };

      final CreateProjectComposite demoProject = new CreateProjectComposite( "Demo-Projekt entpacken", bodyProjects, toolkit, demoDelegate );
      demoProject.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    }
    else
    {
      toolkit.createLabel( bodyProjects, "" ); // spacer
    }

    if( m_enteringPage.hasImportWizard() )
    {
      final SpecialImportProjectComposite specialImport = new SpecialImportProjectComposite( bodyProjects, toolkit, m_enteringPage );
      specialImport.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    }

    if( ProjectDatabaseServerUtils.handleRemoteProject() )
    {

      final ProjectDatabaseServerStatusComposite status = new ProjectDatabaseServerStatusComposite( bodyProjects, toolkit );
      status.setLayoutData( new GridData( GridData.FILL, GridData.FILL, false, false ) );
    }
  }
}
