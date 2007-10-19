/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.wspm.sobek.core.wizard.pages;

import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekModelMember;
import org.kalypso.model.wspm.sobek.core.ui.boundarycondition.RepositoryLabelProvider;
import org.kalypso.model.wspm.sobek.core.ui.boundarycondition.RepositoryTreeContentProvider;
import org.kalypso.model.wspm.sobek.core.ui.boundarycondition.RepositoryViewerFilter;

/**
 * @author kuch
 */
public class PageEditBoundaryConditionTimeSeries extends WizardPage
{
  private final IBoundaryConditionGeneral m_settings;

  private final ISobekModelMember m_model;

  public PageEditBoundaryConditionTimeSeries( final ISobekModelMember model, final IBoundaryConditionGeneral settings )
  {
    super( "editBoundaryConditionTimeSeries" );
    m_model = model;
    m_settings = settings;

    setTitle( "Edit boundary condition" );
    setDescription( "Enter boundary condition parameters, please." );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    setPageComplete( false );

    final Composite container = new Composite( parent, SWT.NULL );
    container.setLayout( new GridLayout( 2, false ) );
    setControl( container );

    final Composite cBrowser = new Composite( container, SWT.NULL );
    cBrowser.setLayoutData( new GridData( GridData.FILL, GridData.FILL, false, true ) );
    final GridLayout bLayout = new GridLayout();
    bLayout.marginWidth = bLayout.horizontalSpacing = 0;
    cBrowser.setLayout( bLayout );

    getTSBrowser( cBrowser );

    final Composite cClient = new Composite( container, SWT.NULL );
    cClient.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );
    final GridLayout cLayout = new GridLayout();
    cLayout.marginWidth = bLayout.horizontalSpacing = 0;
    cClient.setLayout( cLayout );

    checkPageCompleted();
  }

  private void getTSBrowser( final Composite body )
  {
    final CheckboxTreeViewer viewer = new CheckboxTreeViewer( body );
    viewer.getTree().setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );
    viewer.addFilter( new RepositoryViewerFilter() );
    viewer.setContentProvider( new RepositoryTreeContentProvider() );
    viewer.setLabelProvider( new RepositoryLabelProvider() );

    viewer.setInput( m_model.getRepositoryContainer() );

    viewer.expandAll();
  }

  protected void checkPageCompleted( )
  {
    setMessage( null );
    setErrorMessage( null );
    setPageComplete( false );
  }

}
