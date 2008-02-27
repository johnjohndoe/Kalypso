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
package org.kalypso.model.wspm.tuhh.ui.wizards;

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.kalypso.model.wspm.tuhh.core.util.TuhhDemoProject;

/**
 * @author Gernot Belger
 */
public class DemoProjectsWizardPage extends WizardPage
{
  private final TuhhDemoProject[] m_demoProjects;

  private TuhhDemoProject m_selectedProject;

  public DemoProjectsWizardPage( final String pageName, final TuhhDemoProject[] demoProjects )
  {
    super( pageName );

    m_demoProjects = demoProjects;

    setTitle( "Beispielprojekte" );
    setMessage( "Auf dieser Seite wählen Sie aus, welches Beispielprojekt Sie erzeugen möchten." );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    final Composite composite = new Composite( parent, SWT.NONE );
    setControl( composite );

    composite.setLayout( new GridLayout() );

    final TableViewer tableViewer = new TableViewer( composite, SWT.FULL_SELECTION | SWT.SINGLE | SWT.BORDER );
    tableViewer.getControl().setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    tableViewer.setContentProvider( new ArrayContentProvider() );
    tableViewer.setLabelProvider( new LabelProvider()
    {
      /**
       * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
       */
      @Override
      public String getText( final Object element )
      {
        return ((TuhhDemoProject) element).getLabel();
      }

      /**
       * @see org.eclipse.jface.viewers.LabelProvider#getImage(java.lang.Object)
       */
      @Override
      public Image getImage( final Object element )
      {
        final String icon = ((TuhhDemoProject) element).getIcon();
        if( icon == null || icon.length() == 0 )
          return null;

        // TODO: create icon and dispose after use

        return super.getImage( element );
      }
    } );

    tableViewer.setInput( m_demoProjects );

    // Info Group
    final Group group = new Group( composite, SWT.NONE );
    group.setLayout( new GridLayout() );
    group.setText( "Projektbeschreibung" );
    final GridData groupData = new GridData( SWT.FILL, SWT.CENTER, true, false );
    groupData.heightHint = 100;
    group.setLayoutData( groupData );

    final Label descriptionLabel = new Label( group, SWT.H_SCROLL | SWT.V_SCROLL );
    descriptionLabel.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    descriptionLabel.setText( "-kein Projekt gewählt-" );

    tableViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        final TuhhDemoProject selectedProject = (TuhhDemoProject) selection.getFirstElement();

        setSelectedProject( selectedProject );

        if( selectedProject == null )
          descriptionLabel.setText( "" );
        else
          descriptionLabel.setText( selectedProject.getDescription() );
      }
    } );

    if( m_selectedProject != null )
      tableViewer.setSelection( new StructuredSelection( m_selectedProject ), true );
  }

  protected void setSelectedProject( final TuhhDemoProject selectedProject )
  {
    m_selectedProject = selectedProject;

    getContainer().updateButtons();
  }

  /**
   * @see org.eclipse.jface.wizard.WizardPage#canFlipToNextPage()
   */
  @Override
  public boolean canFlipToNextPage( )
  {
    return m_selectedProject != null;
  }

  public TuhhDemoProject getSelectedProject( )
  {
    return m_selectedProject;
  }

}
