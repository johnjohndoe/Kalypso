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
package org.kalypso.ui.calcwizard.createpages;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.nature.ModelNature;
import org.kalypso.ui.nature.prognose.CalcCaseTableTreeViewer;

/**
 * Diese Implementierung erzeugt eine völlig neue Rechenvariante im Prognoseverzeichnis
 * 
 * @author belger
 */
public class CopyCalcCaseChoice implements IAddCalcCaseChoice
{
  private Control m_control;

  private IFolder m_folder;

  private final String m_label;

  private final IProject m_project;

  private CalcCaseTableTreeViewer m_viewer;

  private final CreateCalcCasePage m_page;

  private String m_name;

  private Text m_edit;

  public CopyCalcCaseChoice( final String label, final IProject project, final CreateCalcCasePage page )
  {
    m_label = label;
    m_project = project;
    m_page = page;
  }

  /**
   * @see org.kalypso.ui.calcwizard.createpages.IAddCalcCaseChoice#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    panel.setLayout( new GridLayout() );

    // entry field for new name
    final Label nameLabel = new Label( panel, SWT.NONE );
    nameLabel.setLayoutData( new GridData() );
    nameLabel.setText( "Bezeichnung:" );
    nameLabel.setToolTipText( AddNewCalcCaseChoice.TOOLTIP );

    final Text edit = new Text( panel, SWT.BORDER );
    edit.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    edit.setToolTipText( AddNewCalcCaseChoice.TOOLTIP );
    edit.addModifyListener( new ModifyListener()
    {
      public void modifyText( ModifyEvent e )
      {
        setName( edit.getText() );
      }
    } );
    m_edit = edit;

    // Tree of available calc cases
    final Label label = new Label( panel, SWT.NONE );
    label.setText( "wählen Sie eine der vorhandenen Hochwasser-Vorhersagen:" );

    final CalcCaseTableTreeViewer viewer = new CalcCaseTableTreeViewer( null, panel, SWT.BORDER | SWT.SINGLE | SWT.FULL_SELECTION );
    final GridData viewerData = new GridData( GridData.FILL_BOTH );
    viewerData.horizontalSpan = 2;
    viewer.getControl().setLayoutData( viewerData );
    
    viewer.setInput( m_project.getFolder( ModelNature.PROGNOSE_FOLDER ) );

    viewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection)viewer.getSelection();
        if( selection.isEmpty() )
          setFolder( null );
        else
        {
          final IFolder folder = (IFolder)selection.getFirstElement();
          if( ModelNature.isCalcCalseFolder( folder ) )
            setFolder( folder );
          else
            setFolder( null );
        }
      }
    } );
    m_viewer = viewer;

    m_control = panel;

    try
    {
      refresh( new NullProgressMonitor() );
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
    }
  }

  protected void setName( final String text )
  {
    m_name = text;

    validateChoice();
  }

  protected void setFolder( final IFolder folder )
  {
    m_folder = folder;

    if( m_folder != null )
      m_edit.setText( "Kopie von " + folder.getName() );
    
    validateChoice();
  }

  public void refresh( final IProgressMonitor monitor ) throws CoreException
  {
    m_viewer.refresh();
  }
  
  /**
   * @throws CoreException
   * @see org.kalypso.ui.calcwizard.createpages.IAddCalcCaseChoice#perform(org.eclipse.core.runtime.IProgressMonitor)
   */
  public IFolder perform( final IProgressMonitor monitor ) throws CoreException
  {
    final ModelNature nature = (ModelNature)m_project.getNature( ModelNature.ID );

    final IFolder folder = nature.getPrognoseFolder();
    if( m_name.length() == 0 )
      throw new CoreException( KalypsoGisPlugin
          .createErrorStatus( "Geben Sie einen Namen für die Vorhersage ein", null ) );

    final IFolder calcCaseFolder = folder.getFolder( m_name );
    if( calcCaseFolder.exists() )
      throw new CoreException( KalypsoGisPlugin.createErrorStatus(
          "Eine Vorhersage mit diesem namen existiert bereits: " + m_name, null ) );

    if( m_folder == null )
      throw new CoreException( KalypsoGisPlugin.createErrorStatus(
          "Es muss eine vorhandene Berechnung ausgewählt werden", null ) );

    // quellverzeichnis holen
    m_folder.copy( calcCaseFolder.getFullPath(), false, monitor );

    return calcCaseFolder;
  }

  /**
   * @see org.kalypso.ui.calcwizard.createpages.IAddCalcCaseChoice#getControl()
   */
  public Control getControl()
  {
    return m_control;
  }

  /**
   * @see org.kalypso.ui.calcwizard.createpages.IAddCalcCaseChoice#toString()
   */
  public String toString()
  {
    return m_label;
  }

  /**
   * @see org.kalypso.ui.calcwizard.createpages.IAddCalcCaseChoice#shouldUpdate()
   */
  public boolean shouldUpdate()
  {
    return true;
  }

  /**
   * @see org.kalypso.ui.calcwizard.createpages.IAddCalcCaseChoice#validateChoice()
   */
  public void validateChoice()
  {
    if( m_folder == null )
    {
      m_page.setErrorMessage( "Es muss eine vorhandene Rechenvariante ausgewählt werden." );
      m_page.setMessage( null );
      m_page.setPageComplete( false );
    }

    final IStatus status = m_project.getWorkspace().validateName( m_name, IResource.FOLDER );
    if( status.getSeverity() == IStatus.OK )
    {
      m_page.setErrorMessage( null );
      m_page.setMessage( null );
      m_page.setPageComplete( true );
    }
    else
    {
      m_page.setErrorMessage( status.getMessage() );
      m_page.setMessage( null );
      m_page.setPageComplete( false );
    }
  }
}