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

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.nature.CalcCaseCollector;
import org.kalypso.ui.nature.ModelNature;

/**
 * Die Implementierung erzeugt eine völlig neue Rechenvariante im Prognoseverzeichnis
 * 
 * @author belger
 */
public class ContinueOldCalcCaseChoice implements IAddCalcCaseChoice
{
  private Control m_control;

  private IFolder m_folder;

  private final String m_label;

  private final IProject m_project;

  private ListViewer m_viewer;

  private Collection m_oldCalcCases = new LinkedList();

  private final AddCalcCasePage m_page;

  public ContinueOldCalcCaseChoice( final String label, final IProject project, final AddCalcCasePage page )
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

    final Label label = new Label( panel, SWT.NONE );
    label.setText( "wählen Sie eine der vorhandenen Hochwasser-Vorhersagen:" );

    final ListViewer viewer = new ListViewer( panel, SWT.BORDER );
    viewer.setContentProvider( new ArrayContentProvider() );
    viewer.setLabelProvider( new WorkbenchLabelProvider() );
    viewer.setInput( m_oldCalcCases );
    viewer.getControl().setLayoutData( new GridData( GridData.FILL_BOTH ) );

    viewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection)viewer.getSelection();
        if( selection.isEmpty() )
          setFolder( null );
        else
          setFolder( (IFolder)selection.getFirstElement() );
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

  protected void setFolder( final IFolder folder )
  {
    m_folder = folder;

    validateChoice();
  }

  public void refresh( final IProgressMonitor monitor ) throws CoreException
  {
    final ModelNature nature = (ModelNature)m_project.getNature( ModelNature.ID );
    final IFolder prognoseFolder = nature.getPrognoseFolder();

    // alle Prognosen finden
    final CalcCaseCollector calcCaseCollector = new CalcCaseCollector();
    if( prognoseFolder.exists() )
      prognoseFolder.accept( calcCaseCollector );
    final IFolder[] calcCases = calcCaseCollector.getCalcCases();

    final List usedCalcCases = m_page.getCalcCases();

    m_oldCalcCases.clear();

    IFolder newSelect = null;
    if( usedCalcCases != null )
    {
      for( int i = 0; i < calcCases.length; i++ )
      {
        final IFolder folder = calcCases[i];

        if( !usedCalcCases.contains( folder ) )
        {
          m_oldCalcCases.add( folder );
          if( newSelect == null )
            newSelect = folder;
        }
      }
    }

    final IFolder newSelectFinal = newSelect;
    final Viewer viewer = m_viewer;
    if( viewer != null )
    {
      viewer.getControl().getDisplay().syncExec( new Runnable()
      {
        public void run()
        {
          viewer.refresh();

          if( newSelectFinal == null )
            viewer.setSelection( StructuredSelection.EMPTY );
          else
            viewer.setSelection( new StructuredSelection( newSelectFinal ) );
        }
      } );
    }

    //    m_page.getWizard().getContainer().updateButtons();
  }

  /**
   * @throws CoreException
   * @see org.kalypso.ui.calcwizard.createpages.IAddCalcCaseChoice#perform(org.eclipse.core.runtime.IProgressMonitor)
   */
  public IFolder perform( final IProgressMonitor monitor ) throws CoreException
  {
    if( m_folder == null )
      throw new CoreException( KalypsoGisPlugin.createErrorStatus(
          "Es muss eine vorhandene Berechnung ausgewählt werden", null ) );

    return m_folder;
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
    return false;
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
    else
    {
      m_page.setErrorMessage( null );
      m_page.setMessage( null );
      m_page.setPageComplete( true );
    }
  }
}