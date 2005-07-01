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
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.kalypso.ui.calcwizard.ICalcWizardPage;

/**
 * @deprecated Der Wizard soll nur noch eine Rechenvariante enthalten, das Konzept der 'aktuellen Rechenvarianten' soll weg.
 * 
 * @author belger
 */
public class ICalcWizardStartingPage extends WizardPage implements ICalcWizardPage
{
  private final List m_choices = new LinkedList();

  private ComboViewer m_chooserViewer;

  private List m_calcCases = new LinkedList();

  private IFolder m_currentCalcCase = null;

  private IAddCalcCaseChoice m_choice;

  private ListViewer m_caseViewer;

  private Collection m_choiceListener = new LinkedList();

  public ICalcWizardStartingPage( final String pagename, final String title, final ImageDescriptor image )
  {
    super( pagename, title, image );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#dispose()
   */
  public void dispose()
  {
    m_choiceListener.clear();

    super.dispose();
  }

  public void addChoice( final IAddCalcCaseChoice choice )
  {
    m_choices.add( choice );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    panel.setLayout( new GridLayout() );

    createExistingGroup( panel );

    final Label chooserLabel = new Label( panel, SWT.NONE );
    chooserLabel.setText( "Bitte w�hlen Sie aus, was Sie tun m�chten:" );

    m_chooserViewer = new ComboViewer( panel, SWT.READ_ONLY | SWT.DROP_DOWN );
    m_chooserViewer.getControl().setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );

    m_chooserViewer.setContentProvider( new ArrayContentProvider() );
    m_chooserViewer.setInput( m_choices );

    final Group choiceGroup = new Group( panel, SWT.NONE );
    choiceGroup.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    final StackLayout choiceLayout = new StackLayout();
    choiceGroup.setLayout( choiceLayout );

    for( final Iterator iter = m_choices.iterator(); iter.hasNext(); )
      ( (IAddCalcCaseChoice)iter.next() ).createControl( choiceGroup );

    m_chooserViewer.addPostSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final ISelection selection = event.getSelection();
        if( !selection.isEmpty() )
        {
          final IAddCalcCaseChoice choice = getChoosen( selection );
          setChoice( choice );
          choiceLayout.topControl = choice.getControl();
          choiceGroup.setText( choice.toString() );

          choiceGroup.layout( true );
        }
      }

    } );

    setControl( panel );

    if( m_choices.size() > 0 )
      m_chooserViewer.setSelection( new StructuredSelection( m_choices.get( 0 ) ) );
  }

  protected void setChoice( final IAddCalcCaseChoice choice )
  {
    if( m_choice != choice )
    {
      m_choice = choice;

      m_choice.validateChoice();

      fireChoiceChanged( choice );
    }
  }

  public IFolder getCurrentCalcCase()
  {
    return m_currentCalcCase;
  }

  protected IAddCalcCaseChoice getChoosen( final ISelection selection )
  {
    return (IAddCalcCaseChoice)( (IStructuredSelection)selection ).getFirstElement();
  }

  private void createExistingGroup( final Composite parent )
  {
    final Group existingGroup = new Group( parent, SWT.NONE );
    existingGroup.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    existingGroup.setLayout( new GridLayout() );
    existingGroup.setText( "In dieser Sitzung durchgef�hrte Vorhersagen" );

    m_caseViewer = new ListViewer( existingGroup );
    final GridData viewerData = new GridData();
    viewerData.grabExcessHorizontalSpace = true;
    viewerData.grabExcessVerticalSpace = true;
    viewerData.horizontalAlignment = GridData.FILL;
    viewerData.verticalAlignment = GridData.FILL;
    m_caseViewer.getControl().setLayoutData( viewerData );

    m_caseViewer.setContentProvider( new ArrayContentProvider() );

    m_caseViewer.setInput( m_calcCases );
    m_caseViewer.setLabelProvider( new WorkbenchLabelProvider() );
  }

  public void doNext( final IProgressMonitor monitor ) throws CoreException
  {
    final IFolder folder = m_choice.perform( monitor );
    m_calcCases.add( folder );
    m_currentCalcCase = folder;

    final ListViewer viewer = m_caseViewer;
    viewer.getControl().getDisplay().asyncExec( new Runnable()
    {
      public void run()
      {
        viewer.refresh();
      }
    } );
  }

  public void update( final IProgressMonitor monitor ) throws CoreException
  {
    monitor.beginTask( "Seite wird aktualisiert", m_choices.size() );
    for( Iterator iter = m_choices.iterator(); iter.hasNext(); )
    {
      ( (IAddCalcCaseChoice)iter.next() ).refresh( new NullProgressMonitor() );
      monitor.worked( 1 );
    }

    monitor.done();
  }

  public boolean shouldUpdate()
  {
    return m_choice == null ? false : m_choice.shouldUpdate();
  }

  public List getCalcCases()
  {
    return m_calcCases;
  }

  public void addChoiceListener( final IChoiceListener l )
  {
    m_choiceListener.add( l );
  }

  public void removeChoiceListener( final IChoiceListener l )
  {
    m_choiceListener.remove( l );
  }

  public void fireChoiceChanged( final IAddCalcCaseChoice newChoice )
  {
    for( final Iterator iter = m_choiceListener.iterator(); iter.hasNext(); )
      ( (IChoiceListener)iter.next() ).onChoiceChanged( newChoice );
  }

  /**
   * @see org.eclipse.jface.wizard.IWizardPage#getPreviousPage()
   */
  public IWizardPage getPreviousPage()
  {
    return super.getPreviousPage();
  }

  /**
   * @see org.eclipse.jface.wizard.IWizardPage#setPreviousPage(org.eclipse.jface.wizard.IWizardPage)
   */
  public void setPreviousPage( IWizardPage page )
  {
    super.setPreviousPage( page );
  }
}