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
package org.kalypso.model.wspm.tuhh.ui.wizards;

import java.util.Arrays;

import org.eclipse.core.databinding.observable.set.WritableSet;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.databinding.viewers.IViewerObservableSet;
import org.eclipse.jface.databinding.viewers.ViewersObservables;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.ui.internal.WorkbenchMessages;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.kalypso.commons.databinding.DataSetBinder;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.model.wspm.tuhh.core.wspwin.WspWinExportProjectData;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;

/**
 * @author thuel2
 */
@SuppressWarnings("restriction")
public class WspWinExportProjectSelectionPage extends WizardPage
{
  private Button m_selectAllButton;

  private Button m_deselectAllButton;

  private final WspWinExportProjectData m_data;

  private DatabindingWizardPage m_binding;

  /**
   * Creates an instance of this class
   * 
   * @param aWorkbench
   *          IWorkbench
   */
  protected WspWinExportProjectSelectionPage( final String pageName, final WspWinExportProjectData data )
  {
    super( pageName );

    m_data = data;

    setTitle( Messages.getString( "WspWinExportProjectSelectionPage_0" ) ); //$NON-NLS-1$
    setDescription( Messages.getString( "WspWinExportProjectSelectionPage_1" ) ); //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createControl( final Composite parent )
  {
    initializeDialogUnits( parent );

    final Composite composite = new Composite( parent, SWT.NULL );
    composite.setLayout( new GridLayout() );
    composite.setFont( parent.getFont() );
    setControl( composite );

    m_binding = new DatabindingWizardPage( this, null );

    createResourcesGroup( composite );
    createButtonsGroup( composite );
  }

  /**
   * Creates the checkbox tree and list for selecting resources.
   * 
   * @param parent
   *          the parent control
   */
  protected final void createResourcesGroup( final Composite parent )
  {
    final Table table = new Table( parent, SWT.BORDER | SWT.CHECK );
    final CheckboxTableViewer viewer = new CheckboxTableViewer( table );
    viewer.getControl().setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    viewer.setContentProvider( new ArrayContentProvider() );
    viewer.setLabelProvider( new WorkbenchLabelProvider() );
    viewer.setComparator( new ViewerComparator() );

    viewer.setInput( m_data.getWspmProjects() );

    final IViewerObservableSet target = ViewersObservables.observeCheckedElements( viewer, IProject.class );
    final WritableSet model = m_data.getSelectedProjectList();
    final DataSetBinder binder = new DataSetBinder( target, model );
    m_binding.bindValue( binder );
  }

  /**
   * // * Creates the buttons for selecting specific types or selecting all or none of the // * elements. // * // *
   * 
   * @param parent
   *          the parent control //
   */
  protected final void createButtonsGroup( final Composite parent )
  {
    // top level group
    final Composite buttonComposite = new Composite( parent, SWT.NONE );
    buttonComposite.setFont( parent.getFont() );

    buttonComposite.setLayout( new GridLayout( 3, true ) );
    buttonComposite.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    m_selectAllButton = createButton( buttonComposite, IDialogConstants.SELECT_ALL_ID, WorkbenchMessages.SelectionDialog_selectLabel, false ); //$NON-NLS-1$

    final WritableSet selectedProjectList = m_data.getSelectedProjectList();
    final IProject[] wspmProjects = m_data.getWspmProjects();

    m_selectAllButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        selectedProjectList.addAll( Arrays.asList( wspmProjects ) );
      }
    } );
    setButtonLayoutData( m_selectAllButton );

    m_deselectAllButton = createButton( buttonComposite, IDialogConstants.DESELECT_ALL_ID, WorkbenchMessages.SelectionDialog_deselectLabel, false ); //$NON-NLS-1$

    m_deselectAllButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        selectedProjectList.clear();
      }
    } );
    setButtonLayoutData( m_deselectAllButton );
  }

  /**
   * Creates a new button with the given id.
   * <p>
   * The <code>Dialog</code> implementation of this framework method creates a standard push button, registers for
   * selection events including button presses and registers default buttons with its shell. The button id is stored as
   * the buttons client data. Note that the parent's layout is assumed to be a GridLayout and the number of columns in
   * this layout is incremented. Subclasses may override.
   * </p>
   * 
   * @param parent
   *          the parent composite
   * @param id
   *          the id of the button (see <code>IDialogConstants.*_ID</code> constants for standard dialog button ids)
   * @param label
   *          the label from the button
   * @param defaultButton
   *          <code>true</code> if the button is to be the default button, and <code>false</code> otherwise
   */
  private Button createButton( final Composite parent, final int id, final String label, final boolean defaultButton )
  {
    // increment the number of columns in the button bar
    ((GridLayout) parent.getLayout()).numColumns++;

    final Button button = new Button( parent, SWT.PUSH );
    button.setFont( parent.getFont() );

    final GridData buttonData = new GridData( GridData.FILL_HORIZONTAL );
    button.setLayoutData( buttonData );

    button.setData( Integer.valueOf( id ) );
    button.setText( label );
    button.setFont( parent.getFont() );

    if( defaultButton )
    {
      final Shell shell = parent.getShell();
      if( shell != null )
      {
        shell.setDefaultButton( button );
      }
      button.setFocus();
    }
    button.setFont( parent.getFont() );
    setButtonLayoutData( button );
    return button;
  }
}
