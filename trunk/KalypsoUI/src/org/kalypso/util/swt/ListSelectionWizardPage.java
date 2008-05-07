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
package org.kalypso.util.swt;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.ui.internal.WorkbenchMessages;

/**
 * A wizard page that lets the user select from a list.<br>
 * TODO: add an validation mechanism for the current selection
 * 
 * @author Gernot Belger
 */
@SuppressWarnings("restriction") //$NON-NLS-1$
public class ListSelectionWizardPage extends WizardPage
{
  private Object[] m_selection;

  private CheckboxTableViewer m_viewer;

  private StatusComposite m_statusLabel;

  private final IStructuredContentProvider m_contentProvider;

  private final ILabelProvider m_labelProvider;

  private Object m_input;

  private boolean m_allowNextIfEmpty;

  public ListSelectionWizardPage( final String pageName, final IStructuredContentProvider contentProvider, final ILabelProvider labelProvider )
  {
    super( pageName );

    m_contentProvider = contentProvider;
    m_labelProvider = labelProvider;
  }

  public ListSelectionWizardPage( final String pageName, final String title, final ImageDescriptor titleImage, final IStructuredContentProvider contentProvider, final ILabelProvider labelProvider )
  {
    super( pageName, title, titleImage );

    m_contentProvider = contentProvider;
    m_labelProvider = labelProvider;
  }

  /**
   * Set, if the next button should be allowed an an empty selection.
   */
  public void setAllowNextIfEmpty( final boolean allowNextIfEmpty )
  {
    m_allowNextIfEmpty = allowNextIfEmpty;
  }

  /**
   * @see org.eclipse.jface.wizard.WizardPage#canFlipToNextPage()
   */
  @Override
  public boolean canFlipToNextPage( )
  {
    if( m_allowNextIfEmpty )
      return true;

    return m_selection != null && m_selection.length > 0;
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    final Composite composite = new Composite( parent, SWT.NONE );
    composite.setLayout( new GridLayout() );

    m_statusLabel = new StatusComposite( composite, SWT.NONE );
    final GridData statusData = new GridData( SWT.FILL, SWT.TOP, true, false );
    statusData.exclude = true;
    m_statusLabel.setLayoutData( statusData );
    m_statusLabel.setVisible( false );

    new Label( composite, SWT.NONE );

    final Table table = new Table( composite, SWT.BORDER | SWT.CHECK );
    final GridData tableGridData = new GridData( SWT.FILL, SWT.FILL, true, true );
    tableGridData.heightHint = 200;

    table.setLayoutData( tableGridData );
    m_viewer = new CheckboxTableViewer( table );

    m_viewer.setContentProvider( m_contentProvider );
    m_viewer.setLabelProvider( m_labelProvider );
    m_viewer.setInput( m_input );
    if( m_selection != null )
      m_viewer.setCheckedElements( m_selection );

    m_viewer.addCheckStateListener( new ICheckStateListener()
    {
      public void checkStateChanged( final CheckStateChangedEvent event )
      {
        updateSelection();
      }
    } );

    addSelectionButtons( m_viewer, composite );

    setControl( composite );
  }

  protected void updateSelection( )
  {
    m_selection = m_viewer.getCheckedElements();

    getContainer().updateButtons();
  }

  public void setCheckedElements( final Object[] elements )
  {
    if( m_viewer == null )
      m_selection = elements;
    else
    {
      m_viewer.setCheckedElements( elements );
      updateSelection();
    }
  }

  public void setInput( final Object input )
  {
    if( m_viewer == null )
      m_input = input;
    else
    {
      m_viewer.setInput( input );
      updateSelection();
    }
  }

  public void setStatus( final IStatus status )
  {
    // REMARK: we hide the status completely, if the status is null.
    // Makes layout necessary if status changed from/to null.
    final IStatus currentStatus = m_statusLabel.getStatus();
    final boolean layoutNeeded = (status == null && currentStatus != null) || (status != null && currentStatus == null);

    m_statusLabel.setStatus( status );

    m_statusLabel.setVisible( status != null );
    ((GridData) m_statusLabel.getLayoutData()).exclude = status == null;

    if( layoutNeeded )
      m_statusLabel.getParent().layout();
  }

  /**
   * Add the selection and deselection buttons to the page.
   */
  private void addSelectionButtons( final CheckboxTableViewer checkboxViewer, final Composite composite )
  {
    final Composite buttonComposite = new Composite( composite, SWT.NONE );
    final GridLayout layout = new GridLayout();
    layout.numColumns = 0;
    layout.marginWidth = 0;
    layout.horizontalSpacing = convertHorizontalDLUsToPixels( IDialogConstants.HORIZONTAL_SPACING );
    buttonComposite.setLayout( layout );
    buttonComposite.setLayoutData( new GridData( SWT.END, SWT.TOP, true, false ) );

    final String SELECT_ALL_TITLE = WorkbenchMessages.SelectionDialog_selectLabel;
    final String DESELECT_ALL_TITLE = WorkbenchMessages.SelectionDialog_deselectLabel;

    final Button selectButton = createButton( buttonComposite, IDialogConstants.SELECT_ALL_ID, SELECT_ALL_TITLE, false );

    selectButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        checkboxViewer.setAllChecked( true );
        updateSelection();
      }
    } );

    final Button deselectButton = createButton( buttonComposite, IDialogConstants.DESELECT_ALL_ID, DESELECT_ALL_TITLE, false );

    deselectButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        checkboxViewer.setAllChecked( false );
        updateSelection();
      }
    } );
  }

  /**
   * Creates a new button with the given id.
   * <p>
   * The <code>Dialog</code> implementation of this framework method creates a standard push button, registers it for
   * selection events including button presses, and registers default buttons with its shell. The button id is stored as
   * the button's client data. If the button id is <code>IDialogConstants.CANCEL_ID</code>, the new button will be
   * accessible from <code>getCancelButton()</code>. If the button id is <code>IDialogConstants.OK_ID</code>, the
   * new button will be accessible from <code>getOKButton()</code>. Note that the parent's layout is assumed to be a
   * <code>GridLayout</code> and the number of columns in this layout is incremented. Subclasses may override.
   * </p>
   * 
   * @param parent
   *            the parent composite
   * @param id
   *            the id of the button (see <code>IDialogConstants.*_ID</code> constants for standard dialog button ids)
   * @param label
   *            the label from the button
   * @param defaultButton
   *            <code>true</code> if the button is to be the default button, and <code>false</code> otherwise
   * @return the new button
   * @see #getCancelButton
   * @see #getOKButton()
   */
  protected Button createButton( final Composite parent, final int id, final String label, final boolean defaultButton )
  {
    // increment the number of columns in the button bar
    ((GridLayout) parent.getLayout()).numColumns++;
    final Button button = new Button( parent, SWT.PUSH );
    button.setText( label );
    button.setFont( JFaceResources.getDialogFont() );
    button.setData( new Integer( id ) );
    if( defaultButton )
    {
      final Shell shell = parent.getShell();
      if( shell != null )
      {
        shell.setDefaultButton( button );
      }
    }
    setButtonLayoutData( button );
    return button;
  }

  public Object[] getSelection( )
  {
    return m_selection;
  }
}
