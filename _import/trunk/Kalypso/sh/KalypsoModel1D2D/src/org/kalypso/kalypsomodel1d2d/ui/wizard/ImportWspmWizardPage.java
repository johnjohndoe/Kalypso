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
package org.kalypso.kalypsomodel1d2d.ui.wizard;

import java.util.List;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
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
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.ui.editor.gmleditor.ui.GMLLabelProvider;
import org.kalypso.util.swt.StatusComposite;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 */
@SuppressWarnings("restriction")
public class ImportWspmWizardPage extends WizardPage
{
  private TuhhCalculation m_calculation;

  private CheckboxTableViewer m_profileViewer;

  private TuhhReachProfileSegment[] m_segments;

  private StatusComposite m_calcLabel;

  public ImportWspmWizardPage( final String pageName )
  {
    super( pageName );

    setTitle( "Profile w‰hlen" );
    setMessage( "W‰hlen Sie auf dieser Seite aus, welche Profile importiert werden sollen." );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    final Composite composite = new Composite( parent, SWT.NONE );
    composite.setLayout( new GridLayout() );

    m_calcLabel = new StatusComposite( composite, SWT.NONE );

    final Table table = new Table( composite, SWT.BORDER | SWT.CHECK );
    table.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    m_profileViewer = new CheckboxTableViewer( table );

    m_profileViewer.setContentProvider( new ArrayContentProvider() );
    m_profileViewer.setLabelProvider( new GMLLabelProvider() );

    m_profileViewer.addCheckStateListener( new ICheckStateListener()
    {
      public void checkStateChanged( final CheckStateChangedEvent event )
      {
        updateSelection();
      }
    } );

    addSelectionButtons( m_profileViewer, composite );

    setControl( composite );
  }

  protected void updateSelection( )
  {
    final Object[] array = m_profileViewer.getCheckedElements();
    m_segments = new TuhhReachProfileSegment[array.length];
    for( int i = 0; i < array.length; i++ )
      m_segments[i] = new TuhhReachProfileSegment( (Feature) array[i] );
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
   * 
   * @return the new button
   * 
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

  public TuhhCalculation getCalculation( )
  {
    return m_calculation;
  }

  public void setCalculation( final TuhhCalculation calculation )
  {
    m_calculation = calculation;

    final List< ? > reaches = (List< ? >) m_calculation.getReach().getWrappedFeature().getProperty( TuhhReach.QNAME_PROP_REACHSEGMENTMEMBER );
    m_profileViewer.setInput( reaches );

    m_profileViewer.setAllChecked( true );
    updateSelection();

    m_calcLabel.setStatus( StatusUtilities.createStatus( IStatus.INFO, "Gew‰hlte Berechnungsvariante: " + calculation.getName(), null ) );
  }

  public TuhhReachProfileSegment[] getReachProfileSegments( )
  {
    return m_segments;
  }

}
