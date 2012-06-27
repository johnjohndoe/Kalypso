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
package org.kalypso.model.km.internal.ui.kmupdate;

import java.math.BigDecimal;
import java.util.List;

import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.wizard.IWizardContainer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.internal.WorkbenchMessages;
import org.kalypso.contribs.eclipse.jface.viewers.table.ColumnsResizeControlListener;
import org.kalypso.contribs.eclipse.swt.widgets.ColumnViewerSorter;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.model.km.internal.binding.KMChannelElement;
import org.kalypso.model.km.internal.core.ProfileDataSet;
import org.kalypso.model.km.internal.i18n.Messages;

import de.tu_harburg.wb.kalypso.rrm.kalininmiljukov.KalininMiljukovType;
import de.tu_harburg.wb.kalypso.rrm.kalininmiljukov.KalininMiljukovType.Profile;

/**
 * @author Andreas Doemming (original)
 * @author Holger Albert (modified)
 */
@SuppressWarnings("restriction")
public class KMViewer
{
  /**
   * The text field for the label.
   */
  private Text m_labelText;

  /**
   * The widget for selecting a file.
   */
  private DirectoryFieldWidget m_dirField;

  /**
   * The text for the start km.
   */
  private Text m_startText;

  /**
   * The text for the end km.
   */
  private Text m_endText;

  /**
   * The checkbox table viewer, which displays the profiles.
   */
  private CheckboxTableViewer m_profileListViewer;

  /**
   * The input.
   */
  private KMChannelElement m_input;

  private final IWizardContainer m_context;

  private Button m_selectAllButton;

  private Button m_deselectAllButton;

  private final KMProfileStationFilter m_profileFilter = new KMProfileStationFilter();

  public KMViewer( final IWizardContainer context )
  {
    m_context = context;
  }

  /**
   * This function creates the controls.
   * 
   * @param parent
   *          The parent composite.
   */
  public void createControls( final Composite parent )
  {
    /* Set a layout to the parent. */
    parent.setLayout( new GridLayout( 3, false ) );

    /* Create a empty label. */
    final Label emptyLabel = new Label( parent, SWT.NONE );
    emptyLabel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, false, false ) );

    /* Create a text. */
    m_labelText = new Text( parent, SWT.BORDER | SWT.READ_ONLY );
    m_labelText.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    /* Create a empty label. */
    final Label emptyLabel1 = new Label( parent, SWT.NONE );
    emptyLabel1.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, false, false ) );

    /* Create a widget for asking for a file. */
    m_dirField = new DirectoryFieldWidget( parent, m_context ); //$NON-NLS-1$ //$NON-NLS-2$
    m_dirField.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        handleDirectoryFieldSelected( (IStructuredSelection) event.getSelection() );
      }
    } );

    /* Create a label. */
    final Label startLabel = new Label( parent, SWT.NONE );
    startLabel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, false, false ) );
    startLabel.setText( Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMViewer.2" ) ); //$NON-NLS-1$

    /* Create a text. */
    m_startText = new Text( parent, SWT.BORDER );
    m_startText.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    /* Create a empty label. */
    final Label emptyLabel2 = new Label( parent, SWT.NONE );
    emptyLabel2.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, false, false ) );

    /* Create a label. */
    final Label endLabel = new Label( parent, SWT.NONE );
    endLabel.setText( Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMViewer.3" ) ); //$NON-NLS-1$
    endLabel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, false, false ) );

    /* Create a text. */
    m_endText = new Text( parent, SWT.BORDER );
    m_endText.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    /* Create a empty label. */
    final Label emptyLabel3 = new Label( parent, SWT.NONE );
    emptyLabel3.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, false, false ) );

    /* Create a label. */
    final Label profilesLabel = new Label( parent, SWT.NONE );
    profilesLabel.setLayoutData( new GridData( SWT.BEGINNING, SWT.BEGINNING, false, false ) );
    profilesLabel.setText( Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMViewer.4" ) ); //$NON-NLS-1$

    /* Create a checkbox table viewer. */
    m_profileListViewer = CheckboxTableViewer.newCheckList( parent, SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER | SWT.HIDE_SELECTION | SWT.FULL_SELECTION );
    final Table table = m_profileListViewer.getTable();
    table.setHeaderVisible( true );
    m_profileListViewer.getControl().setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true, 2, 1 ) );
    m_profileListViewer.setContentProvider( new KMViewerContentProvider() );
    m_profileListViewer.setCheckStateProvider( new KMViewerCheckStateProvider( this ) );
    m_profileListViewer.addFilter( m_profileFilter );

    /* Create a table viewer column. */
    final TableViewerColumn stationViewerColumn = new TableViewerColumn( m_profileListViewer, SWT.LEFT );
    stationViewerColumn.setLabelProvider( new ProfileStationLabelProvider() );
    final TableColumn stationColumn = stationViewerColumn.getColumn();
    stationColumn.setText( Messages.getString( "KMViewer_1" ) ); //$NON-NLS-1$
    stationColumn.setResizable( false );
    ColumnsResizeControlListener.setMinimumPackWidth( stationColumn );
    ColumnViewerSorter.registerSorter( stationViewerColumn, new ProfileStationSorter() );

    /* Create a table viewer column. */
    final TableViewerColumn validViewerColumn = new TableViewerColumn( m_profileListViewer, SWT.LEFT );
    validViewerColumn.setLabelProvider( new ProfileValidLabelProvider( this ) );
    final TableColumn validColumn = validViewerColumn.getColumn();
    validColumn.setText( Messages.getString( "KMViewer_2" ) ); //$NON-NLS-1$
    stationColumn.setResizable( false );
    ColumnsResizeControlListener.setMinimumPackWidth( validColumn );

    /* Make sure, the columns are properly resized. */
    table.addControlListener( new ColumnsResizeControlListener() );

    m_profileListViewer.addCheckStateListener( new ICheckStateListener()
    {
      @Override
      public void checkStateChanged( final CheckStateChangedEvent event )
      {
        final Profile profile = (Profile) event.getElement();
        profile.setEnabled( event.getChecked() );
      }
    } );

    createSelectButtons( parent );

    /* Add a listener. */
    m_startText.addFocusListener( new FocusAdapter()
    {
      /**
       * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
       */
      @Override
      public void focusLost( final FocusEvent e )
      {
        handleKMStartModified();
      }
    } );

    /* Add a listener. */
    m_endText.addFocusListener( new FocusAdapter()
    {
      @Override
      public void focusLost( final FocusEvent e )
      {
        handleKMEndModified();
      }
    } );

    updateControls();
  }

  private void createSelectButtons( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    GridLayoutFactory.fillDefaults().numColumns( 2 ).applyTo( panel );
    panel.setLayoutData( new GridData( SWT.RIGHT, SWT.CENTER, true, false, 3, 1 ) );

    m_selectAllButton = new Button( panel, SWT.PUSH );
    m_selectAllButton.setText( WorkbenchMessages.SelectionDialog_selectLabel );
    m_selectAllButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final org.eclipse.swt.events.SelectionEvent e )
      {
        updateCheckedProfiles( true );
      }
    } );

    m_deselectAllButton = new Button( panel, SWT.PUSH );
    m_deselectAllButton.setText( WorkbenchMessages.SelectionDialog_deselectLabel );
    m_deselectAllButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final org.eclipse.swt.events.SelectionEvent e )
      {
        updateCheckedProfiles( false );
      }
    } );
  }

  protected void updateCheckedProfiles( final boolean enabled )
  {
    if( m_input == null )
      return;

    if( enabled )
      m_input.enableAllProfiles();
    else
    {
      final List<Profile> profiles = m_input.getKMType().getProfile();
      for( final Profile profile : profiles )
        profile.setEnabled( false );
    }

    m_profileListViewer.refresh();
  }

  protected void handleKMStartModified( )
  {
    try
    {
      final KMChannelElement input = getInput();
      if( input != null )
      {
        final BigDecimal km = NumberUtils.parseQuietDecimal( m_startText.getText() );
        input.getKMType().setKmStart( km );
      }

      updateControls();
    }
    catch( final Exception ex )
    {
      ex.printStackTrace();
    }
  }

  protected void handleKMEndModified( )
  {
    try
    {
      final KMChannelElement input = getInput();
      if( input != null )
      {
        final BigDecimal km = NumberUtils.parseQuietDecimal( m_endText.getText() );
        input.getKMType().setKmEnd( km );
      }

      updateControls();
    }
    catch( final Exception ex )
    {
      ex.printStackTrace();
    }
  }

  protected void handleDirectoryFieldSelected( final IStructuredSelection selection )
  {
    final String path = (String) selection.getFirstElement();

    final KMChannelElement input = getInput();
    if( input == null )
      return;

    final String oldFile = input.getKMType().getFile();
    if( ObjectUtils.equals( path, oldFile ) )
      return;

    input.getKMType().setFile( path );
    input.loadData( m_context );
    /* If we choose a new file, we will enable all */
    input.enableAllProfiles();

    final ProfileDataSet profileSet = input.getProfileSet();
    if( profileSet != null )
    {
      final KalininMiljukovType kmType = input.getKMType();
      final BigDecimal oldKmStart = kmType.getKmStart();
      if( oldKmStart == null )
        kmType.setKmStart( profileSet.getStartStation() );

      final BigDecimal oldKmEnd = kmType.getKmEnd();
      if( oldKmEnd == null )
        kmType.setKmEnd( profileSet.getEndStation() );
    }

    updateControls();
  }

  private KMChannelElement getInput( )
  {
    return m_input;
  }

  private void updateControls( )
  {
    final KMChannelElement input = getInput();
    if( input == null )
    {
      m_labelText.setText( Messages.getString( "KMViewer_3" ) ); //$NON-NLS-1$
      m_dirField.setSelection( StructuredSelection.EMPTY );
      m_dirField.setEnabled( false );
      m_startText.setText( "" ); //$NON-NLS-1$
      m_startText.setEnabled( false );
      m_endText.setText( "" ); //$NON-NLS-1$
      m_endText.setEnabled( false );
      m_profileListViewer.getControl().setEnabled( false );
      m_selectAllButton.setEnabled( false );
      m_deselectAllButton.setEnabled( false );
    }
    else
    {
      final KalininMiljukovType kmType = input.getKMType();
      final BigDecimal kmStart = kmType.getKmStart();
      final BigDecimal kmEnd = kmType.getKmEnd();
      final String path = kmType.getFile();

      m_dirField.setSelection( new StructuredSelection( path ) );

      if( kmStart != null )
        m_startText.setText( String.format( "%.4f", kmStart ) ); //$NON-NLS-1$
      else
        m_startText.setText( "" ); //$NON-NLS-1$

      if( kmEnd != null )
        m_endText.setText( String.format( "%.4f", kmEnd ) ); //$NON-NLS-1$
      else
        m_endText.setText( "" ); //$NON-NLS-1$

      m_dirField.setEnabled( true );
      m_startText.setEnabled( !StringUtils.isBlank( path ) );
      m_endText.setEnabled( !StringUtils.isBlank( path ) );
      final boolean enableList = !StringUtils.isBlank( path );
      m_profileListViewer.getControl().setEnabled( enableList );
      m_selectAllButton.setEnabled( enableList );
      m_deselectAllButton.setEnabled( enableList );
    }

    m_profileListViewer.refresh();

    ColumnsResizeControlListener.refreshColumnsWidth( m_profileListViewer.getTable() );
  }

  private void inputChanged( final KMChannelElement element )
  {
    m_input = element;

    if( m_profileListViewer != null )
    {
      m_profileFilter.setInput( element );
      m_profileListViewer.setInput( element );
    }

    updateControls();
  }

  public void setInput( final String label, final KMChannelElement element, final boolean force )
  {
    m_labelText.setText( label );

    final KMChannelElement oldInput = getInput();
    if( oldInput == element && !force )
      return;

    inputChanged( element );
  }

  public String getValidMessage( final Profile profile )
  {
    if( m_input == null )
      return StringUtils.EMPTY;

    return m_input.getValidMessage( profile );
  }

  public boolean isValid( final Profile profile )
  {
    if( m_input == null )
      return false;

    return m_input.isValid( profile );
  }
}