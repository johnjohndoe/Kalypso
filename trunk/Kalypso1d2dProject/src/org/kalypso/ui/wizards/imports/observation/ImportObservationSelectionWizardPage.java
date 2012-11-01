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
package org.kalypso.ui.wizards.imports.observation;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.TimeZone;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.ui.forms.MessageProvider;
import org.kalypso.core.KalypsoCoreExtensions;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.ogc.sensor.adapter.INativeObservationAdapter;
import org.kalypso.zml.ui.imports.ObservationImportSelection;
import org.kalypso.zml.ui.imports.TimezoneEtcFilter;

/**
 * FIXME: this is a stupid copy/paste from the original ImportObservationSelectionWizardPage, however it does almost the
 * same thing -> we need to combine the two pages again!
 * 
 * @author doemming
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class ImportObservationSelectionWizardPage extends WizardPage implements FocusListener, ISelectionProvider, ISelectionChangedListener
{
  protected IFolder m_targetFolder;

  private static final String DEFAUL_FILE_LABEL = ""; //$NON-NLS-1$

  private final INativeObservationAdapter[] m_adapter;

  final List<ISelectionChangedListener> m_selectionListener = new ArrayList<>();

  protected Text m_textFileSource;

  private Button m_buttonRetainMeta;

  private Button m_buttonAppend;

  private ComboViewer m_formatCombo;

  private IFile m_targetFile = null;

  File m_sourceFile = null;

  private boolean m_controlFinished = false;

  private TimeZone m_timezone;

  public ImportObservationSelectionWizardPage( final String pageName, final IFolder targetFolder )
  {
    this( pageName, null, null );

    m_targetFolder = targetFolder;
  }

  public ImportObservationSelectionWizardPage( final String pageName, final String title, final ImageDescriptor titleImage )
  {
    super( pageName, title, titleImage );

    setDescription( Messages.getString( "org.kalypso.ui.wizards.imports.observation.ImportObservationSelectionWizardPage.Description" ) ); //$NON-NLS-1$
    setTitle( Messages.getString( "org.kalypso.ui.wizards.imports.observation.ImportObservationSelectionWizardPage.Title" ) ); //$NON-NLS-1$
    setPageComplete( false );

    m_adapter = KalypsoCoreExtensions.createNativeAdaptersOldStyle();
  }

  @Override
  public void createControl( final Composite parent )
  {
    initializeDialogUnits( parent );

    final Composite topLevel = new Composite( parent, SWT.NONE );
    topLevel.setLayout( new GridLayout( 1, false ) );
    setControl( topLevel );

    createControlSource( topLevel );
    createControlTarget( topLevel );

    m_controlFinished = true;
  }

  public void createControlSource( final Composite parent )
  {
    final Group group = new Group( parent, SWT.NONE );
    group.setText( Messages.getString( "org.kalypso.ui.wizards.imports.observation.ImportObservationSelectionWizardPage.5" ) ); //$NON-NLS-1$

    final GridData groupData = new GridData( SWT.FILL, SWT.FILL, true, false );
    group.setLayoutData( groupData );

    final GridLayout gridLayout = new GridLayout();
    gridLayout.numColumns = 3;
    group.setLayout( gridLayout );

    /* file selection */
    final Label labelFilePath = new Label( group, SWT.READ_ONLY );
    labelFilePath.setText( Messages.getString( "org.kalypso.ui.wizards.imports.observation.ImportObservationSelectionWizardPage.6" ) ); //$NON-NLS-1$

    m_textFileSource = new Text( group, SWT.READ_ONLY | SWT.BORDER );
    m_textFileSource.setText( DEFAUL_FILE_LABEL );
    m_textFileSource.addFocusListener( this );

    final GridData textData = new GridData( SWT.FILL, SWT.FILL, true, false );
    m_textFileSource.setLayoutData( textData );

    /* Choose file button */
    final Button chooseFileButton = new Button( group, SWT.PUSH );
    chooseFileButton.setText( Messages.getString( "org.kalypso.ui.wizards.imports.observation.ImportObservationSelectionWizardPage.7" ) ); //$NON-NLS-1$
    final GridData chooseFileButtonGridData = new GridData();
    chooseFileButtonGridData.horizontalAlignment = GridData.END;
    chooseFileButton.setLayoutData( chooseFileButtonGridData );

    chooseFileButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        m_sourceFile = chooseFile( m_sourceFile );
        if( m_sourceFile != null )
        {
          m_textFileSource.setText( m_sourceFile.getPath() );
          validate();
        }
      }
    } );

    final Label formatLabel = new Label( group, SWT.NONE );
    formatLabel.setText( Messages.getString( "org.kalypso.ui.wizards.imports.observation.ImportObservationSelectionWizardPage.10" ) ); //$NON-NLS-1$

    m_formatCombo = new ComboViewer( group, SWT.READ_ONLY | SWT.DROP_DOWN );

    final GridData formatData = new GridData( SWT.FILL, SWT.FILL, true, false );
    m_formatCombo.getControl().setLayoutData( formatData );

    m_formatCombo.add( m_adapter );

    m_formatCombo.setContentProvider( new ArrayContentProvider() );

    m_formatCombo.setLabelProvider( new ILabelProvider()
    {
      @Override
      public Image getImage( final Object element )
      {
        return null;
      }

      @Override
      public String getText( final Object element )
      {
        return element.toString();
      }

      @Override
      public void addListener( final ILabelProviderListener listener )
      {
        // nothing as labelprovider will not change
      }

      @Override
      public void dispose( )
      {
        // nothing as labelprovider will not change
      }

      @Override
      public boolean isLabelProperty( final Object element, final String property )
      {
        return true;
      }

      @Override
      public void removeListener( final ILabelProviderListener listener )
      {
        // nothing
      }
    } );

    m_formatCombo.setInput( m_adapter );
    m_formatCombo.addSelectionChangedListener( this );

    if( m_adapter.length > 0 )
      m_formatCombo.setSelection( new StructuredSelection( m_adapter[0] ) );

    // just a placeholder
    new Label( group, SWT.NONE );

    /* time zone selection */
    final Label timezoneLabel = new Label( group, SWT.NONE );
    timezoneLabel.setText( Messages.getString( "org.kalypso.ui.wizards.imports.observation.ImportObservationSelectionWizardPage.0" ) ); //$NON-NLS-1$

    final String[] tz = TimeZone.getAvailableIDs();
    Arrays.sort( tz );

    final ComboViewer comboTimeZones = new ComboViewer( group, SWT.BORDER | SWT.SINGLE );
    comboTimeZones.getControl().setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    m_timezone = KalypsoCorePlugin.getDefault().getTimeZone();

    comboTimeZones.setContentProvider( new ArrayContentProvider() );
    comboTimeZones.setLabelProvider( new LabelProvider() );
    comboTimeZones.setInput( tz );

    comboTimeZones.addFilter( new TimezoneEtcFilter() );

    comboTimeZones.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection)comboTimeZones.getSelection();
        updateTimeZone( (String)selection.getFirstElement() );
      }
    } );

    comboTimeZones.getCombo().addModifyListener( new ModifyListener()
    {
      @Override
      public void modifyText( final ModifyEvent e )
      {
        updateTimeZone( comboTimeZones.getCombo().getText() );
      }
    } );

    if( m_timezone != null )
    {
      final String id = m_timezone.getID();
      if( ArrayUtils.contains( tz, id ) )
        comboTimeZones.setSelection( new StructuredSelection( id ) );
      else
        comboTimeZones.getCombo().setText( id );
    }

    // just a placeholder
    new Label( group, SWT.NONE );
  }

  /**
   * resolves the type value of selected observation adapter.
   */
  private String getInputTypeFromSelection( )
  {
    final ISelection lSelection = m_formatCombo.getSelection();
    final INativeObservationAdapter lNativeObservationAdapter = (INativeObservationAdapter)((StructuredSelection)lSelection).getFirstElement();
    return lNativeObservationAdapter.getAxisTypeValue();
  }

  protected void updateTimeZone( final String timeZoneID )
  {
    m_timezone = null;

    if( timeZoneID != null )
    {
      final TimeZone timeZone = TimeZone.getTimeZone( timeZoneID.toUpperCase() );
      // Only set, if timezone could be parsed
      if( !timeZone.getID().equals( "GMT" ) || timeZoneID.toUpperCase().equals( "GMT" ) ) //$NON-NLS-1$ //$NON-NLS-2$
        m_timezone = timeZone;
    }

    validate();
  }

  public void createControlTarget( final Composite parent )
  {
    final Group group = new Group( parent, SWT.NONE );
    group.setText( Messages.getString( "org.kalypso.ui.wizards.imports.observation.ImportObservationSelectionWizardPage.11" ) ); //$NON-NLS-1$
    final GridLayout gridLayout3 = new GridLayout();
    group.setLayout( gridLayout3 );
    final GridData data4 = new GridData();
    data4.horizontalAlignment = GridData.FILL;
    data4.grabExcessHorizontalSpace = true;
    group.setLayoutData( data4 );

    final Composite top = new Composite( group, SWT.NONE );
    final GridData data = new GridData();
    data.horizontalAlignment = GridData.FILL;
    data.grabExcessHorizontalSpace = true;
    top.setLayoutData( data );

    final GridLayout gridLayout = new GridLayout();
    gridLayout.numColumns = 3;
    top.setLayout( gridLayout );

    final Composite bottom = new Composite( group, SWT.NONE );
    final GridData data3 = new GridData();
    data3.horizontalAlignment = GridData.FILL;
    data3.grabExcessHorizontalSpace = true;
    bottom.setLayoutData( data3 );

    final GridLayout gridLayout2 = new GridLayout();

    bottom.setLayout( gridLayout2 );

    m_buttonRetainMeta = new Button( bottom, SWT.CHECK );
    m_buttonRetainMeta.setText( Messages.getString( "org.kalypso.ui.wizards.imports.observation.ImportObservationSelectionWizardPage.12" ) ); //$NON-NLS-1$
    m_buttonRetainMeta.setSelection( true );
    m_buttonAppend = new Button( bottom, SWT.CHECK );
    m_buttonAppend.setText( Messages.getString( "org.kalypso.ui.wizards.imports.observation.ImportObservationSelectionWizardPage.13" ) ); //$NON-NLS-1$
    m_buttonAppend.setSelection( true );
  }

  File chooseFile( final File selectedFile )
  {
    final FileDialog dialog = new FileDialog( getShell(), SWT.SINGLE );
    if( selectedFile != null )
    {
      dialog.setFileName( selectedFile.getName() );
      dialog.setFilterPath( selectedFile.getParent() );
    }
    dialog.open();
    final String fileName = dialog.getFileName();
    if( fileName != null && fileName.length() > 0 )
    {
      final String filterPath = dialog.getFilterPath();
      return new File( filterPath, fileName );
    }
    else
      return null;
  }

  /**
   * validates the page
   */
  void validate( )
  {
    // TODO: does not belong here, strange!
    if( m_sourceFile != null && m_sourceFile.isFile() )
      m_textFileSource.setText( m_sourceFile.getPath() );

    final IMessageProvider message = doValidate();
    if( message == null )
      setMessage( null );
    else
      setMessage( message.getMessage(), message.getMessageType() );

    setPageComplete( message == null );

    // TODO: does not belong here, strange
    fireSelectionChanged();
  }

  private IMessageProvider doValidate( )
  {
    if( m_sourceFile == null || !m_sourceFile.isFile() )
      return new MessageProvider( Messages.getString( "org.kalypso.ui.wizards.imports.observation.ImportObservationSelectionWizardPage.14" ), ERROR ); //$NON-NLS-1$

    // setMessage( Messages.getString( "org.kalypso.ui.wizards.imports.observation.ImportObservationSelectionWizardPage.15" ) ); //$NON-NLS-1$

    if( m_timezone == null )
      return new MessageProvider( Messages.getString( "ImportObservationSelectionWizardPage.0" ), ERROR ); //$NON-NLS-1$

    return null;
  }

  /**
   * @see org.eclipse.swt.events.FocusListener#focusGained(org.eclipse.swt.events.FocusEvent)
   */
  @Override
  public void focusGained( final FocusEvent e )
  {
    // nothing
  }

  /**
   * @see org.eclipse.swt.events.FocusListener#focusLost(org.eclipse.swt.events.FocusEvent)
   */
  @Override
  public void focusLost( final FocusEvent e )
  {
    if( m_sourceFile != null && !m_sourceFile.getName().equals( m_textFileSource.getText() ) )
    {
      m_sourceFile = new File( m_textFileSource.getText() );
    }
    validate();
  }

  private void fireSelectionChanged( )
  {
    if( m_sourceFile == null )
    {
      return;
    }
    for( final ISelectionChangedListener iSelectionChangedListener : m_selectionListener )
      iSelectionChangedListener.selectionChanged( new SelectionChangedEvent( this, getSelection() ) );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#addSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  @Override
  public void addSelectionChangedListener( final ISelectionChangedListener listener )
  {
    m_selectionListener.add( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#getSelection()
   */
  @Override
  public ISelection getSelection( )
  {
    final IStructuredSelection formatSelection = (IStructuredSelection)m_formatCombo.getSelection();
    if( !m_controlFinished )
      return new ISelection()
      {
        @Override
        public boolean isEmpty( )
        {
          return true;
        }
      };

    if( m_sourceFile == null )
      return StructuredSelection.EMPTY;

    final String sourceName = FileUtilities.nameWithoutExtension( m_sourceFile.getName() );

    m_targetFile = m_targetFolder.getFile( sourceName + "." + getInputTypeFromSelection() + ".zml" ); //$NON-NLS-1$ //$NON-NLS-2$
    return new ObservationImportSelection( m_sourceFile, m_targetFile, (INativeObservationAdapter)formatSelection.getFirstElement(), m_buttonAppend.getSelection(), m_buttonRetainMeta.getSelection(), m_timezone );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#removeSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  @Override
  public void removeSelectionChangedListener( final ISelectionChangedListener listener )
  {
    m_selectionListener.remove( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#setSelection(org.eclipse.jface.viewers.ISelection)
   */
  @Override
  public void setSelection( final ISelection selection )
  {
    if( selection instanceof ObservationImportSelection )
    {
      final ObservationImportSelection s = (ObservationImportSelection)selection;
      if( m_formatCombo != null )
        m_formatCombo.setSelection( new StructuredSelection( s.getNativeAdapter() ) );
      m_sourceFile = s.getFileSource();
      m_targetFile = s.getFileTarget();
      if( m_buttonAppend != null )
        m_buttonAppend.setSelection( s.isAppend() );
      if( m_buttonRetainMeta != null )
        m_buttonRetainMeta.setSelection( s.isRetainMetadata() );
    }
    else if( selection instanceof IStructuredSelection )
    {
      final Object firstElement = ((StructuredSelection)selection).getFirstElement();
      if( firstElement instanceof IFile )
        m_targetFile = (IFile)firstElement;
    }
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
   */
  @Override
  public void selectionChanged( final SelectionChangedEvent event )
  {
    fireSelectionChanged();
  }

}