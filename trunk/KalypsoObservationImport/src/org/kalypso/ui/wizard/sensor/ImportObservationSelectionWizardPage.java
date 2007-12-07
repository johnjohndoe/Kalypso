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
package org.kalypso.ui.wizard.sensor;

import java.io.File;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import org.eclipse.compare.internal.ListContentProvider;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
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
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.ogc.sensor.adapter.INativeObservationAdapter;


/**
 * @author doemming
 */
public class ImportObservationSelectionWizardPage extends WizardPage implements FocusListener, ISelectionProvider,
    ISelectionChangedListener
{
  private static final String DEFAUL_FILE_LABEL = "";

  private final List m_adapter;

  final List m_selectionListener = new ArrayList();

  private Composite m_topLevel = null;

  private Text m_textFileSource;

  private Text m_textFileTarget;

  private Button m_buttonRetainMeta;

  private Button m_buttonAppend;

  private ComboViewer m_formatCombo;

  File m_targetFile = null;

  File m_sourceFile = null;

  private boolean m_controlFinished = false;

  public ImportObservationSelectionWizardPage( String pageName )
  {
    this( pageName, null, null );
  }

  public ImportObservationSelectionWizardPage( String pageName, String title, ImageDescriptor titleImage )
  {
    super( pageName, title, titleImage );

    setDescription( "Auswahl Zeitreihe" );
    setTitle( "Auswahl der zu importierenden Zeitreihe" );
    setPageComplete( false );

    m_adapter = createNativeAdapters();
  }

  private List createNativeAdapters()
  {
    final List adapters = new ArrayList();

    final IExtensionRegistry registry = Platform.getExtensionRegistry();

    final IExtensionPoint extensionPoint = registry.getExtensionPoint( "org.kalypso.core.nativeObsAdapter" );

    if( extensionPoint == null )
      return adapters;

    final IExtension[] extensions = extensionPoint.getExtensions();
    for( int i = 0; i < extensions.length; i++ )
    {
      final IExtension extension = extensions[i];
      final IConfigurationElement[] elements = extension.getConfigurationElements();

      for( int j = 0; j < elements.length; j++ )
      {
        final IConfigurationElement element = elements[j];

        try
        {
          final INativeObservationAdapter adapter = (INativeObservationAdapter)element
              .createExecutableExtension( "class" );
          adapters.add( adapter );
        }
        catch( final CoreException e )
        {
          // TODO Auto-generated catch block
          e.printStackTrace();
        }
      }
    }

    return adapters;
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( Composite parent )
  {
    initializeDialogUnits( parent );
    m_topLevel = new Composite( parent, SWT.NONE );

    GridLayout gridLayout = new GridLayout();
    m_topLevel.setLayout( gridLayout );

    GridData data = new GridData();
    data.horizontalAlignment = GridData.FILL;
    data.grabExcessHorizontalSpace = true;
    m_topLevel.setLayoutData( data );

    createControlSource( m_topLevel );
    createControlTarget( m_topLevel );
    setControl( m_topLevel );
    validate();
    m_controlFinished = true;
  }

  public void createControlSource( Composite parent )
  {
    Group group = new Group( parent, SWT.NONE );
    group.setText( "Quelle" );

    GridData data = new GridData();
    data.horizontalAlignment = GridData.FILL;
    data.grabExcessHorizontalSpace = true;
    group.setLayoutData( data );

    GridLayout gridLayout = new GridLayout();
    gridLayout.numColumns = 3;
    group.setLayout( gridLayout );

    //  line 1
    Label label = new Label( group, SWT.NONE );
    label.setText( "von " );

    m_textFileSource = new Text( group, SWT.BORDER );
    m_textFileSource.setText( DEFAUL_FILE_LABEL );
    m_textFileSource.addFocusListener( this );

    GridData data1 = new GridData();
    data1.horizontalAlignment = GridData.FILL;
    data1.grabExcessHorizontalSpace = true;
    m_textFileSource.setLayoutData( data1 );

    Button button = new Button( group, SWT.PUSH );
    button.setText( "Auswahl ..." );
    GridData data2 = new GridData();
    data2.horizontalAlignment = GridData.END;
    button.setLayoutData( data2 );

    button.addSelectionListener( new SelectionAdapter()
    {
      public void widgetSelected( SelectionEvent e )
      {
        m_sourceFile = chooseFile( m_sourceFile );
        validate();
      }
    } );
    // line 2

    Label formatLabel = new Label( group, SWT.NONE );
    formatLabel.setText( "Format " );

    m_formatCombo = new ComboViewer( group, SWT.NONE );
    m_formatCombo.add( m_adapter );
    final ListContentProvider provider = new ListContentProvider();
    m_formatCombo.setContentProvider( provider );
    m_formatCombo.setLabelProvider( new ILabelProvider()
    {
      public Image getImage( Object element )
      {
        return null;
      }

      public String getText( Object element )
      {
        return element.toString();
      }

      public void addListener( ILabelProviderListener listener )
      {
      // nothing as labelprovider will not change
      }

      public void dispose()
      {
      // nothing as labelprovider will not change
      }

      public boolean isLabelProperty( Object element, String property )
      {
        return true;
      }

      public void removeListener( ILabelProviderListener listener )
      {
      //nothing
      }
    } );

    m_formatCombo.setInput( m_adapter );
    m_formatCombo.addSelectionChangedListener( this );

    if( m_adapter.size() > 0 )
      m_formatCombo.setSelection( new StructuredSelection( m_adapter.get( 0 ) ) );
  }

  public void createControlTarget( Composite parent )
  {
    Group group = new Group( parent, SWT.NONE );
    group.setText( "Ziel" );
    GridLayout gridLayout3 = new GridLayout();
    group.setLayout( gridLayout3 );
    GridData data4 = new GridData();
    data4.horizontalAlignment = GridData.FILL;
    data4.grabExcessHorizontalSpace = true;
    group.setLayoutData( data4 );

    Composite top = new Composite( group, SWT.NONE );
    GridData data = new GridData();
    data.horizontalAlignment = GridData.FILL;
    data.grabExcessHorizontalSpace = true;
    top.setLayoutData( data );

    GridLayout gridLayout = new GridLayout();
    gridLayout.numColumns = 3;
    top.setLayout( gridLayout );

    Label label = new Label( top, SWT.NONE );
    label.setText( "nach" );

    m_textFileTarget = new Text( top, SWT.BORDER );
    m_textFileTarget.setText( DEFAUL_FILE_LABEL );
    m_textFileTarget.addFocusListener( this );
    GridData data1 = new GridData();
    data1.horizontalAlignment = GridData.FILL;
    data1.grabExcessHorizontalSpace = true;
    m_textFileTarget.setLayoutData( data1 );
    Button button = new Button( top, SWT.PUSH );
    button.setText( "Auswahl ..." );
    GridData data2 = new GridData();
    data2.horizontalAlignment = GridData.END;
    button.setLayoutData( data2 );

    button.addSelectionListener( new SelectionAdapter()
    {
      public void widgetSelected( SelectionEvent e )
      {
        m_targetFile = chooseFileZML( m_targetFile );
        validate();

      }
    } );

    Composite bottom = new Composite( group, SWT.NONE );
    GridData data3 = new GridData();
    data3.horizontalAlignment = GridData.FILL;
    data3.grabExcessHorizontalSpace = true;
    bottom.setLayoutData( data3 );

    GridLayout gridLayout2 = new GridLayout();

    bottom.setLayout( gridLayout2 );

    m_buttonRetainMeta = new Button( bottom, SWT.CHECK );
    m_buttonRetainMeta.setText( "Metadaten beibehalten" );
    m_buttonRetainMeta.setSelection( true );
    m_buttonAppend = new Button( bottom, SWT.CHECK );
    m_buttonAppend.setText( "hinzufuegen statt ueberschreiben" );
    m_buttonAppend.setSelection( true );
  }

  File chooseFile( File selectedFile )
  {
    FileDialog dialog = new FileDialog( getShell(), SWT.SINGLE );
    if( selectedFile != null )
    {
      dialog.setFileName( selectedFile.getName() );
      dialog.setFilterPath( selectedFile.getParent() );
    }
    dialog.open();
    String fileName = dialog.getFileName();
    String filterPath = dialog.getFilterPath();
    return new File( filterPath, fileName );
  }
  File chooseFileZML( File selectedFile )
  {
    FileDialog dialog = new FileDialog( getShell(), SWT.SINGLE );
    dialog.setFilterExtensions( new String[] { "*.zml" }  );
    if( selectedFile != null )
    {
      dialog.setFileName( selectedFile.getName() );
      dialog.setFilterPath( selectedFile.getParent() );
    }
    dialog.open();
    String fileName = dialog.getFileName();
    String filterPath = dialog.getFilterPath();
    return new File( filterPath, fileName );
  }

  /**
   * validates the page
   */
  void validate()
  {
    setErrorMessage( null );
    setMessage( null );
    setPageComplete( true );
    StringBuffer error = new StringBuffer();
    if( m_sourceFile != null )
      m_textFileSource.setText( m_sourceFile.getName() );
    else
    {
      m_textFileSource.setText( DEFAUL_FILE_LABEL );
      error.append( "Quelle nicht ausgew‰hlt\n" );
      setPageComplete( false );
    }
    m_buttonAppend.setEnabled( false );
    m_buttonRetainMeta.setEnabled( false );
    if( m_targetFile != null )
    {
      m_textFileTarget.setText( m_targetFile.getName() );
      if( m_targetFile.exists() )
      {
        m_buttonAppend.setEnabled( true );
        m_buttonRetainMeta.setEnabled( true );
      }
    }
    else
    {
      m_textFileTarget.setText( DEFAUL_FILE_LABEL );
      error.append( "Ziel nicht ausgew‰hlt\n" );
      setPageComplete( false );
    }
    if( error.length() > 0 )
      setErrorMessage( error.toString() );
    else
      setMessage( "Eingaben OK" );
    fireSelectionChanged();
  }

  /**
   * 
   * @see org.eclipse.jface.wizard.IWizardPage#canFlipToNextPage()
   */
  public boolean canFlipToNextPage()
  {
    return isPageComplete();
  }

  /**
   * 
   * @see org.eclipse.jface.dialogs.IDialogPage#dispose()
   */
  public void dispose()
  {
    super.dispose();
    if( m_topLevel != null && !m_topLevel.isDisposed() )
    {
      m_topLevel.dispose();
      m_topLevel = null;
    }
  }

  /**
   * @see org.eclipse.swt.events.FocusListener#focusGained(org.eclipse.swt.events.FocusEvent)
   */
  public void focusGained( FocusEvent e )
  {
  // nothing
  }

  /**
   * @see org.eclipse.swt.events.FocusListener#focusLost(org.eclipse.swt.events.FocusEvent)
   */
  public void focusLost( FocusEvent e )
  {
    if( m_sourceFile != null && !m_sourceFile.getName().equals( m_textFileSource.getText() ) )
    {
      m_sourceFile = new File( m_sourceFile.getParentFile(), m_textFileSource.getText() );
    }
    if( m_targetFile != null && !m_targetFile.getName().equals( m_textFileTarget.getText() ) )
    {
      m_targetFile = new File( m_targetFile.getParentFile(), m_textFileTarget.getText() );
    }
    validate();
  }

  private void fireSelectionChanged()
  {
    for( Iterator iter = m_selectionListener.iterator(); iter.hasNext(); )
    {
      ( (ISelectionChangedListener)iter.next() ).selectionChanged( new SelectionChangedEvent( this, getSelection() ) );
    }
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#addSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  public void addSelectionChangedListener( ISelectionChangedListener listener )
  {
    m_selectionListener.add( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#getSelection()
   */
  public ISelection getSelection()
  {
    final IStructuredSelection formatSelection = (IStructuredSelection)m_formatCombo.getSelection();
    if( !m_controlFinished )
      return new ISelection()
      {
        public boolean isEmpty()
        {
          return true;
        }
      };
    return new ObservationImportSelection( m_sourceFile, m_targetFile, (INativeObservationAdapter)formatSelection
        .getFirstElement(), m_buttonAppend.getSelection(), m_buttonRetainMeta.getSelection() );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#removeSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  public void removeSelectionChangedListener( ISelectionChangedListener listener )
  {
    m_selectionListener.remove( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#setSelection(org.eclipse.jface.viewers.ISelection)
   */
  public void setSelection( ISelection selection )
  {
    if( selection instanceof ObservationImportSelection )
    {
      ObservationImportSelection s = ( (ObservationImportSelection)selection );
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
      Object firstElement = ( (StructuredSelection)selection ).getFirstElement();
      if( firstElement instanceof IFile )
      {
        m_targetFile = ResourceUtilities.makeFileFromPath( ( (IFile)firstElement ).getFullPath() );
      }
    }
    // nothing
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
   */
  public void selectionChanged( SelectionChangedEvent event )
  {
    fireSelectionChanged();
  }

}