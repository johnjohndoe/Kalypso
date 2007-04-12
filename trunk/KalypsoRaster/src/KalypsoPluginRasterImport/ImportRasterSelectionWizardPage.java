/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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

package KalypsoPluginRasterImport;

import java.io.File;
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Text;
import org.kalypso.contribs.eclipse.core.resources.ProjectUtilities;
import org.kalypso.contribs.eclipse.ui.dialogs.KalypsoResourceSelectionDialog;
import org.kalypso.contribs.eclipse.ui.dialogs.ResourceSelectionValidator;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * ImportRasterSelectionWizardPage, Selection: source(*.ascii) and target(*.gml) file
 * 
 * @author Nadja Peiler
 * 
 * @deprecated use KalypsoGmlUi - RectifiedGridCoverageImportWizard for importing raster files (ascii, tif, geotif)...
 */
@Deprecated
public class ImportRasterSelectionWizardPage extends WizardPage implements FocusListener, ISelectionProvider
{
  private static final String DEFAUL_FILE_LABEL = "";

  private final List<ISelectionChangedListener> m_selectionListener = new ArrayList<ISelectionChangedListener>();

  private Composite m_topLevel = null;

  private Text m_textFileSource;

  Text m_textFileTarget;

  Combo m_formatCombo;

  String[] formats = { "Ascii", "Other" };

  String m_format = formats[0];

  IPath m_targetPath = null;

  File m_sourceFile = null;

  IProject selectedProject;

  private final String[] coordinateSystems = (new ConvenienceCSFactoryFull()).getKnownCS();

  CS_CoordinateSystem selectedCoordinateSystem;

  String selectedCoordinateSystemName;

  public ImportRasterSelectionWizardPage( final String pageName )
  {
    this( pageName, null, null );
  }

  public ImportRasterSelectionWizardPage( final String pageName, final String title, final ImageDescriptor titleImage )
  {
    super( pageName, title, titleImage );

    setDescription( "Auswahl Rasterdaten" );
    setTitle( "Auswahl der zu importierenden Rasterdaten" );
    setPageComplete( false );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    initializeDialogUnits( parent );
    m_topLevel = new Composite( parent, SWT.NONE );

    final GridLayout gridLayout = new GridLayout();
    m_topLevel.setLayout( gridLayout );

    final GridData data = new GridData();
    data.horizontalAlignment = GridData.FILL;
    data.grabExcessHorizontalSpace = true;
    m_topLevel.setLayoutData( data );

    createControlSource( m_topLevel );
    createControlTarget( m_topLevel );
    setControl( m_topLevel );
    // validate();
  }

  public void createControlSource( final Composite parent )
  {
    final Group group = new Group( parent, SWT.NONE );
    group.setText( "Quelle" );

    final GridData data = new GridData();
    data.horizontalAlignment = GridData.FILL;
    data.grabExcessHorizontalSpace = true;
    group.setLayoutData( data );

    final GridLayout gridLayout = new GridLayout();
    gridLayout.numColumns = 3;
    group.setLayout( gridLayout );

    // line 1
    final Label label = new Label( group, SWT.NONE );
    label.setText( "von " );

    m_textFileSource = new Text( group, SWT.BORDER );
    m_textFileSource.setText( ImportRasterSelectionWizardPage.DEFAUL_FILE_LABEL );
    m_textFileSource.addFocusListener( this );

    final GridData data1 = new GridData();
    data1.horizontalAlignment = GridData.FILL;
    data1.grabExcessHorizontalSpace = true;
    m_textFileSource.setLayoutData( data1 );

    final Button button = new Button( group, SWT.PUSH );
    button.setText( "Auswahl ..." );
    final GridData data2 = new GridData();
    data2.horizontalAlignment = GridData.END;
    button.setLayoutData( data2 );

    button.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        if( m_format.equals( formats[0] ) )
        {
          final String filePath = chooseFile( m_sourceFile, new String[] { "*.asc" } );
          if( filePath != null )
          {
            m_sourceFile = new File( filePath );
          }
        }
        else
        {
          final String filePath = chooseFile( m_sourceFile, null );
          if( filePath != null )
          {
            m_sourceFile = new File( filePath );
          }
        }
        validate();
      }
    } );
    // line 2

    final Label formatLabel = new Label( group, SWT.NONE );
    formatLabel.setText( "Format " );

    m_formatCombo = new Combo( group, SWT.NONE );
    m_formatCombo.setItems( formats );
    m_formatCombo.select( 0 );
    m_formatCombo.addSelectionListener( new SelectionListener()
    {

      public void widgetSelected( final SelectionEvent e )
      {
        m_format = formats[m_formatCombo.getSelectionIndex()];
      }

      public void widgetDefaultSelected( final SelectionEvent e )
      {// default
      }
    } );

    final Label dummyLabel = new Label( group, SWT.NONE );
    dummyLabel.setText( "" );

    // line 3
    final Label csLabel = new Label( group, SWT.NONE );
    csLabel.setText( "Coordinate system: " );

    final Combo csCombo = new Combo( group, SWT.NONE );
    csCombo.setItems( coordinateSystems );
    try
    {
      selectedCoordinateSystemName = KalypsoGisPlugin.getDefault().getCoordinatesSystem().getName();
    }
    catch( final RemoteException e1 )
    {
      e1.printStackTrace();
    }
    csCombo.select( csCombo.indexOf( selectedCoordinateSystemName ) );

    final GridData data3 = new GridData();
    data3.horizontalSpan = 2;
    csCombo.setLayoutData( data3 );

    csCombo.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        selectedCoordinateSystemName = csCombo.getText();
        validate();
      }
    } );

    csCombo.addModifyListener( new ModifyListener()
    {
      public void modifyText( final ModifyEvent e )
      {
        setPageComplete( false );
      }
    } );

    csCombo.addListener( SWT.DefaultSelection, new Listener()
    {
      public void handleEvent( final Event e )
      {
        selectedCoordinateSystemName = ((Combo) e.widget).getText();
        validate();
      }
    } );
  }

  public void createControlTarget( final Composite parent )
  {
    final Group group = new Group( parent, SWT.NONE );
    group.setText( "Ziel" );
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

    final Label label = new Label( top, SWT.NONE );
    label.setText( "nach" );

    m_textFileTarget = new Text( top, SWT.BORDER );
    m_textFileTarget.setText( ImportRasterSelectionWizardPage.DEFAUL_FILE_LABEL );
    m_textFileTarget.addFocusListener( this );
    final GridData data1 = new GridData();
    data1.horizontalAlignment = GridData.FILL;
    data1.grabExcessHorizontalSpace = true;
    m_textFileTarget.setLayoutData( data1 );
    final Button button = new Button( top, SWT.PUSH );
    button.setText( "Auswahl ..." );
    final GridData data2 = new GridData();
    data2.horizontalAlignment = GridData.END;
    button.setLayoutData( data2 );

    button.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        try
        {
          selectedProject = ProjectUtilities.getSelectedProjects()[0];
          final KalypsoResourceSelectionDialog dialog = createResourceDialog( new String[] { "gml" } );
          dialog.open();
          final Object[] result = dialog.getResult();
          if( result != null )
          {
            final Path resultPath = (Path) result[0];
            m_textFileTarget.setText( resultPath.toString() );
            m_targetPath = resultPath;
          }
          validate();

        }
        catch( final Exception e1 )
        {
          e1.printStackTrace();
        }
      }
    } );
  }

  KalypsoResourceSelectionDialog createResourceDialog( final String[] fileResourceExtensions )
  {
    return new KalypsoResourceSelectionDialog( getShell(), selectedProject, "Select resource", fileResourceExtensions, selectedProject, new ResourceSelectionValidator() );
  }

  String chooseFile( final File selectedFile, final String[] filterExtensions )
  {
    final FileDialog dialog = new FileDialog( getShell(), SWT.SINGLE );
    dialog.setFilterExtensions( filterExtensions );
    if( selectedFile != null )
    {
      dialog.setFileName( selectedFile.getName() );
      dialog.setFilterPath( selectedFile.getParent() );
    }
    dialog.open();
    final String fileName = dialog.getFileName();
    final String filterPath = dialog.getFilterPath();
    String filePath = null;
    if( (fileName != null) && (fileName != "") && (filterPath != null) )
    {
      filePath = filterPath + "/" + fileName;
    }
    return filePath;
  }

  /**
   * validates the page
   */
  void validate( )
  {
    setErrorMessage( null );
    setMessage( null );
    setPageComplete( true );
    final StringBuffer error = new StringBuffer();
    if( m_sourceFile != null )
    {
      m_textFileSource.setText( m_sourceFile.getName() );
      if( !m_sourceFile.exists() )
      {
        error.append( "Quelle existiert nicht\n" );
        setPageComplete( false );
      }
    }
    else
    {
      m_textFileSource.setText( ImportRasterSelectionWizardPage.DEFAUL_FILE_LABEL );
      error.append( "Quelle nicht ausgew�hlt\n" );
      setPageComplete( false );
    }

    if( selectedCoordinateSystemName != null )
    {
      selectedCoordinateSystem = ConvenienceCSFactory.getInstance().getOGCCSByName( selectedCoordinateSystemName );
      if( selectedCoordinateSystem == null )
      {
        error.append( "Koordinatensystem existiert nicht\n" );
        setPageComplete( false );
      }
    }

    if( m_targetPath != null )
    {
      setPageComplete( true );
    }
    else
    {
      m_textFileTarget.setText( ImportRasterSelectionWizardPage.DEFAUL_FILE_LABEL );
      error.append( "Ziel nicht ausgew�hlt\n" );
      setPageComplete( false );
    }
    if( error.length() > 0 )
    {
      setErrorMessage( error.toString() );
      // setMessage( error.toString() );
    }
    else
    {
      setMessage( "Eingaben OK" );
    }
  }

  /**
   * @see org.eclipse.jface.wizard.IWizardPage#canFlipToNextPage()
   */
  @Override
  public boolean canFlipToNextPage( )
  {
    return false;
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#dispose()
   */
  @Override
  public void dispose( )
  {
    super.dispose();
    if( (m_topLevel != null) && !m_topLevel.isDisposed() )
    {
      m_topLevel.dispose();
      m_topLevel = null;
    }
  }

  /**
   * @see org.eclipse.swt.events.FocusListener#focusGained(org.eclipse.swt.events.FocusEvent)
   */
  public void focusGained( final FocusEvent e )
  {
    // nothing
  }

  /**
   * @see org.eclipse.swt.events.FocusListener#focusLost(org.eclipse.swt.events.FocusEvent)
   */
  public void focusLost( final FocusEvent e )
  {
    if( (m_sourceFile != null) && !m_sourceFile.getName().equals( m_textFileSource.getText() ) )
    {
      m_sourceFile = new File( m_sourceFile.getParentFile(), m_textFileSource.getText() );
    }
    if( (m_targetPath != null) && !m_targetPath.equals( m_textFileTarget.getText() ) )
    {
      m_targetPath = new Path( m_textFileTarget.getText() );
    }
    validate();
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#addSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  public void addSelectionChangedListener( final ISelectionChangedListener listener )
  {
    m_selectionListener.add( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#getSelection()
   */
  public ISelection getSelection( )
  {
    return new RasterImportSelection( m_sourceFile, m_targetPath, selectedProject, m_format );
  }

  public CS_CoordinateSystem getSelectedCoordinateSystem( )
  {
    return selectedCoordinateSystem;
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#removeSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  public void removeSelectionChangedListener( final ISelectionChangedListener listener )
  {
    m_selectionListener.remove( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#setSelection(org.eclipse.jface.viewers.ISelection)
   */
  public void setSelection( final ISelection selection )
  {
    if( selection instanceof RasterImportSelection )
    {
      final RasterImportSelection s = ((RasterImportSelection) selection);
      m_sourceFile = s.getFileSource();
      m_targetPath = s.getPathTarget();
      selectedProject = s.getProject();
    }
  }
}