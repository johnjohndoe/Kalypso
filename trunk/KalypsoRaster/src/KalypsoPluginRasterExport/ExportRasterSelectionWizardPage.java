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

package KalypsoPluginRasterExport;

import java.io.File;
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
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.contribs.eclipse.core.resources.ProjectUtilities;
import org.kalypso.contribs.eclipse.ui.dialogs.KalypsoResourceSelectionDialog;
import org.kalypso.contribs.eclipse.ui.dialogs.ResourceSelectionValidator;

/**
 * ExportRasterSelectionWizardPage, Selection: source(*.gml) and target(*.asc) file
 * 
 * @author Nadja Peiler
 */
public class ExportRasterSelectionWizardPage extends WizardPage implements FocusListener, ISelectionProvider
{
  private static final String DEFAUL_FILE_LABEL = "";

  private final List<ISelectionChangedListener> m_selectionListener = new ArrayList<ISelectionChangedListener>();

  private Composite m_topLevel = null;

  Text m_textFileSource;

  private Text m_textFileTarget;

  Combo m_formatCombo;

  String[] formats = { "Ascii", "Other" };

  String m_format = formats[0];

  File m_targetFile = null;

  IPath m_sourcePath = null;

  IProject selectedProject;

  public ExportRasterSelectionWizardPage( String pageName )
  {
    this( pageName, null, null );
  }

  public ExportRasterSelectionWizardPage( String pageName, String title, ImageDescriptor titleImage )
  {
    super( pageName, title, titleImage );

    setDescription( "Auswahl Rasterdaten" );
    setTitle( "Auswahl der zu exportierenden Rasterdaten" );
    setPageComplete( false );
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

    // line 1
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
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        selectedProject = ProjectUtilities.getSelectedProjects()[0];
        KalypsoResourceSelectionDialog dialog = createResourceDialog( new String[] { "gml" } );
        dialog.open();
        Object[] result = dialog.getResult();
        if( result != null )
        {
          Path resultPath = (Path) result[0];
          m_textFileSource.setText( resultPath.toString() );
          m_sourcePath = resultPath;
        }
        validate();
      }
    } );
  }

  KalypsoResourceSelectionDialog createResourceDialog( String[] fileResourceExtensions )
  {
    return new KalypsoResourceSelectionDialog( getShell(), selectedProject, "Select resource", fileResourceExtensions, selectedProject, new ResourceSelectionValidator() );
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
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        if( m_format.equals( formats[0] ) )
        {
          String targetFileName = chooseFile( m_targetFile, new String[] { "*.asc" } );
          if( targetFileName != null )
          {
            if( targetFileName.indexOf( "." ) == -1 )
              m_targetFile = new File( targetFileName + ".asc" );
            else
              m_targetFile = new File( targetFileName );
          }
        }
        else
        {
          String targetFileName = chooseFile( m_targetFile, null );
          if( targetFileName != null )
            m_targetFile = new File( targetFileName );
        }
        validate();
      }
    } );

    // line 2

    Label formatLabel = new Label( group, SWT.NONE );
    formatLabel.setText( "Format " );

    m_formatCombo = new Combo( group, SWT.NONE );
    m_formatCombo.setItems( formats );
    m_formatCombo.select( 0 );
    m_formatCombo.addSelectionListener( new SelectionListener()
    {

      public void widgetSelected( SelectionEvent e )
      {
        m_format = formats[m_formatCombo.getSelectionIndex()];
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {// default
      }
    } );
  }

  String chooseFile( File selectedFile, String[] filterExtensions )
  {
    FileDialog dialog = new FileDialog( getShell(), SWT.SINGLE );
    dialog.setFilterExtensions( filterExtensions );
    if( selectedFile != null )
    {
      dialog.setFileName( selectedFile.getName() );
      dialog.setFilterPath( selectedFile.getParent() );
    }
    dialog.open();
    String fileName = dialog.getFileName();
    String filterPath = dialog.getFilterPath();
    String filePath = null;
    if( fileName != null && fileName != "" && filterPath != null )
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
    StringBuffer error = new StringBuffer();
    if( m_sourcePath != null )
    {
      setPageComplete( true );
    }
    else
    {
      m_textFileSource.setText( DEFAUL_FILE_LABEL );
      error.append( "Quelle nicht ausgew‰hlt \n" );
      setPageComplete( false );
    }

    if( m_targetFile != null )
    {
      m_textFileTarget.setText( m_targetFile.getName() );
    }
    else
    {
      m_textFileTarget.setText( DEFAUL_FILE_LABEL );
      error.append( "Ziel nicht ausgew‰hlt" );
      setPageComplete( false );
    }
    if( error.length() > 0 )
      setErrorMessage( error.toString() );
    else
      setMessage( "Eingaben OK" );
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
    if( m_sourcePath != null && !m_sourcePath.equals( m_textFileSource.getText() ) )
    {
      m_sourcePath = new Path( m_textFileSource.getText() );
    }
    if( m_targetFile != null && !m_targetFile.getName().equals( m_textFileTarget.getText() ) )
    {
      m_targetFile = new File( m_targetFile.getParentFile(), m_textFileTarget.getText() );
    }
    validate();
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
  public ISelection getSelection( )
  {
    return new RasterExportSelection( m_sourcePath, selectedProject, m_targetFile, m_format );
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
    if( selection instanceof RasterExportSelection )
    {
      RasterExportSelection s = ((RasterExportSelection) selection);
      m_sourcePath = s.getPathSource();
      m_targetFile = s.getFileTarget();
      selectedProject = s.getProject();
    }
  }
}