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

package org.kalypso.wizard;

import java.io.File;
import java.rmi.RemoteException;
import java.util.Vector;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * 
 * Page for selecting waterlevel files (ArcView ascii raster format) and coordinate system
 * 
 * @author Nadja Peiler
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */

public class SelectWaterlevelWizardPage extends WizardPage
{
  private Composite m_topLevel;

  protected Table waterlevelTable;

  protected Integer selectedIndex = null;

  protected Vector<File> waterlevelGrids = new Vector<File>();

  protected File waterlevelFile;

  private String[] coordinateSystems = ( new ConvenienceCSFactoryFull() ).getKnownCS();

  CS_CoordinateSystem selectedCoordinateSystem;

  String selectedCoordinateSystemName;

  public SelectWaterlevelWizardPage()
  {
    super( "page_type:waterlevel", WizardMessages.getString("SelectWaterlevelWizardPage.LoadWaterlevelData"), ImageProvider.IMAGE_KALYPSO_ICON_BIG ); //$NON-NLS-1$ //$NON-NLS-2$
    setDescription( WizardMessages.getString("SelectWaterlevelWizardPage.WaterlevelWizardPage") ); //$NON-NLS-1$
    setPageComplete( false );
  }

  public Vector getWaterlevelGrids()
  {
    return waterlevelGrids;
  }

  public CS_CoordinateSystem getSelectedCoordinateSystem()
  {
    return selectedCoordinateSystem;
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

    createControlWaterlevel( m_topLevel );
    setControl( m_topLevel );
  }

  public void createControlWaterlevel( Composite parent )
  {
    GridLayout gridLayout = new GridLayout();
    gridLayout.marginHeight = 2;
    Group group = new Group( parent, SWT.NONE );
    GridData groupData = new GridData();
    groupData.horizontalAlignment = GridData.FILL;
    groupData.grabExcessHorizontalSpace = true;
    groupData.verticalAlignment = GridData.FILL;
    groupData.grabExcessVerticalSpace = true;
    group.setLayoutData( groupData );
    group.setLayout( gridLayout );
    group.layout();
    group.setText( WizardMessages.getString("SelectWaterlevelWizardPage.FloodedArea") ); //$NON-NLS-1$

    Composite tableComposite = new Composite( group, SWT.NULL );
    GridData gridData = new GridData( GridData.FILL_VERTICAL | GridData.HORIZONTAL_ALIGN_FILL | GridData.FILL_BOTH );
    tableComposite.setLayoutData( gridData );
    GridLayout layout = new GridLayout( 2, false );
    layout.marginWidth = 4;
    tableComposite.setLayout( layout );

    //  Create the table
    createTable( tableComposite );

    // Create Buttons
    createButtons( tableComposite );

    // Create CSCombo
    createCombo( tableComposite );

  }

  private void createTable( Composite parent )
  {
    waterlevelTable = new Table( parent, SWT.SINGLE | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER | SWT.FULL_SELECTION );
    GridData gridData = new GridData( GridData.FILL_BOTH );
    gridData.grabExcessVerticalSpace = true;
    gridData.horizontalSpan = 2;
    waterlevelTable.setLayoutData( gridData );

    TableColumn fileColumn = new TableColumn( waterlevelTable, SWT.LEFT );
    fileColumn.setText( WizardMessages.getString("SelectWaterlevelWizardPage.Data") ); //$NON-NLS-1$
    fileColumn.setWidth( 400 );
    /*
     * TableColumn hqColumn = new TableColumn( waterlevelTable, SWT.CENTER ); hqColumn.setText( "HQ" );
     * hqColumn.setWidth( 100 );
     */
    waterlevelTable.setHeaderVisible( true );
    waterlevelTable.setLinesVisible( true );

    waterlevelTable.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        int[] selection = waterlevelTable.getSelectionIndices();
        // nur singleSelection erlaubt
        selectedIndex = new Integer( selection[0] );
      }
    } );
  }

  private void createButtons( Composite parent )
  {

    Composite buttonComp = new Composite( parent, SWT.NULL );
    GridLayout layout = new GridLayout( 2, false );
    buttonComp.setLayout( layout );

    // Create and configure the "Add" button
    Button add = new Button( buttonComp, SWT.PUSH | SWT.CENTER );
    add.setImage( ImageProvider.IMAGE_STYLEEDITOR_ADD_RULE.createImage() );

    GridData gridData = new GridData( GridData.HORIZONTAL_ALIGN_END );
    gridData.widthHint = 30;
    add.setLayoutData( gridData );
    add.addSelectionListener( new SelectionAdapter()
    {

      @Override
      public void widgetSelected( SelectionEvent e )
      {
        //System.out.println( "Add..." );
        String filePath = chooseFile( waterlevelFile, new String[]
        { "*.asc" } ); //$NON-NLS-1$
        if( filePath != null )
        {
          waterlevelFile = new File( filePath );
          if( waterlevelFile.exists() )
          {
            TableItem item = new TableItem( waterlevelTable, 0 );
            item.setText( new String[]
            { waterlevelFile.getAbsolutePath() } );
            waterlevelTable.redraw();
            waterlevelGrids.add( waterlevelFile );
          }
        }
        validate();
      }
    } );

    //  Create and configure the "Delete" button
    Button delete = new Button( buttonComp, SWT.PUSH | SWT.CENTER );
    delete.setImage( ImageProvider.IMAGE_STYLEEDITOR_REMOVE.createImage() );
    gridData = new GridData( GridData.HORIZONTAL_ALIGN_END );
    gridData.widthHint = 30;
    delete.setLayoutData( gridData );

    delete.addSelectionListener( new SelectionAdapter()
    {

      //  Remove the selection and refresh the table
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        //System.out.println( "Delete..." );
        if( selectedIndex != null )
        {
          waterlevelGrids.remove( selectedIndex.intValue() );
          waterlevelTable.remove( selectedIndex.intValue() );
          waterlevelTable.redraw();
          selectedIndex = null;
        }
        validate();
      }
    } );

    Label dummyLabel = new Label( parent, SWT.NONE );
    dummyLabel.setText( "" ); //$NON-NLS-1$
  }

  private void createCombo( Composite parent )
  {
    Label csLabel = new Label( parent, SWT.NONE );
    csLabel.setText( WizardMessages.getString("SelectWaterlevelWizardPage.CoordinateSystem")+": " ); //$NON-NLS-1$ //$NON-NLS-2$

    final Combo csCombo = new Combo( parent, SWT.NONE );
    csCombo.setItems( coordinateSystems );
    try
    {
      selectedCoordinateSystemName = KalypsoGisPlugin.getDefault().getCoordinatesSystem().getName();
    }
    catch( RemoteException e1 )
    {
      e1.printStackTrace();
    }
    csCombo.select( csCombo.indexOf( selectedCoordinateSystemName ) );

    GridData data3 = new GridData();
    data3.horizontalSpan = 1;
    csCombo.setLayoutData( data3 );

    csCombo.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        selectedCoordinateSystemName = csCombo.getText();
        validate();
      }
    } );

    csCombo.addModifyListener( new ModifyListener()
    {
      public void modifyText( ModifyEvent e )
      {
        setPageComplete( false );
      }
    } );

    csCombo.addListener( SWT.DefaultSelection, new Listener()
    {
      public void handleEvent( Event e )
      {
        selectedCoordinateSystemName = ( (Combo)e.widget ).getText();
        validate();
      }
    } );

  }

  /**
   * opens a file dialog
   * 
   * @param selectedFile
   * @param filterExtensions
   * @return selected filePath
   *  
   */
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
    if( fileName != null && fileName != "" && filterPath != null ) //$NON-NLS-1$
    {
      filePath = filterPath + "/" + fileName; //$NON-NLS-1$
    }
    return filePath;
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

    if( !( waterlevelGrids.size() > 0 ) )
    {
      error.append( WizardMessages.getString("SelectWaterlevelWizardPage.NoDataSelected")+"\n\n" ); //$NON-NLS-1$ //$NON-NLS-2$
      setPageComplete( false );
    }

    if( selectedCoordinateSystemName != null )
    {
      selectedCoordinateSystem = ConvenienceCSFactory.getInstance().getOGCCSByName( selectedCoordinateSystemName );
      if( selectedCoordinateSystem == null )
      {
        error.append( WizardMessages.getString("SelectWaterlevelWizardPage.CoordinateSystemNotExists")+"\n\n" ); //$NON-NLS-1$ //$NON-NLS-2$
        setPageComplete( false );
      }
    }

    if( error.length() > 0 )
      setMessage( error.toString() );
    else
      setMessage( WizardMessages.getString("SelectWaterlevelWizardPage.SelectionOK") ); //$NON-NLS-1$
  }

  /**
   * 
   * WaterlevelInputDialog
   * <p>
   * Dialog to choose waterlevel file (ascii) and assign an annuality !! not uses at the moment !!
   * 
   * created by
   * 
   * @author Nadja Peiler (19.06.2005)
   */
  class WaterlevelInputDialog extends Dialog
  {
    private String DEFAUL_FILE_LABEL = ""; //$NON-NLS-1$

    protected File m_file = null;

    private static final String fNON_NEGATIVE_INTEGER_FIELD = "(\\d){1,9}"; //$NON-NLS-1$

    protected Integer m_hq = null;

    public WaterlevelInputDialog( Shell parentShell )
    {
      super( parentShell );
    }

    @Override
    protected Control createDialogArea( Composite parent )
    {
      parent.getShell().setText( WizardMessages.getString("SelectWaterlevelWizardPage.AddFloodedArea")+"..." ); //$NON-NLS-1$ //$NON-NLS-2$

      Composite topLevel = new Composite( parent, SWT.NONE );

      GridLayout gridLayout = new GridLayout();
      gridLayout.numColumns = 3;
      topLevel.setLayout( gridLayout );

      GridData data = new GridData();
      data.horizontalAlignment = GridData.FILL;
      data.grabExcessHorizontalSpace = true;
      topLevel.setLayoutData( data );

      createFileMenu( topLevel );
      createHQMenu( topLevel );

      return topLevel;
    }

    private void createFileMenu( Composite parent )
    {
      Label label = new Label( parent, SWT.NONE );
      label.setText( WizardMessages.getString("SelectWaterlevelWizardPage.Data")+": " ); //$NON-NLS-1$

      final Text textFileSource = new Text( parent, SWT.BORDER );
      textFileSource.setText( DEFAUL_FILE_LABEL );
      textFileSource.setEditable( false );

      GridData data = new GridData();
      data.horizontalAlignment = GridData.FILL;
      data.grabExcessHorizontalSpace = true;
      textFileSource.setLayoutData( data );

      Button button = new Button( parent, SWT.PUSH );
      button.setText( WizardMessages.getString("SelectWaterlevelWizardPage.Browse")+"..." ); //$NON-NLS-1$ //$NON-NLS-2$
      GridData data2 = new GridData();
      data2.horizontalAlignment = GridData.END;
      button.setLayoutData( data2 );

      button.addSelectionListener( new SelectionAdapter()
      {
        @Override
        public void widgetSelected( SelectionEvent e )
        {
          String filePath = chooseFile( m_file, new String[]
          { "*.asc" } ); //$NON-NLS-1$
          if( filePath != null )
          {
            m_file = new File( filePath );
            textFileSource.setText( filePath );
          }
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
      if( fileName != null && fileName != "" && filterPath != null ) //$NON-NLS-1$
      {
        filePath = filterPath + "/" + fileName; //$NON-NLS-1$
      }
      return filePath;
    }

    private void createHQMenu( Composite parent )
    {
      Label label = new Label( parent, SWT.NONE );
      label.setText( "HQ: " ); //$NON-NLS-1$

      Text textHQ = new Text( parent, SWT.BORDER );
      textHQ.setText( DEFAUL_FILE_LABEL );

      GridData data = new GridData();
      data.horizontalAlignment = GridData.FILL;
      data.grabExcessHorizontalSpace = true;
      data.horizontalSpan = 2;
      textHQ.setLayoutData( data );
      textHQ.addModifyListener( new ModifyListener()
      {
        public void modifyText( ModifyEvent e )
        {
          m_hq = new Integer( ( (Text)e.getSource() ).getText() );
        }
      } );
      textHQ.addVerifyListener( new VerifyListener()
      {
        public void verifyText( VerifyEvent e )
        {
          e.doit = false;
          if( e.text.matches( fNON_NEGATIVE_INTEGER_FIELD ) )
            e.doit = true;
        }
      } );
    }

    public Integer getHQ()
    {
      return m_hq;
    }

    public File getFile()
    {
      File returnFile = null;

      if( m_file.exists() )
        returnFile = m_file;

      return returnFile;
    }
  }
}