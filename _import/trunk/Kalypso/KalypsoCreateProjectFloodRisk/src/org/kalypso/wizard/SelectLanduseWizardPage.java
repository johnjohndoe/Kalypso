package org.kalypso.wizard;

import java.io.File;
import java.io.IOException;
import java.rmi.RemoteException;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
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
import org.kalypso.java.io.FileUtilities;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree_impl.io.shpapi.DBaseException;
import org.kalypsodeegree_impl.io.shpapi.HasNoDBaseFileException;
import org.kalypsodeegree_impl.io.shpapi.ShapeFile;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.opengis.cs.CS_CoordinateSystem;

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

/**
 * 
 * SelectLanduseWizardPage
 * <p>
 * Select: shape file (landuse), coordinate system, name of property to raster
 * and selection automatically create landuseCollection
 * 
 * created by
 * 
 * @author Nadja Peiler (19.06.2005)
 */

public class SelectLanduseWizardPage extends WizardPage implements
    FocusListener
{

  private Composite m_topLevel;

  private Text m_textFileSource;

  private String DEFAUL_FILE_LABEL = "";

  File m_file;

  private String[] coordinateSystems = ( new ConvenienceCSFactoryFull() )
      .getKnownCS();

  CS_CoordinateSystem selectedCoordinateSystem;

  String selectedCoordinateSystemName;

  private Combo landusePropCombo;

  protected String propertyName;

  private boolean check;

  private Label landusePropLabel;

  public SelectLanduseWizardPage()
  {
    super( "page_type:landuse", "Landnutzungsdaten einlesen",
        ImageProvider.IMAGE_KALYPSO_ICON_BIG );
    setDescription( "Landuse WizardPage" );
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

    createControlFile( m_topLevel );
    setControl( m_topLevel );
    //validate();
  }

  public void createControlFile( Composite parent )
  {
    Group group = new Group( parent, SWT.NONE );
    group.setText( "Landnutzung" );

    GridData data = new GridData();
    data.horizontalAlignment = GridData.FILL;
    data.grabExcessHorizontalSpace = true;
    group.setLayoutData( data );

    GridLayout gridLayout = new GridLayout();
    gridLayout.numColumns = 3;
    group.setLayout( gridLayout );

    //  line 1: ShapeFile selection
    Label label = new Label( group, SWT.NONE );
    label.setText( "Shape-Datei: " );

    m_textFileSource = new Text( group, SWT.BORDER );
    m_textFileSource.setText( DEFAUL_FILE_LABEL );
    m_textFileSource.addFocusListener( this );
    m_textFileSource.setEditable( false );

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
        String filePath = chooseFile( m_file, new String[]
        { "*.shp" } );
        if( filePath != null )
        {
          m_file = new File( filePath );
          if( m_file.exists() )
          {
            loadLanduseProperties( FileUtilities.nameWithoutExtension( m_file
                .toString() ) );
            landusePropLabel.setEnabled( true );
            landusePropCombo.setEnabled( true );
          }
        }
        validate();
      }
    } );

    //line2: coordinate system
    Label csLabel = new Label( group, SWT.NONE );
    csLabel.setText( "Coordinate system: " );

    final Combo csCombo = new Combo( group, SWT.NONE );
    csCombo.setItems( coordinateSystems );
    try
    {
      selectedCoordinateSystemName = KalypsoGisPlugin.getDefault()
          .getCoordinatesSystem().getName();
    }
    catch( RemoteException e1 )
    {
      e1.printStackTrace();
    }
    csCombo.select( csCombo.indexOf( selectedCoordinateSystemName ) );

    GridData data3 = new GridData();
    data3.horizontalSpan = 2;
    csCombo.setLayoutData( data3 );

    csCombo.addSelectionListener( new SelectionAdapter()
    {
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

    //line 3: landusePropertyName
    landusePropLabel = new Label( group, SWT.NONE );
    landusePropLabel.setText( "Attribut Landnutzung: " );
    landusePropLabel.setEnabled( false );

    landusePropCombo = new Combo( group, SWT.READ_ONLY );
    landusePropCombo.setEnabled( false );
    landusePropCombo.addSelectionListener( new SelectionAdapter()
    {
      public void widgetSelected( SelectionEvent e )
      {
        propertyName = landusePropCombo.getText();
      }
    } );

    Label dummyLabel2 = new Label( group, SWT.NONE );

    //line 4: check autogenerateLanduseCollection
    final Button checkButton = new Button( group, SWT.CHECK );
    checkButton.setSelection( check );
    checkButton.addSelectionListener( new SelectionAdapter()
    {
      public void widgetSelected( SelectionEvent e )
      {
        check = checkButton.getSelection();
      }
    } );

    Label landuseColLabel = new Label( group, SWT.NONE );
    landuseColLabel.setText( "LanduseCollection automatisch generieren" );
  }

  /**
   * loads the property names of a shape file
   * 
   * @param shapeBaseFile
   *  
   */
  void loadLanduseProperties( String shapeBaseFile )
  {
    try
    {
      ShapeFile shape = new ShapeFile( shapeBaseFile );
      String[] propertyNames = shape.getProperties();
      landusePropCombo.setItems( propertyNames );
      propertyName = propertyNames[0];
      landusePropCombo.select( landusePropCombo.indexOf( propertyName ) );
    }
    catch( IOException e )
    {
      e.printStackTrace();
    }
    catch( HasNoDBaseFileException e )
    {
      e.printStackTrace();
    }
    catch( DBaseException e )
    {
      e.printStackTrace();
    }
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
    if( fileName != null && fileName != "" && filterPath != null )
    {
      filePath = filterPath + "/" + fileName;
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
    setPageComplete( false );
    StringBuffer error = new StringBuffer();
    if( m_file != null )
    {
      m_textFileSource.setText( m_file.getAbsolutePath() );
      if( !m_file.exists() )
      {
        error.append( "Datei existiert nicht\n\n" );
        setPageComplete( false );
      }
    }
    else
    {
      m_textFileSource.setText( DEFAUL_FILE_LABEL );
      error.append( "Datei nicht ausgew‰hlt\n\n" );
      setPageComplete( false );
    }

    if( selectedCoordinateSystemName != null )
    {
      selectedCoordinateSystem = ConvenienceCSFactory.getInstance()
          .getOGCCSByName( selectedCoordinateSystemName );
      if( selectedCoordinateSystem == null )
      {
        error.append( "Koordinatensystem existiert nicht\n\n" );
        setPageComplete( false );
      }
    }

    if( propertyName != null )
    {
      //nothing
    }
    else
    {
      error.append( "Attribut Landnutzung w‰hlen.\n\n" );
      setPageComplete( false );
    }

    if( error.length() > 0 )
      setMessage( error.toString() );
    else
    {
      setMessage( "Eingabe OK" );
      setPageComplete( true );
    }
  }

  public File getLanduseDataFile()
  {
    return m_file;
  }

  public CS_CoordinateSystem getSelectedCoordinateSystem()
  {
    return selectedCoordinateSystem;
  }

  public boolean isCheck()
  {
    return check;
  }

  public String getPropertyName()
  {
    return propertyName;
  }

  /**
   * @see org.eclipse.swt.events.FocusListener#focusGained(org.eclipse.swt.events.FocusEvent)
   */
  public void focusGained( FocusEvent e )
  {// nothing
  }

  /**
   * @see org.eclipse.swt.events.FocusListener#focusLost(org.eclipse.swt.events.FocusEvent)
   */
  public void focusLost( FocusEvent e )
  {
    if( m_file != null && !m_file.getName().equals( m_textFileSource.getText() ) )
    {
      m_file = new File( m_file.getParentFile(), m_textFileSource.getText() );
    }

    validate();
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

}