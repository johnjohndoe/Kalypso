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

/*
 * Created on 31.01.2005
 *  
 */
package org.kaylpso.ui.wizard.shape;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.rmi.RemoteException;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
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
import org.eclipse.ui.dialogs.ContainerSelectionDialog;
import org.eclipse.ui.dialogs.ResourceListSelectionDialog;
import org.kalypso.java.io.FileUtilities;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author kuepfer
 *  
 */
public class ImportShapeFileImportPage extends WizardPage implements SelectionListener,
    ModifyListener
{

  //constants
  private static final int SIZING_TEXT_FIELD_WIDTH = 250;

  //widgets
  private Group fileGroup;

  private Label fileLabel;

  private Text textField;

  private Composite topComposite;

  private Button browseButton;

  //Geodata
  private CS_CoordinateSystem customCS = null;

  private CS_CoordinateSystem defaultCS = ConvenienceCSFactory.getInstance().getOGCCSByName(
      "EPSG:31467" );

  private URL fileURL;

  //mapping

  private Label projectLabel;

  private Text projectTextField;

  private Button projectBrowse;

  private IPath m_path;

  private Combo m_checkCRS;

  private Button checkToMap;

  private Text toMapText;

  private Label toMapLabel;

  private Button toMapBrowse;

  private String m_relativeProjectPath;

  private IProject[] m_projects = null;

  /**
   * @param pageName
   */
  public ImportShapeFileImportPage( String pageName )
  {
    super( pageName );
    setDescription( "Dieser Dialog liest eine ESRI Shape-Datei in den Workspace ein." );

    setPageComplete( false );
  }

  /**
   * @param pageName
   * @param title
   * @param titleImage
   */
  public ImportShapeFileImportPage( String pageName, String title, ImageDescriptor titleImage )
  {
    super( pageName, title, titleImage );
    setDescription( "Dieser Dialog liest eine ESRI Shape-Datei in den Workspace ein." );
    setPageComplete( false );
  }

  private boolean checkSuffix( Text path )
  {
    boolean test = false;
    int dotLoc = path.getText().lastIndexOf( '.' );
    if( dotLoc != -1 )
    {
      String ext = path.getText().substring( dotLoc + 1 );
      if( ext.equalsIgnoreCase( "shp" ) == false )
        test = false;
      else
        test = true;
    }
    return test;
  }

  /*
   * (non-Javadoc)
   * 
   * @see wizard.eclipse.jface.dialogs.IDialogPage#createControl(wizard.eclipse.swt.widgets.Composite)
   */
  public void createControl( Composite parent )
  {

    topComposite = new Composite( parent, SWT.NULL );
    topComposite.setFont( parent.getFont() );

    initializeDialogUnits( parent );

    //		WorkbenchHelp.setHelp(topComposite,
    //				IHelpContextIds.NEW_PROJECT_WIZARD_PAGE);

    topComposite.setLayout( new GridLayout() );
    topComposite.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    // build wizard page
    createFileGroup( topComposite );
    setControl( topComposite );

  }

  private void createFileGroup( Composite parent )
  {
    fileGroup = new Group( parent, SWT.NULL );
    GridLayout topGroupLayout = new GridLayout();
    GridData topGroupData = new GridData();
    topGroupLayout.numColumns = 3;
    topGroupData.horizontalAlignment = GridData.FILL;
    fileGroup.setLayout( topGroupLayout );
    fileGroup.setLayoutData( topGroupData );
    fileGroup.setText( "Shape-Datei" );
    fileLabel = new Label( fileGroup, SWT.NONE );
    fileLabel.setText( "Quelle : " );

    // Set width of Text fields
    GridData dataCatchment = new GridData( GridData.FILL_HORIZONTAL );
    dataCatchment.widthHint = SIZING_TEXT_FIELD_WIDTH;

    textField = new Text( fileGroup, SWT.BORDER );
    textField.setLayoutData( dataCatchment );
    textField.setEditable( false );
    textField.addModifyListener( this );

    browseButton = new Button( fileGroup, SWT.PUSH );
    browseButton.setText( "Durchsuchen..." );
    browseButton.setLayoutData( new GridData( GridData.END ) );
    browseButton.addSelectionListener( this );

    projectLabel = new Label( fileGroup, SWT.NONE );
    projectLabel.setText( "Ziel: " );
    projectTextField = new Text( fileGroup, SWT.BORDER );
    projectTextField.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    projectTextField.setEditable( true );
    projectBrowse = new Button( fileGroup, SWT.PUSH );
    projectBrowse.setText( "Durchsuchen..." );
    projectBrowse.addSelectionListener( this );

    Label crsLabel = new Label( fileGroup, SWT.NONE );
    crsLabel.setText( "CRS: " );

    m_checkCRS = new Combo( fileGroup, SWT.DEFAULT );

    try
    {
      m_checkCRS.add( KalypsoGisPlugin.getDefault().getCoordinatesSystem().getName() );
      initializeCRS( m_checkCRS );
      m_checkCRS.select( 0 );
    }
    catch( RemoteException e )
    {
      e.printStackTrace();
      //do nothing ???
    }
    m_checkCRS.setToolTipText( "Coordinatensystem der ESRI(tm) Shape Datei" );
    //checkCRS.setEnabled(true);
    GridData data = new GridData( GridData.FILL_HORIZONTAL );
    data.widthHint = SIZING_TEXT_FIELD_WIDTH;
    m_checkCRS.setLayoutData( data );
    m_checkCRS.addSelectionListener( this );
    //  add spacer
    Label label = new Label( fileGroup, SWT.SEPARATOR | SWT.HORIZONTAL );
    label.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 3, 3 ) );

    toMapLabel = new Label( fileGroup, SWT.NONE );
    toMapLabel.setText( "Karte: " );

    toMapText = new Text( fileGroup, SWT.BORDER );
    GridData data1 = new GridData( GridData.FILL_HORIZONTAL );
    data1.widthHint = SIZING_TEXT_FIELD_WIDTH;
    toMapText.setLayoutData( data1 );

    toMapBrowse = new Button( fileGroup, SWT.PUSH );
    toMapBrowse.setText( "Durchsuchen..." );
    toMapBrowse
        .setToolTipText( "Die Karte wählen zu welcher die Shape Datei hinzugefügt werden soll" );
    toMapBrowse.addSelectionListener( this );

    checkToMap = new Button( fileGroup, SWT.CHECK );
    checkToMap.setText( "Zu neuer Karte Hinzufügen" );
    checkToMap.setSelection( true );
    checkToMap.setToolTipText( "Wenn nicht aktiv, neue Karte erzeugen." );
    checkToMap.addSelectionListener( this );
    fileGroup.pack();

  }

  private void initializeCRS( Combo checkCRS )
  {
    // TODO Initialize the Combo with availabel CRS Systems (for possible
    // Transformation)
  }

  public boolean validateFile( URL url )
  {
    try
    {
      InputStream ios = url.openStream();
      ios.close();
    }
    catch( IOException e )
    {
      e.printStackTrace();
      return false;
    }
    return true;
  }

  public String getShapeBase()
  {
    String path1 = FileUtilities.nameWithoutExtension( fileURL.getPath() );
    path1 = path1.substring( 1 );
    return path1;
  }

  public URL getURL()
  {
    return fileURL;
  }

  public CS_CoordinateSystem getCoordinateSystem()
  {
    if( customCS != null )
      return customCS;
    return defaultCS;
  }

  /**
   * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
   */
  public void widgetSelected( SelectionEvent e )
  {
    Combo c;
    Button b;
    if( e.widget instanceof Button )
    {
      b = (Button)e.widget;
      if( b.equals( projectBrowse ) )
      {

        ContainerSelectionDialog dialog2 = new ContainerSelectionDialog( getContainer().getShell(),
            m_projects[0], false, "Message" );
        dialog2.showClosedProjects( false );
        dialog2.setTitle( "Projekt und Ordner auswählen" );
        dialog2.setMessage( "Auswählen des Ziel Verzeichnisses der ESRI(tm) Shape-Datei" );
        dialog2.open();
        Object[] result = dialog2.getResult();
        m_path = (Path)result[0];
        m_relativeProjectPath = "project:/" + m_path.removeFirstSegments( 1 ).toString() + "/";
        projectTextField.setText( m_path.toString() );
        if( checkToMap.getSelection() )
          toMapText.setText( m_path.toString() + "/new.gmt" );
        else
        {
          toMapText.setVisible( false );
          toMapText.setEnabled( false );
        }
      }
      if( b.equals( browseButton ) )
      {
        FileDialog fdialog = new FileDialog( getShell(), SWT.OPEN | SWT.SINGLE );
        fdialog.setFilterExtensions( new String[]
        {
          "shp"
        } );
        fdialog.setText( "Wählen Sie eine ESRI Arc-View Shapedatei aus:" );
        fdialog.setFilterNames( new String[]
        {
            "Shape Files",
            "All Files (*.*)"
        } );
        fdialog.setFilterExtensions( new String[]
        {
            "*.shp",
            "*.*"
        } );
        fdialog.setFileName( "*.shp" );
        if( fdialog.open() != null )
        {
          String textStr;
          try
          {

            fileURL = ( new File( fdialog.getFilterPath() + File.separator + fdialog.getFileName() ) )
                .toURL();
            textStr = fileURL.toExternalForm();
            textField.setText( textStr );
            if( projectTextField.getText().length() < 1 )
              setPageComplete( false );
            //copyShapeFile(fileURL);

          }
          catch( MalformedURLException ex )
          {
            ex.printStackTrace();
          }

        }//if dialog

      }
      if( b.equals( checkToMap ) )
      {
        if( checkToMap.getSelection() )
        {
          //TODO einbleden der Karten selection
          toMapText.setEditable( false );
          setPageComplete( true );
        }
        else
        {
          toMapText.setText( "" );
          toMapText.setEditable( true );
          //TODO add to default Map directory
        }

      }
      if( b.equals( toMapBrowse ) )
      {
        IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
        ResourceListSelectionDialog dialog = new ResourceListSelectionDialog( getControl()
            .getShell(), root, IResource.FILE );
        dialog.open();
        dialog.getResult();
      }
    }
    if( e.widget instanceof Combo )
    {
      c = (Combo)e.widget;
      if( c == m_checkCRS )
      {
        // TODO check if CRS is availabel (Geotransfrom)
      }
    }

  }

  /**
   * @see org.eclipse.swt.events.SelectionListener#widgetDefaultSelected(org.eclipse.swt.events.SelectionEvent)
   */
  public void widgetDefaultSelected( SelectionEvent e )
  {
    //not implemented
  }

  /**
   * @see org.eclipse.swt.events.ModifyListener#modifyText(org.eclipse.swt.events.ModifyEvent)
   */
  public void modifyText( ModifyEvent e )
  {
    if( e.widget == textField )
    {
      if( textField.getText().length() == 0 )
      {
        setErrorMessage( "Bitte eine Datei auswählen!" );
        setPageComplete( false );
      }
      else if( checkSuffix( textField ) == false )
      {
        setErrorMessage( "Falscher Suffix Datei, muss \"shp\" sein!" );
        setPageComplete( false );
      }
      else if( validateFile( fileURL ) == false )
      {
        setErrorMessage( "Gewählte Shape-Datei nicht gültg" );
        setPageComplete( false );
      }
      if( projectTextField.getText().length() < 1 )
        setPageComplete( false );
      setPageComplete( true );
      setErrorMessage( null );
      setMessage( null );
    }

  }

  public IPath getWorkspaceLocation()
  {
    return m_path;
  }

  public CS_CoordinateSystem getCRS()
  {
    return ConvenienceCSFactory.getInstance().getOGCCSByName( m_checkCRS.getText() );
  }

  public String makeProjectPath( IPath eclipsePath )
  {
    String res = null;
    res = "project:/" + eclipsePath.removeFirstSegments( 1 ).toString() + "/";
    return res;

  }

  public String getRelativeProjectPath()
  {
    return m_relativeProjectPath;
  }

  public void setProjectSelection( IProject[] project )
  {
    m_projects = project;
  }

}

