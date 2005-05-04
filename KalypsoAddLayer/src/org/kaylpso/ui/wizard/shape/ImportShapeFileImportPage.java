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

import org.eclipse.core.internal.resources.PlatformURLResourceConnection;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
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
import org.eclipse.swt.widgets.Widget;
import org.eclipse.ui.dialogs.ContainerSelectionDialog;
import org.eclipse.ui.dialogs.ResourceListSelectionDialog;
import org.kalypso.java.io.FileUtilities;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author kuepfer
 *  
 */
public class ImportShapeFileImportPage extends WizardPage implements SelectionListener,
    ModifyListener, KeyListener, FocusListener
{

  //constants
  private static final int SIZING_TEXT_FIELD_WIDTH = 250;

  //widgets
  private Group m_group;

  private Label m_sourceFileLabel;

  private Text m_sourceFileText;

  private Composite m_topComposite;

  private Button m_browseButton;

  //Geodata
  private CS_CoordinateSystem m_customCS = null;

  private CS_CoordinateSystem m_defaultCS = ConvenienceCSFactory.getInstance().getOGCCSByName(
      "EPSG:31469" );

  private URL m_sourceFileURL;

  //mapping

  private Label m_projectLabel;

  private Text m_projectTextField;

  private Button m_projectBrowse;

  private IPath m_relativeContextPath;

  private Combo m_checkCRS;

  private Button m_checkToMap;

  private Text m_toMapText;

  private Label m_toMapLabel;

  private Button m_toMapBrowse;

  private IProject m_projects = null;

  private URL m_activeMapContext = null;

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

    m_topComposite = new Composite( parent, SWT.NULL );
    m_topComposite.setFont( parent.getFont() );

    initializeDialogUnits( parent );

    //		WorkbenchHelp.setHelp(topComposite,
    //				IHelpContextIds.NEW_PROJECT_WIZARD_PAGE);

    m_topComposite.setLayout( new GridLayout() );
    m_topComposite.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    // build wizard page
    createFileGroup( m_topComposite );
    setControl( m_topComposite );

  }

  private void createFileGroup( Composite parent )
  {
    m_group = new Group( parent, SWT.NULL );
    GridLayout topGroupLayout = new GridLayout();
    GridData topGroupData = new GridData();
    topGroupLayout.numColumns = 3;
    topGroupData.horizontalAlignment = GridData.FILL;
    m_group.setLayout( topGroupLayout );
    m_group.setLayoutData( topGroupData );
    m_group.setText( "Shape-Datei" );
    m_sourceFileLabel = new Label( m_group, SWT.NONE );
    m_sourceFileLabel.setText( "Quelle : " );

    // Set width of source path field
    GridData data0 = new GridData( GridData.FILL_HORIZONTAL );
    data0.widthHint = SIZING_TEXT_FIELD_WIDTH;

    m_sourceFileText = new Text( m_group, SWT.BORDER );
    m_sourceFileText.setLayoutData( data0 );
    m_sourceFileText.setEditable( false );
    m_sourceFileText.addModifyListener( this );

    m_browseButton = new Button( m_group, SWT.PUSH );
    m_browseButton.setText( "Durchsuchen..." );
    m_browseButton.setLayoutData( new GridData( GridData.END ) );
    m_browseButton.addSelectionListener( this );

    m_projectLabel = new Label( m_group, SWT.NONE );
    m_projectLabel.setText( "Ziel: " );
    m_projectTextField = new Text( m_group, SWT.BORDER );
    m_projectTextField.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    m_projectTextField.setEditable( true );
    m_projectBrowse = new Button( m_group, SWT.PUSH );
    m_projectBrowse.setText( "Durchsuchen..." );
    m_projectBrowse.addSelectionListener( this );

    Label crsLabel = new Label( m_group, SWT.NONE );
    crsLabel.setText( "Coordinaten System: " );

    m_checkCRS = new Combo( m_group, SWT.NONE );

    availableCoordinateSystems( m_checkCRS );
    m_checkCRS.select( 0 );

    m_checkCRS.setToolTipText( "Coordinatensystem der ESRI(tm) Shape Datei" );
    //    m_checkCRS.setEnabled(true);
    GridData data = new GridData( GridData.FILL_HORIZONTAL );
    data.widthHint = SIZING_TEXT_FIELD_WIDTH;
    m_checkCRS.setLayoutData( data );
    m_checkCRS.addSelectionListener( this );
    m_checkCRS.addFocusListener( this );
    //    m_checkCRS.addKeyListener( this );
    //  add spacer
    Label label = new Label( m_group, SWT.SEPARATOR | SWT.HORIZONTAL );
    label.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 3, 3 ) );

    m_toMapLabel = new Label( m_group, SWT.NONE );
    m_toMapLabel.setText( "Karte: " );

    m_toMapText = new Text( m_group, SWT.BORDER );
    GridData data1 = new GridData( GridData.FILL_HORIZONTAL );
    data1.widthHint = SIZING_TEXT_FIELD_WIDTH;
    m_toMapText.setLayoutData( data1 );
    m_toMapText.setEditable( false );
    if( m_activeMapContext != null )
      m_toMapText.setText( m_activeMapContext.getFile() );
    else
      m_toMapText.setText( "leereKarte" );

    m_toMapBrowse = new Button( m_group, SWT.PUSH );
    m_toMapBrowse.setText( "Durchsuchen..." );
    m_toMapBrowse.setToolTipText( "Namen der neuen Karte eingeben." );
    m_toMapBrowse.addSelectionListener( this );
    m_toMapBrowse.setEnabled( false );

    m_checkToMap = new Button( m_group, SWT.CHECK );
    m_checkToMap.setText( "Neue Karte" );
    m_checkToMap.setSelection( false );
    m_checkToMap.setVisible( true );
    m_checkToMap.setToolTipText( "Erzeugt eine neue Karte im aktiven Projektwurzelverzeichnis" );
    m_checkToMap.addSelectionListener( this );
    m_group.pack();

  }

  private void availableCoordinateSystems( Combo checkCRS )
  {
    ConvenienceCSFactoryFull factory = new ConvenienceCSFactoryFull();
    checkCRS.setItems( factory.getKnownCS() );
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
    String path1 = FileUtilities.nameWithoutExtension( m_sourceFileURL.getPath() );
    path1 = path1.substring( 1 );
    return path1;
  }

  //  public URL getSourceFileURL()
  //  {
  //    return m_sourceFileURL;
  //  }

  public CS_CoordinateSystem getCoordinateSystem()
  {
    if( m_customCS != null )
      return m_customCS;
    return m_defaultCS;
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
      if( b.equals( m_projectBrowse ) )
      {

        ContainerSelectionDialog dialog2 = new ContainerSelectionDialog( getContainer().getShell(),
            m_projects, false, "Message" );
        dialog2.showClosedProjects( false );
        dialog2.setTitle( "Projekt und Ordner auswählen" );
        dialog2.setMessage( "Auswählen des Ziel Verzeichnisses der ESRI(tm) Shape-Datei" );
        dialog2.open();
        Object[] result = dialog2.getResult();
        m_relativeContextPath = (Path)result[0];
        m_projectTextField.setText( m_relativeContextPath.toString() );
        if( m_checkToMap.getSelection() )
        {
          //          checkToMap.setVisible( true );
          m_toMapText.setEnabled( true );
          m_toMapText.setEditable( true );
          setPageComplete( checkMapPath() );
        }
        else
        {
          //          toMapText.setVisible( false );
          m_toMapText.setEnabled( false );
          m_toMapText.setEditable( false );
          setPageComplete( true );
        }

      }
      if( b.equals( m_browseButton ) )
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

            m_sourceFileURL = ( new File( fdialog.getFilterPath() + File.separator
                + fdialog.getFileName() ) ).toURL();
            textStr = m_sourceFileURL.toExternalForm();
            m_sourceFileText.setText( textStr );
            if( m_projectTextField.getText().length() < 1 )
              setPageComplete( false );
            else
              setPageComplete( true );
            //copyShapeFile(fileURL);

          }
          catch( MalformedURLException ex )
          {
            ex.printStackTrace();
          }

        }//if dialog

      }
      if( b.equals( m_checkToMap ) )
      {
        if( m_checkToMap.getSelection() )
        {
          //TODO einbleden der Karten selection
          m_toMapBrowse.setEnabled( true );
          m_toMapText.setEnabled( true );
          m_toMapText.setEditable( true );
          if( m_projects != null )
            m_toMapText.setText( m_projects.getFullPath().toString() );
          setPageComplete( checkMapPath() );

        }
        else
        {
          m_toMapBrowse.setEnabled( false );
          m_toMapText.setEditable( false );
          m_toMapText.setEnabled( false );
          if( m_projects != null )
            m_toMapText.setText( m_projects.getFullPath().toString() );
          setPageComplete( true );
        }

      }
      if( b.equals( m_toMapBrowse ) )
      {
        IContainer container = null;
        if( m_projects != null )
          container = m_projects;
        else
          container = ResourcesPlugin.getWorkspace().getRoot();
        ResourceListSelectionDialog dialog = new ResourceListSelectionDialog( getControl()
            .getShell(), container, IResource.FILE );
        dialog.open();
        Object[] selection = dialog.getResult();
        if( selection.length > 0 )
        {
          //TODO
        }

      }
    }
    if( e.widget instanceof Combo )
    {
      c = (Combo)e.widget;
      if( c == m_checkCRS )
      {
        m_checkCRS.setEnabled( true );
      }
    }

  }

  private boolean checkMapPath()
  {
    boolean check = false;
    Path mapPath = new Path( m_toMapText.getText() );
    //TODO implement check
    check = mapPath.isValidPath( m_toMapBrowse.getText() );
    check = ResourcesPlugin.getWorkspace().getRoot().exists( mapPath );
    if( m_toMapText.getText().length() > 0 )
      check = true;
    else
      check = false;
    return check;
  }

  /**
   * @see org.eclipse.swt.events.SelectionListener#widgetDefaultSelected(org.eclipse.swt.events.SelectionEvent)
   */
  public void widgetDefaultSelected( SelectionEvent e )
  {
    //no default selection
  }

  /**
   * @see org.eclipse.swt.events.ModifyListener#modifyText(org.eclipse.swt.events.ModifyEvent)
   */
  public void modifyText( ModifyEvent e )
  {
    if( e.widget == m_sourceFileText )
    {
      if( m_sourceFileText.getText().length() == 0 )
      {
        setErrorMessage( "Bitte eine Datei auswählen!" );
        setPageComplete( false );
      }
      else if( checkSuffix( m_sourceFileText ) == false )
      {
        setErrorMessage( "Falscher Suffix Datei, muss \"shp\" sein!" );
        setPageComplete( false );
      }
      else if( validateFile( m_sourceFileURL ) == false )
      {
        setErrorMessage( "Gewählte Shape-Datei nicht gültg" );
        setPageComplete( false );
      }
      if( m_projectTextField.getText().length() < 1 )
        setPageComplete( false );
      setPageComplete( true );
      setErrorMessage( null );
      setMessage( null );
    }

  }

  public IPath getProjectWorkspaceLocation()
  {
    return m_relativeContextPath;
  }

  public CS_CoordinateSystem getCRS()
  {
    return ConvenienceCSFactory.getInstance().getOGCCSByName( m_checkCRS.getText() );
  }

  private String makeProjectPath( IPath eclipsePath )
  {
    String res = null;
    res = "project:/" + eclipsePath.removeFirstSegments( 1 ).toString() + "/";
    return res;

  }

  public URL getResourceContextURL() throws MalformedURLException
  {
    return new URL( PlatformURLResourceConnection.RESOURCE_URL_STRING + "/" + m_projects.getName()
        + "/" + m_toMapText.getText() + ".gmt" );

  }

  protected void setProjectSelection( IProject project )
  {
    m_projects = project;
  }

  public String getRelativeProjectContext()
  {
    String projectPath = makeProjectPath( m_relativeContextPath );
    return projectPath;
  }

  public boolean isNewMapRequested()
  {
    return m_checkToMap.getSelection();
  }

  public String getAbsoluteMapContext()
  {
    IPath root = ResourcesPlugin.getWorkspace().getRoot().getLocation();
    return root.append( m_relativeContextPath.append( "/" + m_toMapText.getText() ) ).toString();
  }

  public URL getDefaultMapContextURL() throws MalformedURLException
  {
    return ( ( ResourcesPlugin.getWorkspace().getRoot().getLocation().append( m_projects
        .getFullPath()
        + "/" + m_toMapText.getText() + ".gmt" ) ).toFile() ).toURL();
  }

  public void setMapContextURL( URL context )
  {
    m_activeMapContext = context;
  }

  /**
   * @see org.eclipse.swt.events.KeyListener#keyPressed(org.eclipse.swt.events.KeyEvent)
   */
  public void keyPressed( KeyEvent e )
  {
    // do nothing
  }

  /**
   * @see org.eclipse.swt.events.KeyListener#keyReleased(org.eclipse.swt.events.KeyEvent)
   */
  public void keyReleased( KeyEvent e )
  {
    Widget w = e.widget;
    if( w == m_checkCRS && e.keyCode == SWT.DefaultSelection )
    {

      setPageComplete( checkCRS( ( (Combo)w ).getText() ) );
    }
    setPageComplete( false );
  }

  private boolean checkCRS( String customCRS )
  {
    for( int i = 0; i < m_checkCRS.getItems().length; i++ )
    {
      String name = m_checkCRS.getItems()[i];
      if( name.equals( customCRS ) )
        return true;
    }
    return false;
  }

  /**
   * @see org.eclipse.swt.events.FocusListener#focusGained(org.eclipse.swt.events.FocusEvent)
   */
  public void focusGained( FocusEvent e )
  {
    Widget w = e.widget;
    if( w == m_checkCRS )
    {
      m_checkCRS.setEnabled( true );
    }
  }

  /**
   * @see org.eclipse.swt.events.FocusListener#focusLost(org.eclipse.swt.events.FocusEvent)
   */
  public void focusLost( FocusEvent e )
  {
    Widget w = e.widget;
    if( w == m_checkCRS )
    {
      for( int i = 0; i < m_checkCRS.getItems().length; i++ )
      {
        String name = m_checkCRS.getItems()[i];
        if( name.equals( m_checkCRS.getText() ) )
        {
          setMessage( null );
          setPageComplete( true );
          return;
        }

      }
      setMessage( "Das gewählte Coodrdinaten System wird nicht unterstüzt." );
      setPageComplete( false );
    }
  }

  public void removeListeners()
  {
    m_browseButton.removeSelectionListener(this);
    m_checkCRS.removeSelectionListener(this);
    m_projectBrowse.removeSelectionListener(this);
    m_projectTextField.removeModifyListener(this);
    m_toMapBrowse.removeSelectionListener(this);
  }
}

