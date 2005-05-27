/*
 * --------------- Kalypso-Header
 * --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal
 * engineering Denickestr. 22 21073 Hamburg, Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany
 * http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kaylpso.ui.wizard.image;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.rmi.RemoteException;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.kaylpso.ui.dialog.KalypsoResourceSelectionDialog;

/**
 * ImportImageWizardPage
 * <p>
 * 
 * created by
 * 
 * @author kuepfer (21.05.2005)
 */
public class ImportImageWizardPage extends WizardPage implements SelectionListener, KeyListener
{
  //constants
  private static final int SIZING_TEXT_FIELD_WIDTH = 250;

  private Group m_group;

  private Button m_browseButton;

  private Text m_sourceFileText;

  private IPath m_relativeSourcePath;

  private IProject m_project;

  private Composite m_topComposite;

  private Text m_textDx;

  private Text m_textPhix;

  private Text m_textPhiy;

  private Text m_textDy;

  private Text m_textULCy;

  private Text m_textULCx;

  private Group m_worldFileGroup;

  private String m_fileType;

  private String m_wfType;

  private boolean m_wfExists;

  private Combo m_CS;

  protected ImportImageWizardPage( String pageName, String title, ImageDescriptor titleImage )
  {
    super( pageName, title, titleImage );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( Composite parent )
  {
    m_topComposite = new Composite( parent, SWT.NULL );
    m_topComposite.setFont( parent.getFont() );
    m_topComposite.setLayout( new GridLayout() );
    m_topComposite.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    initializeDialogUnits( parent );

    m_group = new Group( m_topComposite, SWT.NULL );
    GridLayout topGroupLayout = new GridLayout();
    GridData topGroupData = new GridData();
    topGroupLayout.numColumns = 3;
    topGroupData.horizontalAlignment = GridData.FILL;
    m_group.setLayout( topGroupLayout );
    m_group.setLayoutData( topGroupData );
    m_group.setText( "Bild-Datei" );
    Label sourceFileLabel = new Label( m_group, SWT.NONE );
    sourceFileLabel.setText( "Quelle : " );

    // Set width of source path field
    GridData data0 = new GridData( GridData.FILL_HORIZONTAL );
    data0.widthHint = SIZING_TEXT_FIELD_WIDTH;

    m_sourceFileText = new Text( m_group, SWT.BORDER );
    m_sourceFileText.setLayoutData( data0 );
    m_sourceFileText.setEditable( false );

    m_browseButton = new Button( m_group, SWT.PUSH );
    m_browseButton.setText( "Durchsuchen..." );
    m_browseButton.setLayoutData( new GridData( GridData.END ) );
    m_browseButton.addSelectionListener( this );
    Label crsLabel = new Label( m_group, SWT.NONE );
    crsLabel.setText( "Koordinaten System: " );

    m_CS = new Combo( m_group, SWT.NONE );

    availableCoordinateSystems( m_CS );
    try
    {
      String defaultCS = KalypsoGisPlugin.getDefault().getCoordinatesSystem().getName();
      m_CS.select( m_CS.indexOf( defaultCS ) );
    }
    catch( RemoteException e1 )
    {
      e1.printStackTrace();
    }

    m_CS.setToolTipText( "Koordinatensystem der World Datei" );
    createWorldFilePanel( m_topComposite );
    //    m_group.pack();
    setControl( m_topComposite );

  }

  private void availableCoordinateSystems( Combo checkCRS )
  {
    ConvenienceCSFactoryFull factory = new ConvenienceCSFactoryFull();
    checkCRS.setItems( factory.getKnownCS() );
  }

  private void createWorldFilePanel( Composite composite )
  {

    m_worldFileGroup = new Group( composite, SWT.NULL );
    m_worldFileGroup.setText( "World file:" );
    GridLayout wfGroupLayout = new GridLayout();
    GridData wfGroupData = new GridData();
    wfGroupData.grabExcessHorizontalSpace = true;
    wfGroupLayout.numColumns = 2;
    wfGroupData.horizontalAlignment = GridData.FILL;
    m_worldFileGroup.setLayout( wfGroupLayout );
    m_worldFileGroup.setLayoutData( wfGroupData );

    Label dx = new Label( m_worldFileGroup, SWT.NONE );
    dx.setText( "Pixel (dx):" );
    m_textDx = new Text( m_worldFileGroup, SWT.BORDER );
    m_textDx.setEditable( false );
    m_textDx.addKeyListener( this );

    Label phix = new Label( m_worldFileGroup, SWT.NONE );
    phix.setText( "phi X:" );
    m_textPhix = new Text( m_worldFileGroup, SWT.BORDER );
    m_textPhix.setEditable( false );
    m_textPhix.addKeyListener( this );

    Label phiy = new Label( m_worldFileGroup, SWT.NONE );
    phiy.setText( "phi Y:" );
    m_textPhiy = new Text( m_worldFileGroup, SWT.BORDER );
    m_textPhiy.setEditable( false );
    m_textPhiy.addKeyListener( this );

    Label dy = new Label( m_worldFileGroup, SWT.NONE );
    dy.setText( "Pixel (dy):" );
    m_textDy = new Text( m_worldFileGroup, SWT.BORDER );
    m_textDy.setEditable( false );
    m_textDy.addKeyListener( this );

    Label ulcx = new Label( m_worldFileGroup, SWT.NONE );
    ulcx.setText( "Obere linke Ecke (X):" );
    m_textULCx = new Text( m_worldFileGroup, SWT.BORDER );
    m_textULCx.setEditable( false );
    m_textULCx.addKeyListener( this );

    Label ulcy = new Label( m_worldFileGroup, SWT.NONE );
    ulcy.setText( "Obere linke Ecke (Y):" );
    m_textULCy = new Text( m_worldFileGroup, SWT.BORDER );
    m_textULCy.setEditable( false );
    m_textULCy.addKeyListener( this );
    m_worldFileGroup.setVisible( false );

  }

  /**
   * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
   */
  public void widgetSelected( SelectionEvent e )
  {
    Button b;
    if( e.widget instanceof Button )
    {
      b = (Button)e.widget;
      if( b.equals( m_browseButton ) )
      {
        KalypsoResourceSelectionDialog dialog = createResourceDialog( new String[]
        {
            "tiff",
            "tif",
            "jpg",
            "TIFF",
            "TIF",
            "JPG",
            "png",
            "PNG",});
            //"gif",
            //"GIF"} );
        dialog.open();
        Object[] result = dialog.getResult();
        if( result != null )
        {
          Path resultPath = (Path)result[0];
          m_sourceFileText.setText( resultPath.toString() );
          m_relativeSourcePath = resultPath;
        }
        updateWorldFile();
      }
    }
    validate();

  }

  private void updateWorldFile()
  {
    IPath path = null;
    if( m_relativeSourcePath.getFileExtension().toUpperCase().equals( "TIF" ) )
    {
      m_fileType = "tif";
      m_wfType = "tfw";
      path = m_relativeSourcePath.removeFileExtension().addFileExtension( m_wfType )
          .removeFirstSegments( 1 );
    }
    if( m_relativeSourcePath.getFileExtension().toUpperCase().equals( "JPG" ) )
    {
      m_fileType = "jpg";
      m_wfType = "jgw";
      path = m_relativeSourcePath.removeFileExtension().addFileExtension( m_wfType )
          .removeFirstSegments( 1 );
    }
    if( m_relativeSourcePath.getFileExtension().toUpperCase().equals( "PNG" ) )
    {
      m_fileType = "png";
      m_wfType = "pgw";
      path = m_relativeSourcePath.removeFileExtension().addFileExtension( m_wfType )
          .removeFirstSegments( 1 );
    }
    //if( m_relativeSourcePath.getFileExtension().toUpperCase().equals( "GIF" ) )
    //{
    //  m_fileType = "gif";
    //  m_wfType = "gfw";
    //  path = m_relativeSourcePath.removeFileExtension().addFileExtension( m_wfType )
    //      .removeFirstSegments( 1 );
    //}
    try
    {
      URL worldFile = m_project.getLocation().append( path ).toFile().toURL();
      InputStream is = worldFile.openStream();

      BufferedReader br = new BufferedReader( new InputStreamReader( is ) );
      m_worldFileGroup.setVisible( true );
      m_textDx.setText( br.readLine().trim() );
      m_textPhix.setText( br.readLine().trim() );
      m_textPhiy.setText( br.readLine().trim() );
      m_textDy.setText( br.readLine().trim() );
      m_textULCx.setText( br.readLine().trim() );
      m_textULCy.setText( br.readLine().trim() );
      m_worldFileGroup.setVisible( true );
      m_worldFileGroup.pack();
      m_topComposite.pack();
      m_wfExists = true;
      setPageComplete( true );

      //      else
      //      {
      //        m_worldFileGroup.setVisible( true );
      //        m_textDx.setEditable( true );
      //        m_textDy.setEditable( true );
      //        m_textPhix.setEditable( true );
      //        m_textPhiy.setEditable( true );
      //        m_textULCx.setEditable( true );
      //        m_textULCy.setEditable( true );
      //        m_group.pack();
      //        m_wfExists = false;
      //        setPageComplete( false );
      //      }
    }
    catch( IOException e )
    {
      m_worldFileGroup.setVisible( true );
      m_textDx.setEditable( true );
      m_textDy.setEditable( true );
      m_textPhix.setEditable( true );
      m_textPhiy.setEditable( true );
      m_textULCx.setEditable( true );
      m_textULCy.setEditable( true );
      m_group.pack();
      m_wfExists = false;
      e.printStackTrace();
      setErrorMessage( "Probleme: World file hat falsche Endung, Namen stimmen nicht überein,\n kein world file vorhanden. Bitte geben Sie die unten Werte ein." );
      setPageComplete( false );
    }

  }

  private boolean validate()
  {

    try
    {
      Double.parseDouble( m_textDx.getText().trim() );
      Double.parseDouble( m_textDy.getText().trim() );
      Double.parseDouble( m_textPhix.getText().trim() );
      Double.parseDouble( m_textPhiy.getText().trim() );
      Double.parseDouble( m_textULCx.getText().trim() );
      Double.parseDouble( m_textULCy.getText().trim() );
    }
    catch( NumberFormatException e )
    {
      setErrorMessage( "Bitte geben sie alle Zahlen unten ein." );
      return false;
    }
    if( !m_wfExists )
      try
      {
        IFile worldfile = m_project.getFile( m_relativeSourcePath.removeFirstSegments( 1 )
            .removeFileExtension().addFileExtension( m_wfType ) );

        String str = "";
        Control[] array = m_worldFileGroup.getChildren();
        for( int i = 0; i < array.length; i++ )
        {
          Control combo = array[i];
          if( combo instanceof Text )
            str = str + ( ( (Text)combo ).getText().trim() + "\n" );
        }

        ByteArrayInputStream bis = new ByteArrayInputStream( str.getBytes() );
        worldfile.create( bis, false, null );
      }
      catch( CoreException e )
      {
        e.printStackTrace();
        setErrorMessage( "Error while writing world file" );
        return false;
      }
    if( m_CS.getText() != null || m_CS.getText() != "" )
    {
      // ok
    }
    else
    {
      setErrorMessage( "Bitte wählen sie ein gültigers Coordinaten System aus." );
      setPageComplete( false );
    }
    setErrorMessage( null );
    return true;
  }

  /**
   * @see org.eclipse.swt.events.SelectionListener#widgetDefaultSelected(org.eclipse.swt.events.SelectionEvent)
   */
  public void widgetDefaultSelected( SelectionEvent e )
  {
  // nothing to do

  }

  KalypsoResourceSelectionDialog createResourceDialog( String[] fileResourceExtensions )
  {
    return new KalypsoResourceSelectionDialog( getShell(), m_project, "Select resource",
        fileResourceExtensions, m_project );
  }

  public void setProjectSelection( IProject project )
  {
    m_project = project;

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

    if( ( e.widget == m_textDx || e.widget == m_textDy || e.widget == m_textPhix
        || e.widget == m_textPhiy || e.widget == m_textULCx || e.widget == m_textULCy )
        && e.character == SWT.CR )
    {
      setPageComplete( validate() );
    }

  }

  public URL getURL() throws MalformedURLException
  {

    return m_project.getLocation().append( m_relativeSourcePath.removeFirstSegments( 1 ) ).toFile()
        .toURL();
  }

  public String getFileType()
  {
    return m_fileType;
  }

  public boolean isWorldFileExisting()
  {
    return m_wfExists;
  }

  public IPath getRelativeSourcePath()
  {
    return m_relativeSourcePath;
  }

  public void dispose()
  {
    super.dispose();
    m_textDx.removeKeyListener( this );
    m_textDy.removeKeyListener( this );
    m_textPhix.removeKeyListener( this );
    m_textPhiy.removeKeyListener( this );
    m_textULCx.removeKeyListener( this );
    m_textULCy.removeKeyListener( this );
    m_browseButton.removeSelectionListener( this );

  }

  public String getCSName()
  {

    return m_CS.getText();
  }
}