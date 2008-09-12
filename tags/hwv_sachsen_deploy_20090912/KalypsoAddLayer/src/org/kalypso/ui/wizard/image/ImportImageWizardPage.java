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
package org.kalypso.ui.wizard.image;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;

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
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.contribs.eclipse.ui.dialogs.KalypsoResourceSelectionDialog;
import org.kalypso.contribs.eclipse.ui.dialogs.ResourceSelectionValidator;
import org.kalypso.transformation.CRSHelper;
import org.kalypso.transformation.ui.CRSSelectionListener;
import org.kalypso.transformation.ui.CRSSelectionPanel;

/**
 * ImportImageWizardPage
 * <p>
 * created by
 * 
 * @author kuepfer (21.05.2005)
 */
public class ImportImageWizardPage extends WizardPage implements SelectionListener, KeyListener
{
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

  private CRSSelectionPanel m_crsPanel;

  public ImportImageWizardPage( String pageName, String title, ImageDescriptor titleImage )
  {
    super( pageName, title, titleImage );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( Composite parent )
  {
    m_topComposite = new Composite( parent, SWT.NONE );
    m_topComposite.setFont( parent.getFont() );
    m_topComposite.setLayout( new GridLayout( 1, false ) );
    m_topComposite.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    initializeDialogUnits( parent );

    /* File group. */
    m_group = new Group( m_topComposite, SWT.NONE );
    m_group.setLayout( new GridLayout( 3, false ) );
    m_group.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    m_group.setText( "Bild-Datei" );

    Label sourceFileLabel = new Label( m_group, SWT.NONE );
    sourceFileLabel.setText( "Quelle : " );

    m_sourceFileText = new Text( m_group, SWT.BORDER );
    m_sourceFileText.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    m_sourceFileText.setEditable( false );

    m_browseButton = new Button( m_group, SWT.PUSH );
    m_browseButton.setText( "Durchsuchen..." );
    m_browseButton.setLayoutData( new GridData( GridData.END ) );
    m_browseButton.addSelectionListener( this );

    /* CRS panel. */
    m_crsPanel = new CRSSelectionPanel( m_topComposite, SWT.NONE );
    m_crsPanel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    m_crsPanel.addSelectionChangedListener( new CRSSelectionListener()
    {
      /**
       * @see org.kalypso.transformation.ui.CRSSelectionListener#selectionChanged(java.lang.String)
       */
      @Override
      protected void selectionChanged( String selectedCRS )
      {
        setPageComplete( validate() );
      }
    } );

    /* World file panel. */
    createWorldFilePanel( m_topComposite );

    // m_group.pack();

    setControl( m_topComposite );
  }

  private void createWorldFilePanel( Composite composite )
  {
    m_worldFileGroup = new Group( composite, SWT.NONE );
    m_worldFileGroup.setText( "World file:" );
    m_worldFileGroup.setLayout( new GridLayout( 2, false ) );
    m_worldFileGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    Label dx = new Label( m_worldFileGroup, SWT.NONE );
    dx.setText( "Pixel (dx):" );
    m_textDx = new Text( m_worldFileGroup, SWT.BORDER );
    m_textDx.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    m_textDx.setEditable( false );
    m_textDx.addKeyListener( this );

    Label phix = new Label( m_worldFileGroup, SWT.NONE );
    phix.setText( "phi X:" );
    m_textPhix = new Text( m_worldFileGroup, SWT.BORDER );
    m_textPhix.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    m_textPhix.setEditable( false );
    m_textPhix.addKeyListener( this );

    Label phiy = new Label( m_worldFileGroup, SWT.NONE );
    phiy.setText( "phi Y:" );
    m_textPhiy = new Text( m_worldFileGroup, SWT.BORDER );
    m_textPhiy.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    m_textPhiy.setEditable( false );
    m_textPhiy.addKeyListener( this );

    Label dy = new Label( m_worldFileGroup, SWT.NONE );
    dy.setText( "Pixel (dy):" );
    m_textDy = new Text( m_worldFileGroup, SWT.BORDER );
    m_textDy.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    m_textDy.setEditable( false );
    m_textDy.addKeyListener( this );

    Label ulcx = new Label( m_worldFileGroup, SWT.NONE );
    ulcx.setText( "Obere linke Ecke (X):" );
    m_textULCx = new Text( m_worldFileGroup, SWT.BORDER );
    m_textULCx.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    m_textULCx.setEditable( false );
    m_textULCx.addKeyListener( this );

    Label ulcy = new Label( m_worldFileGroup, SWT.NONE );
    ulcy.setText( "Obere linke Ecke (Y):" );
    m_textULCy = new Text( m_worldFileGroup, SWT.BORDER );
    m_textULCy.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    m_textULCy.setEditable( false );
    m_textULCy.addKeyListener( this );

    /* Initial invisible. */
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
      b = (Button) e.widget;
      if( b.equals( m_browseButton ) )
      {
        KalypsoResourceSelectionDialog dialog = createResourceDialog( new String[] { "tiff", "tif", "jpg", "TIFF", "TIF", "JPG", "png", "PNG", } );
        // "gif",
        // "GIF"} );
        dialog.open();
        Object[] result = dialog.getResult();
        if( result != null )
        {
          Path resultPath = (Path) result[0];
          m_sourceFileText.setText( resultPath.toString() );
          m_relativeSourcePath = resultPath;
        }
        updateWorldFile();
      }
    }
    validate();

  }

  private void updateWorldFile( )
  {
    IPath path = null;
    if( m_relativeSourcePath.getFileExtension().toUpperCase().equals( "TIF" ) )
    {
      m_fileType = "tif";
      m_wfType = "tfw";
      path = m_relativeSourcePath.removeFileExtension().addFileExtension( m_wfType ).removeFirstSegments( 1 );
    }
    if( m_relativeSourcePath.getFileExtension().toUpperCase().equals( "JPG" ) )
    {
      m_fileType = "jpg";
      m_wfType = "jgw";
      path = m_relativeSourcePath.removeFileExtension().addFileExtension( m_wfType ).removeFirstSegments( 1 );
    }
    if( m_relativeSourcePath.getFileExtension().toUpperCase().equals( "PNG" ) )
    {
      m_fileType = "png";
      m_wfType = "pgw";
      path = m_relativeSourcePath.removeFileExtension().addFileExtension( m_wfType ).removeFirstSegments( 1 );
    }
    // if( m_relativeSourcePath.getFileExtension().toUpperCase().equals( "GIF" ) )
    // {
    // m_fileType = "gif";
    // m_wfType = "gfw";
    // path = m_relativeSourcePath.removeFileExtension().addFileExtension( m_wfType )
    // .removeFirstSegments( 1 );
    // }
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

      // else
      // {
      // m_worldFileGroup.setVisible( true );
      // m_textDx.setEditable( true );
      // m_textDy.setEditable( true );
      // m_textPhix.setEditable( true );
      // m_textPhiy.setEditable( true );
      // m_textULCx.setEditable( true );
      // m_textULCy.setEditable( true );
      // m_group.pack();
      // m_wfExists = false;
      // setPageComplete( false );
      // }
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

  protected boolean validate( )
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
    {
      try
      {
        IFile worldfile = m_project.getFile( m_relativeSourcePath.removeFirstSegments( 1 ).removeFileExtension().addFileExtension( m_wfType ) );

        String str = "";
        Control[] array = m_worldFileGroup.getChildren();
        for( int i = 0; i < array.length; i++ )
        {
          Control combo = array[i];
          if( combo instanceof Text )
            str = str + (((Text) combo).getText().trim() + "\n");
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
    }

    if( m_crsPanel != null && m_crsPanel.getSelectedCRS() != null && CRSHelper.isKnownCRS( m_crsPanel.getSelectedCRS() ) )
    {
      // ok
    }
    else
    {
      setErrorMessage( "Bitte wählen sie ein gültiges Koordinaten-System aus." );
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
    return new KalypsoResourceSelectionDialog( getShell(), m_project, "Select resource", fileResourceExtensions, m_project, new ResourceSelectionValidator() );
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
    if( (e.widget == m_textDx || e.widget == m_textDy || e.widget == m_textPhix || e.widget == m_textPhiy || e.widget == m_textULCx || e.widget == m_textULCy) && e.character == SWT.CR )
    {
      setPageComplete( validate() );
    }

  }

  public URL getURL( ) throws MalformedURLException
  {
    return m_project.getLocation().append( m_relativeSourcePath.removeFirstSegments( 1 ) ).toFile().toURL();
  }

  public String getFileType( )
  {
    return m_fileType;
  }

  public boolean isWorldFileExisting( )
  {
    return m_wfExists;
  }

  public IPath getRelativeSourcePath( )
  {
    return m_relativeSourcePath;
  }

  @Override
  public void dispose( )
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

  public String getCSName( )
  {
    return m_crsPanel.getSelectedCRS();
  }
}