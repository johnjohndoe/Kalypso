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

/*
 * Created on 31.01.2005
 *  
 */
package org.kalypso.wizard;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author kuepfer
 *  
 */
public class KalypsoNAFileImportPage extends WizardPage
{

  //constants
//  private static final String NULL_KEY = "-NULL-";

  private static final int SIZING_TEXT_FIELD_WIDTH = 250;

//  private static final String SOURCE_KEY = "source";

//  private static final String TARGET_KEY = "target";

  //widgets
  private Group fileGroup;

  private Label fileLabel;

//  private Group sourceGroup;

  protected Group buttonGroup;

//  private Group targetGroup;

  private Text textField;

  private Composite topComposite;

  protected ScrolledComposite topSCLMappingComposite;

//  private Composite topMappingComposite;

  private Button skipRadioButton;

  private Button browseButton;

//  private Button okButton;

  //Geodata
  private CS_CoordinateSystem customCS = null;

  private CS_CoordinateSystem defaultCS = ConvenienceCSFactory.getInstance().getOGCCSByName(
      "EPSG:31467" ); //$NON-NLS-1$

  private GMLWorkspace sourceWorkspace;

  private URL fileURL;

  //mapping
//  private HashMap mapping;

  /**
   * @param pageName
   */
  public KalypsoNAFileImportPage( String pageName )
  {
    super( pageName );
    setDescription( WizardMessages.getString("KalypsoNAFileImportPage.PageDescription") ); //$NON-NLS-1$
    setPageComplete( false );
  }

  /**
   * @param pageName
   * @param title
   * @param titleImage
   */
  public KalypsoNAFileImportPage( String pageName, String title, ImageDescriptor titleImage )
  {
    super( pageName, title, titleImage );
    setDescription( WizardMessages.getString("KalypsoNAFileImportPage.PageDescription2") ); //$NON-NLS-1$
    setPageComplete( false );
  }

  private boolean checkSuffix( Text path )
  {
    boolean test = false;
    int dotLoc = path.getText().lastIndexOf( '.' );
    if( dotLoc != -1 )
    {
      String ext = path.getText().substring( dotLoc + 1 );
      if( ext.equalsIgnoreCase( "shp" ) == false ) //$NON-NLS-1$
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
    fileGroup.setText( WizardMessages.getString("KalypsoNAFileImportPage.FileGroupText") ); //$NON-NLS-1$

    fileLabel = new Label( fileGroup, SWT.NONE );
    fileLabel.setText( WizardMessages.getString("KalypsoNAFileImportPage.FileLabelText") ); //$NON-NLS-1$

    // Set width of Text fields
    GridData dataCatchment = new GridData( GridData.FILL_HORIZONTAL );
    dataCatchment.widthHint = SIZING_TEXT_FIELD_WIDTH;

    textField = new Text( fileGroup, SWT.BORDER );
    textField.setLayoutData( dataCatchment );
    textField.addModifyListener( new ModifyListener()
    {
      public void modifyText( ModifyEvent e )
      {
        validateFileField();
      }
    } );

    browseButton = new Button( fileGroup, SWT.PUSH );
    browseButton.setText( WizardMessages.getString("KalypsoNAFileImportPage.BrowseButtonText") ); //$NON-NLS-1$
    browseButton.setLayoutData( new GridData( GridData.END ) );
    browseButton.addSelectionListener( new SelectionAdapter()
    {
      public void widgetSelected( SelectionEvent e )
      {
        //if (validateFileField())
        handleFileBrowse();
        validateFileField();
      }
    } );
    skipRadioButton = new Button( fileGroup, SWT.CHECK );
    skipRadioButton.setText( WizardMessages.getString("KalypsoNAFileImportPage.SkipRadioButtonText") ); //$NON-NLS-1$
    skipRadioButton.setSelection( true );
    skipRadioButton.addSelectionListener( new SelectionAdapter()
    {
      public void widgetSelected( SelectionEvent e )
      {
        Button w = (Button)e.widget;
        if( !w.getSelection() )
        {
          setPageComplete( true );
          topSCLMappingComposite.setVisible( false );
          buttonGroup.setVisible( false );
          //remove mapping
//          mapping = null;
        }
        else
        {
          setPageComplete( false );
          topSCLMappingComposite.setVisible( true );
          buttonGroup.setVisible( true );
        }
      }

    } );
    fileGroup.pack();
  }

//  private FeatureType getSourceFT()
//  {
//    Feature rootFeature = sourceWorkspace.getRootFeature();
//
//    FeatureType rootFT = rootFeature.getFeatureType();
//    FeatureAssociationTypeProperty ftp = (FeatureAssociationTypeProperty)rootFT
//        .getProperty( "featureMember" );
//
//    FeatureType[] associationFeatureTypes = ftp.getAssociationFeatureTypes();
//    FeatureType shapeFT = associationFeatureTypes[0];
//    return shapeFT;
//  }

//  private void handelResetSelection()
//  {
//    Control[] cArray = sourceGroup.getChildren();
//    for( int i = 0; i < cArray.length; i++ )
//    {
//      Combo combo = (Combo)cArray[i];
//      combo.setData( SOURCE_KEY, NULL_KEY );
//      combo.setRedraw( true );
//    }
//    sourceGroup.redraw();
//    setPageComplete( false );
//  }//handelRestSelection

  /**
   * Uses the standard container selection dialog to choose the new value for
   * the container field.
   */

  protected void handleFileBrowse()
  {
    FileDialog fdialog = new FileDialog( getShell(), SWT.OPEN | SWT.SINGLE );
    fdialog.setFilterExtensions( new String[]
    { "shp" } ); //$NON-NLS-1$
    fdialog.setText( WizardMessages.getString("KalypsoNAFileImportPage.FileDialogText") ); //$NON-NLS-1$
    fdialog.setFilterNames( new String[]
    {
        "Shape Files", //$NON-NLS-1$
        "All Files (*.*)" } ); //$NON-NLS-1$
    fdialog.setFilterExtensions( new String[]
    {
        "*.shp", //$NON-NLS-1$
        "*.*" } ); //$NON-NLS-1$
    fdialog.setFileName( "*.shp" ); //$NON-NLS-1$
    if( fdialog.open() != null )
    {
      String textStr;
      try
      {

        fileURL = ( new File( fdialog.getFilterPath() + File.separator + fdialog.getFileName() ) )
            .toURL();
        textStr = fileURL.toExternalForm();
        textField.setText( textStr );
        //copyShapeFile(fileURL);

      }
      catch( MalformedURLException e )
      {
        e.printStackTrace();
      }

    }//if fdialog
  }//handleFileBrowse

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

  protected boolean validateFileField()
  {
    //	 checks catchment field entry and file suffix
    if( textField.getText().length() == 0 )
    {
      setErrorMessage( WizardMessages.getString("KalypsoNAFileImportPage.ErrorMessageNoFile") ); //$NON-NLS-1$
      setPageComplete( false );
      return false;
    }
    else if( checkSuffix( textField ) == false )
    {
      setErrorMessage( WizardMessages.getString("KalypsoNAFileImportPage.ErrorMessageWrongSuffix") ); //$NON-NLS-1$
      setPageComplete( false );
      return false;
    }
    else if( validateFile( fileURL ) == false )
    {
      setErrorMessage( WizardMessages.getString("KalypsoNAFileImportPage.ErrorMessageFileNotValid") ); //$NON-NLS-1$
      setPageComplete( false );
      return false;
    }
    setPageComplete( true );
    setErrorMessage( null );
    setMessage( null );
    return true;
  }

  public GMLWorkspace getSourceWorkspace()
  {
    return sourceWorkspace;
  }

  public URL getFileURL()
  {
    return fileURL;
  }

  public CS_CoordinateSystem getCoordinateSystem()
  {
    if( customCS != null )
      return customCS;

    return defaultCS;
  }
}