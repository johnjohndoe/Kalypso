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
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.MalformedURLException;
import java.net.URL;
import java.rmi.RemoteException;
import java.util.Vector;

import org.bce.eclipse.ui.dialogs.KalypsoResourceSelectionDialog;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
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
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Widget;
import org.kalypso.java.io.FileUtilities;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree.graphics.sld.Layer;
import org.kalypsodeegree.graphics.sld.Style;
import org.kalypsodeegree.graphics.sld.StyledLayerDescriptor;
import org.kalypsodeegree.xml.XMLParsingException;
import org.kalypsodeegree_impl.graphics.sld.SLDFactory;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author kuepfer
 *  
 */
public class ImportShapeFileImportPage extends WizardPage implements
    SelectionListener, ModifyListener, KeyListener
{

  //constants
  private static final int SIZING_TEXT_FIELD_WIDTH = 250;

  //widgets
  private Group m_group;

  private Label m_sourceFileLabel;

  private Text m_sourceFileText;

  private Composite m_topComposite;

  private Button m_browseButton;

  //mapping

  private IPath m_relativeSourcePath;

  private Combo m_checkCRS;

  private IProject m_project;

  //style
  private Text styleTextField;

  private Button browseButton2;

  protected Path stylePath;

  protected Combo styleNameCombo;

  protected String styleName;

  private boolean checkDefaultStyle = false;

  private Button checkDefaultStyleButton;

  private Label styleNameLabel;

  private Label styleLabel;

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
  public ImportShapeFileImportPage( String pageName, String title,
      ImageDescriptor titleImage )
  {
    super( pageName, title, titleImage );
    setDescription( "Dieser Dialog liest eine ESRI Shape-Datei in den Workspace ein." );
    setPageComplete( false );
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

    Label crsLabel = new Label( m_group, SWT.NONE );
    crsLabel.setText( "Koordinaten System: " );

    m_checkCRS = new Combo( m_group, SWT.NONE );

    availableCoordinateSystems( m_checkCRS );
    try
    {
      String defaultCS = KalypsoGisPlugin.getDefault().getCoordinatesSystem()
          .getName();
      m_checkCRS.select( m_checkCRS.indexOf( defaultCS ) );
    }
    catch( RemoteException e1 )
    {
      e1.printStackTrace();
    }

    m_checkCRS.setToolTipText( "Koordinatensystem der ESRI(tm) Shape Datei" );
    GridData data = new GridData( GridData.FILL_HORIZONTAL );
    data.widthHint = SIZING_TEXT_FIELD_WIDTH;
    m_checkCRS.setLayoutData( data );
    m_checkCRS.addSelectionListener( this );
    m_checkCRS.addKeyListener( this );

    m_group.pack();

    //  style
    Group styleGroup = new Group( parent, SWT.NULL );
    styleGroup.setText( "Style" );

    GridData data3 = new GridData();
    data3.horizontalAlignment = GridData.FILL;
    data3.grabExcessHorizontalSpace = true;
    styleGroup.setLayoutData( data3 );
    GridLayout gridLayout1 = new GridLayout();
    gridLayout1.numColumns = 3;
    styleGroup.setLayout( gridLayout1 );

    styleLabel = new Label( styleGroup, SWT.NONE );
    styleLabel.setText( "Datei : " );

    styleTextField = new Text( styleGroup, SWT.BORDER );
    GridData data4 = new GridData();
    data4.horizontalAlignment = GridData.FILL;
    data4.grabExcessHorizontalSpace = true;
    styleTextField.setLayoutData( data4 );
    styleTextField.setEditable( false );

    browseButton2 = new Button( styleGroup, SWT.PUSH );
    browseButton2.setText( "Durchsuchen..." );
    browseButton2.setLayoutData( new GridData( GridData.END ) );
    browseButton2.addSelectionListener( this );

    styleNameLabel = new Label( styleGroup, SWT.NONE );
    styleNameLabel.setText( "UserStyle name: " );

    styleNameCombo = new Combo( styleGroup, SWT.READ_ONLY );
    GridData data5 = new GridData();
    data5.horizontalAlignment = GridData.FILL;
    data5.grabExcessHorizontalSpace = true;
    styleNameCombo.setLayoutData( data5 );
    styleNameCombo.addSelectionListener( this );
    
    //        new SelectionAdapter()
    //    {
    //      public void widgetSelected( SelectionEvent e )
    //      {
    //        styleName = styleNameCombo.getText();
    //        validate();
    //      }
    //    } );

    Label dummyLabel = new Label( styleGroup, SWT.NONE );
    dummyLabel.setText( "" );

    checkDefaultStyleButton = new Button( styleGroup, SWT.CHECK );
    checkDefaultStyleButton.setSelection( checkDefaultStyle );
    checkDefaultStyleButton.addSelectionListener( this );

    Label defaultStyleLabel = new Label( styleGroup, SWT.NONE );
    defaultStyleLabel.setText( "Generate default style" );

  }

  private void availableCoordinateSystems( Combo checkCRS )
  {
    ConvenienceCSFactoryFull factory = new ConvenienceCSFactoryFull();
    checkCRS.setItems( factory.getKnownCS() );
  }

  void validate()
  {
    setErrorMessage( null );
    boolean pageComplete = true;
    if( !checkDefaultStyle )
    {
      //styleName
      if( styleName != null )
      {
        //ok
      }
      else
      {
        setErrorMessage( "Bitte einen Style-Namen auswählen!" );
        pageComplete = false;
      }

      //styleFile
      if( styleTextField.getText() != null
          && styleTextField.getText().length() > 0 )
      {
        //ok
      }
      else
      {
        setErrorMessage( "Bitte eine Style-Datei auswählen!" );
        pageComplete = false;
      }
    }

    //CoordinateSystem
    if( checkCRS( m_checkCRS.getText() ) )
    {
      //ok
    }
    else
    {
      setErrorMessage( "Gewähltes KoordinatenSystem wird nicht unterstützt!" );
      pageComplete = false;
    }

    // shapeFile
    if( m_sourceFileText.getText() != null
        && m_sourceFileText.getText().length() > 0 )
    {
      //ok
    }
    else
    {
      setErrorMessage( "Bitte eine Shape-Datei auswählen!" );
      pageComplete = false;
    }

    setPageComplete( pageComplete );
  }

  //SelectionListener
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
      if( b.equals( m_browseButton ) )
      {
        KalypsoResourceSelectionDialog dialog = createResourceDialog( new String[]
        { "shp" } );
        dialog.open();
        Object[] result = dialog.getResult();
        if( result != null )
        {
          Path resultPath = (Path)result[0];
          m_sourceFileText.setText( resultPath.toString() );
          m_relativeSourcePath = resultPath;
        }
      }
      if( b.equals( browseButton2 ) )
      {
        KalypsoResourceSelectionDialog dialog = createResourceDialog( new String[]
        { "sld" } );
        dialog.open();
        Object[] result = dialog.getResult();
        if( result != null )
        {
          Path resultPath = (Path)result[0];
          styleTextField.setText( resultPath.toString() );
          stylePath = resultPath;
          try
          {
            IPath basePath = m_project.getLocation();
            String styleUrl = basePath.toFile().toURL()
                + stylePath.removeFirstSegments( 1 ).toString();
            Reader reader = new InputStreamReader( ( new URL( styleUrl ) )
                .openStream() );
            StyledLayerDescriptor styledLayerDescriptor = SLDFactory
                .createSLD( reader );
            reader.close();
            Layer[] layers = styledLayerDescriptor.getLayers();
            Vector styleNameVector = new Vector();
            for( int i = 0; i < layers.length; i++ )
            {
              Layer layer = layers[i];
              Style[] styles = layer.getStyles();
              for( int j = 0; j < styles.length; j++ )
              {
                styleNameVector.add( styles[j].getName() );
              }
            }
            String[] styleNames = new String[styleNameVector.size()];
            for( int k = 0; k < styleNameVector.size(); k++ )
            {
              styleNames[k] = (String)styleNameVector.get( k );
            }
            styleNameCombo.setItems( styleNames );
            styleNameCombo.select( 0 );
            styleName = styleNames[0];
          }
          catch( MalformedURLException e1 )
          {
            e1.printStackTrace();
          }
          catch( IOException ioEx )
          {
            ioEx.printStackTrace();
          }
          catch( XMLParsingException xmlEx )
          {
            xmlEx.printStackTrace();
          }
        }
      }
      if( b.equals( checkDefaultStyleButton ) )
      {
        checkDefaultStyle = checkDefaultStyleButton.getSelection();
        if( checkDefaultStyleButton.getSelection() )
        {
          styleLabel.setEnabled( false );
          styleTextField.setEnabled( false );
          browseButton2.setEnabled( false );
          styleNameLabel.setEnabled( false );
          styleNameCombo.setEnabled( false );
        }
        else
        {
          styleLabel.setEnabled( true );
          styleTextField.setEnabled( true );
          browseButton2.setEnabled( true );
          styleNameLabel.setEnabled( true );
          styleNameCombo.setEnabled( true );
        }
      }
    }
    if( e.widget instanceof Combo )
    {
      if( e.widget == m_checkCRS )
      {
        c = (Combo)e.widget;
        if( c == m_checkCRS )
        {
          setPageComplete( true );
        }
      }
      if( e.widget == styleNameCombo )
      {
        styleName = styleNameCombo.getText();
      }
    }

    validate();
  }

  /**
   * @see org.eclipse.swt.events.SelectionListener#widgetDefaultSelected(org.eclipse.swt.events.SelectionEvent)
   */
  public void widgetDefaultSelected( SelectionEvent e )
  {
    //no default selection
  }

  //ModifyListener
  /**
   * @see org.eclipse.swt.events.ModifyListener#modifyText(org.eclipse.swt.events.ModifyEvent)
   */
  public void modifyText( ModifyEvent e )
  {
    validate();
  }

  //KeyListener
  /**
   * @see org.eclipse.swt.events.KeyListener#keyPressed(org.eclipse.swt.events.KeyEvent)
   */
  public void keyPressed( KeyEvent e )
  {
    Widget w = e.widget;
    if( w instanceof Combo && e.character == SWT.CR )
    {
      validate();
    }
  }

  /**
   * @see org.eclipse.swt.events.KeyListener#keyReleased(org.eclipse.swt.events.KeyEvent)
   */
  public void keyReleased( KeyEvent e )
  {
    //do nothing
  }

  public File getShapeBaseFile()
  {
    return new File( m_project.getLocation()
        + "/"
        + FileUtilities.nameWithoutExtension( m_relativeSourcePath
            .removeFirstSegments( 1 ).toString() ) );
  }

  public String getShapeBaseRelativePath()
  {
    return "project:/"
        + FileUtilities.nameWithoutExtension( m_relativeSourcePath
            .removeFirstSegments( 1 ).toString() );
  }

  public IPath getShapePath()
  {
    return m_relativeSourcePath;
  }

  KalypsoResourceSelectionDialog createResourceDialog(
      String[] fileResourceExtensions )
  {
    return new KalypsoResourceSelectionDialog( getShell(), m_project,
        "Select resource", fileResourceExtensions, m_project );
  }

  public CS_CoordinateSystem getCRS()
  {
    return ConvenienceCSFactory.getInstance().getOGCCSByName(
        m_checkCRS.getText() );
  }

  protected void setProjectSelection( IProject project )
  {
    m_project = project;
  }

  private boolean checkCRS( String customCRS )
  {
    boolean result = false;
    CS_CoordinateSystem cs = ConvenienceCSFactory.getInstance().getOGCCSByName(
        customCRS );
    if( cs != null )
    {
      result = true;
    }
    return result;
  }

  public boolean checkDefaultStyle()
  {
    return checkDefaultStyle;
  }

  public IPath getStylePath()
  {
    return stylePath;
  }

  public String getStyleName()
  {
    return styleName;
  }

  public void removeListeners()
  {
    m_browseButton.removeSelectionListener( this );
    m_checkCRS.removeSelectionListener( this );
  }
}
