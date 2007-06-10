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
import java.rmi.RemoteException;
import java.util.HashMap;
import java.util.List;
import java.util.StringTokenizer;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Widget;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.annotation.AnnotationUtilities;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.gml.schema.SpecialPropertyMapper;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author kuepfer
 */
public class KalypsoNAProjectWizardPage extends WizardPage implements SelectionListener, KeyListener
{

  // constants
  private static final String NULL_KEY = "-NULL-"; //$NON-NLS-1$

  private static final int SIZING_TEXT_FIELD_WIDTH = 250;

  private static final String SOURCE_KEY = "source"; //$NON-NLS-1$

  private static final String TARGET_KEY = "target"; //$NON-NLS-1$

  private static final int MIN_COLUMN_SIZE = 150;

  // widgets
  private Group fileGroup;

  private Label fileLabel;

  private Group sourceGroup;

  private IFeatureType targetFT;

  private Group buttonGroup;

  private Combo m_checkCRS;

  private Group targetGroup;

  private Text m_fileField;

  private Composite m_topComposite;

  private ScrolledComposite topSCLMappingComposite;

  private Composite topMappingComposite;

  private Button skipRadioButton;

  private Button browseButton;

  private Button okButton;

  // Geodata
  private CS_CoordinateSystem customCS = null;

  // aus Preferences
  private String defaultCRS = null;

  private GMLWorkspace sourceWorkspace;

  private URL fileURL;

  // mapping
  private HashMap<Object, Object> mapping;

  private int maxSourceComboWidth;

  private Button resetButton;

  private static final int TOOL_TIP = 1;

  private static final int LABEL = 0;

  private static final int DESCRIPTION = 2;

  /**
   * @param pageName
   */
  public KalypsoNAProjectWizardPage( String pageName, IFeatureType featureType )
  {
    super( pageName );
    setDescription( WizardMessages.getString( "KalypsoNAProjectWizardPage.PageDescriptionPart1" ) //$NON-NLS-1$
        + WizardMessages.getString( "KalypsoNAProjectWizardPage.PageDescriptionPart2" ) ); //$NON-NLS-1$
    targetFT = featureType;
    setPageComplete( false );
  }

  /**
   * @param pageName
   * @param title
   * @param titleImage
   */
  public KalypsoNAProjectWizardPage( String pageName, String title, ImageDescriptor titleImage, IFeatureType featureType )
  {
    super( pageName, title, titleImage );
    setDescription( WizardMessages.getString( "KalypsoNAProjectWizardPage.PageDescription2Part1" ) //$NON-NLS-1$
        + WizardMessages.getString( "KalypsoNAProjectWizardPage.PageDescription2Part2" ) ); //$NON-NLS-1$
    targetFT = featureType;
    setPageComplete( false );

  }

  private void availableCoordinateSystems( Combo checkCRS )
  {
    ConvenienceCSFactoryFull factory = new ConvenienceCSFactoryFull();
    checkCRS.setItems( factory.getKnownCS() );
  }

  void validate( )
  {
    setErrorMessage( null );
    boolean pageComplete = true;

    // CoordinateSystem
    if( checkCRS( m_checkCRS.getText() ) )
    {
      // ok
    }
    else
    {
      setErrorMessage( WizardMessages.getString( "KalypsoNAProjectWizardPage.ErrorMessageNotSupportedCS" ) ); //$NON-NLS-1$
      pageComplete = false;
    }

    setPageComplete( pageComplete );
  }

  private boolean checkCRS( String customCRS )
  {
    boolean result = false;
    CS_CoordinateSystem cs = ConvenienceCSFactory.getInstance().getOGCCSByName( customCRS );
    if( cs != null )
    {
      result = true;
    }
    return result;
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

    m_topComposite = new Composite( parent, SWT.NULL );
    m_topComposite.setFont( parent.getFont() );

    initializeDialogUnits( parent );

    m_topComposite.setLayout( new GridLayout() );
    m_topComposite.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    // build wizard page
    createFileGroup( m_topComposite );
    createMappingGroup( m_topComposite );
    setControl( m_topComposite );
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
    fileGroup.setText( WizardMessages.getString( "KalypsoNAProjectWizardPage.FileGroupText" ) ); //$NON-NLS-1$

    fileLabel = new Label( fileGroup, SWT.NONE );
    fileLabel.setText( WizardMessages.getString( "KalypsoNAProjectWizardPage.FileLabelText" ) ); //$NON-NLS-1$

    // Set width of Text fields
    GridData dataCatchment = new GridData( GridData.FILL_HORIZONTAL );
    dataCatchment.widthHint = SIZING_TEXT_FIELD_WIDTH;

    m_fileField = new Text( fileGroup, SWT.BORDER );
    m_fileField.setLayoutData( dataCatchment );
    m_fileField.addModifyListener( new ModifyListener()
    {
      public void modifyText( ModifyEvent e )
      {
        validateFileField();
      }
    } );

    browseButton = new Button( fileGroup, SWT.PUSH );
    browseButton.setText( WizardMessages.getString( "KalypsoNAProjectWizardPage.BrowseButtonText" ) ); //$NON-NLS-1$
    browseButton.setLayoutData( new GridData( GridData.END ) );
    browseButton.addSelectionListener( this );
    Label crsLabel = new Label( fileGroup, SWT.NONE );
    crsLabel.setText( WizardMessages.getString( "KalypsoNAProjectWizardPage.CRSLabelText" ) ); //$NON-NLS-1$

    m_checkCRS = new Combo( fileGroup, SWT.NONE );

    availableCoordinateSystems( m_checkCRS );
    try
    {
      // String defaultCRS = KalypsoGisPlugin.getDefault().getCoordinatesSystem().getName();
      defaultCRS = KalypsoGisPlugin.getDefault().getCoordinatesSystem().getName();
      m_checkCRS.select( m_checkCRS.indexOf( defaultCRS ) );
    }
    catch( RemoteException e1 )
    {
      e1.printStackTrace();
    }

    m_checkCRS.setToolTipText( WizardMessages.getString( "KalypsoNAProjectWizardPage.CRSTooltip" ) ); //$NON-NLS-1$
    GridData data = new GridData( GridData.FILL_HORIZONTAL );
    data.widthHint = SIZING_TEXT_FIELD_WIDTH;
    m_checkCRS.setLayoutData( data );
    m_checkCRS.addSelectionListener( this );
    m_checkCRS.addKeyListener( this );

    skipRadioButton = new Button( fileGroup, SWT.CHECK );
    skipRadioButton.setText( WizardMessages.getString( "KalypsoNAProjectWizardPage.SkipRadioButtonText" ) ); //$NON-NLS-1$
    skipRadioButton.setSelection( true );
    skipRadioButton.addSelectionListener( this );

    fileGroup.pack();

  }

  private void createMappingGroup( Composite parent )
  {
    topSCLMappingComposite = new ScrolledComposite( parent, SWT.V_SCROLL | SWT.BORDER );
    topMappingComposite = new Composite( topSCLMappingComposite, SWT.NONE );
    GridLayout topSCLMappingCompoLayout = new GridLayout();
    topSCLMappingComposite.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    topMappingComposite.setVisible( true );
    GridLayout topMappingCompoLayout = new GridLayout();
    topMappingCompoLayout.numColumns = 2;
    topMappingComposite.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    topMappingComposite.setLayout( topMappingCompoLayout );
    topSCLMappingComposite.setLayout( topSCLMappingCompoLayout );
    topSCLMappingComposite.setContent( topMappingComposite );
    topMappingComposite.pack();

    // Top Group
    Group topGroup = new Group( topMappingComposite, SWT.NONE );
    topGroup.setText( WizardMessages.getString( "KalypsoNAProjectWizardPage.MappingGroupText" ) ); //$NON-NLS-1$
    topGroup.setVisible( true );
    GridLayout topGroupLayout = new GridLayout();
    topGroupLayout.numColumns = 2;
    topGroupLayout.makeColumnsEqualWidth = true;
    topGroup.setLayout( topGroupLayout );

    GridData topGroupGridData = new GridData( GridData.FILL_HORIZONTAL );
    topGroupGridData.horizontalSpan = 2;
    topGroup.setLayoutData( topGroupGridData );
    topGroup.pack();

    // Source
    sourceGroup = new Group( topGroup, SWT.NONE );
    GridLayout sourceGroupLayout = new GridLayout();
    sourceGroupLayout.verticalSpacing = 2;
    sourceGroup.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    sourceGroup.setLayout( sourceGroupLayout );
    sourceGroup.setVisible( true );
    sourceGroup.setText( WizardMessages.getString( "KalypsoNAProjectWizardPage.SourceGroupText" ) ); //$NON-NLS-1$
    IPropertyType[] targetFtp = targetFT.getProperties();
    for( int j = 0; j < targetFtp.length; j++ )
    {
      Combo combo = new Combo( sourceGroup, SWT.READ_ONLY | SWT.DROP_DOWN | SWT.SINGLE );
      combo.setData( TARGET_KEY, targetFtp[j].getQName().getLocalPart() );
      combo.addSelectionListener( new SelectionAdapter()
      {
        @Override
        public void widgetSelected( SelectionEvent e )
        {
          Widget w = e.widget;
          storeSelectionData( w );
        }
      } );
      combo.add( NULL_KEY );
      combo.select( 0 );
    }
    Point sizeSourceG = sourceGroup.computeSize( SWT.DEFAULT, SWT.DEFAULT );
    sourceGroup.setSize( sizeSourceG );
    GridData sourceGroupGridData = new GridData();
    sourceGroupGridData.widthHint = Math.max( sizeSourceG.x, MIN_COLUMN_SIZE );
    // sourceGroupGridData.heightHint = Math.max(sizeSourceG.y, SWT.DEFAULT);
    sourceGroup.setLayoutData( sourceGroupGridData );
    sourceGroup.pack();
    sourceGroup.layout();

    // Target
    targetGroup = new Group( topGroup, SWT.NONE );
    GridLayout targetGroupLayout = new GridLayout();
    targetGroupLayout.verticalSpacing = 10;
    targetGroupLayout.marginHeight = 3;
    targetGroup.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    targetGroup.setVisible( true );
    targetGroup.setLayout( targetGroupLayout );
    targetGroup.setText( WizardMessages.getString( "KalypsoNAProjectWizardPage.TargetGroupText" ) ); //$NON-NLS-1$
    for( int i = 0; i < targetFtp.length; i++ )
    {
      IPropertyType featureTypeProperty = targetFtp[i];
      Text text = new Text( targetGroup, SWT.NONE | SWT.READ_ONLY );
      text.setText( getAnnotation( featureTypeProperty, LABEL ) );
      text.setToolTipText( getAnnotation( featureTypeProperty, TOOL_TIP ) );
    }
    targetGroup.pack();
    // OK and Reset buttons group
    buttonGroup = new Group( parent, SWT.NONE );
    buttonGroup.setText( WizardMessages.getString( "KalypsoNAProjectWizardPage.ButtonGroupText" ) ); //$NON-NLS-1$
    GridLayout buttonbarLayout = new GridLayout();
    buttonbarLayout.numColumns = 2;
    buttonGroup.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    buttonGroup.setLayout( buttonbarLayout );

    okButton = new Button( buttonGroup, SWT.PUSH );
    okButton.setText( WizardMessages.getString( "KalypsoNAProjectWizardPage.OKButtonText" ) ); //$NON-NLS-1$
    okButton.addSelectionListener( this );
    resetButton = new Button( buttonGroup, SWT.PUSH );
    resetButton.setText( WizardMessages.getString( "KalypsoNAProjectWizardPage.ResetButtonText" ) ); //$NON-NLS-1$
    resetButton.addSelectionListener( this );

    Point size = topMappingComposite.computeSize( SWT.DEFAULT, SWT.DEFAULT );
    topMappingComposite.setSize( size );
    GridData topCompoGridData = new GridData();
    topCompoGridData.widthHint = Math.min( size.x, 600 );
    topCompoGridData.heightHint = Math.min( size.y, 200 );
    topSCLMappingComposite.setLayoutData( topCompoGridData );
    topMappingComposite.pack();
    topMappingComposite.layout();

  }

  private IFeatureType getSourceFT( )
  {
    final Feature rootFeature = sourceWorkspace.getRootFeature();

    final IFeatureType rootFT = rootFeature.getFeatureType();
    final IRelationType ftp = (IRelationType) rootFT.getProperty( ShapeSerializer.PROPERTY_FEATURE_MEMBER );

    final IFeatureType associationFeatureType = ftp.getTargetFeatureType();
    final IFeatureType[] associationFeatureTypes = GMLSchemaUtilities.getSubstituts( associationFeatureType, null, false, true );
    final IFeatureType shapeFT = associationFeatureTypes[0];
    return shapeFT;
  }

  private void handelResetSelection( )
  {
    Control[] cArray = sourceGroup.getChildren();
    for( int i = 0; i < cArray.length; i++ )
    {
      Combo combo = (Combo) cArray[i];
      combo.setData( SOURCE_KEY, NULL_KEY );
      combo.select( 0 );
    }
    sourceGroup.redraw();
    setPageComplete( false );
  }// handelRestSelection

  /**
   * Uses the standard container selection dialog to choose the new value for the container field.
   */

  private void handleFileBrowse( )
  {
    FileDialog fdialog = new FileDialog( getShell(), SWT.OPEN | SWT.SINGLE );
    fdialog.setFilterExtensions( new String[] { "shp" } ); //$NON-NLS-1$
    fdialog.setText( WizardMessages.getString( "KalypsoNAProjectWizardPage.BrowseText" ) ); //$NON-NLS-1$
    fdialog.setFilterNames( new String[] { "Shape Files", //$NON-NLS-1$
        "All Files (*.*)" } ); //$NON-NLS-1$
    fdialog.setFilterExtensions( new String[] { "*.shp", //$NON-NLS-1$
        "*.*" } ); //$NON-NLS-1$
    fdialog.setFileName( "*.shp" ); //$NON-NLS-1$
    if( fdialog.open() != null )
    {
      String textStr;
      try
      {

        fileURL = (new File( fdialog.getFilterPath() + File.separator + fdialog.getFileName() )).toURL();
        textStr = fileURL.toString();
        m_fileField.setText( textStr );
        readShapeFile( fileURL );

      }
      catch( MalformedURLException e )
      {
        e.printStackTrace();
      }

    }// if fdialog
    IFeatureType ft = getSourceFT();
    setMaxSourceCombo( ft );
    updateSourceList( ft );
  }// handleFileBrowse

  /**
   * @param ft
   */
  private void setMaxSourceCombo( IFeatureType ft )
  {
    int max = 0;
    IPropertyType[] ftp = ft.getProperties();
    for( int i = 0; i < ftp.length; i++ )
    {
      IPropertyType propertyType = ftp[i];
      int length = propertyType.getQName().getLocalPart().length();
      if( length > max )
        max = length;
    }
    maxSourceComboWidth = max;
  }

  private void handleOKSelection( )
  {
    Control[] cArray = sourceGroup.getChildren();
    mapping = new HashMap<Object, Object>();
    for( int i = 0; i < cArray.length; i++ )
    {
      Control c = cArray[i];
      Object source = c.getData( SOURCE_KEY );
      Object target = c.getData( TARGET_KEY );
      mapping.put( target, source );
    }
    setPageComplete( true );
  }// handleOKSelection

  /**
   * This method returns a HashMap with the user defined mapping of the source to the target. key = (String) target
   * property value = (String) source property
   * 
   * @return map HashMap with the custom mapping
   */

  public HashMap getMapping( )
  {
    HashMap<Object, Object> map = new HashMap<Object, Object>();
    Control[] cArray = sourceGroup.getChildren();
    for( int i = 0; i < cArray.length; i++ )
    {
      Combo c = (Combo) cArray[i];
      Object target = c.getData( TARGET_KEY );
      Object source = c.getData( SOURCE_KEY );
      if( source != null )
        map.put( target, source );
    }// for i
    return map;
  }// getMapping

  private void readShapeFile( URL url )
  {
    CS_CoordinateSystem cs;
    cs = null;
    if( customCS == null )
      cs = ConvenienceCSFactory.getInstance().getOGCCSByName( getDefaultCRS() );
    else
      cs = customCS;

    int index = url.getPath().lastIndexOf( "." ); //$NON-NLS-1$
    String fileBase = url.getPath().substring( 1, index );

    try
    {
      sourceWorkspace = ShapeSerializer.deserialize( fileBase, cs );
    }
    catch( GmlSerializeException e )
    {
      e.printStackTrace();
      if( m_topComposite != null && !m_topComposite.isDisposed() )
      {
        MessageBox message = new MessageBox( m_topComposite.getShell(), SWT.OK );
        message.setText( WizardMessages.getString( "KalypsoNAProjectWizardPage.TextMessageReadError" ) ); //$NON-NLS-1$
        message.setMessage( WizardMessages.getString( "KalypsoNAProjectWizardPage.MessageReadError" ) + url.getFile() ); //$NON-NLS-1$
        message.open();
      }
    }

  }// getData

  /**
   * This method clears the mapping
   */

  public void updateSourceList( IFeatureType srcFT )
  {
    IPropertyType[] ftp = srcFT.getProperties();
    IPropertyType[] targetFTP = targetFT.getProperties();
    Control[] cArray = sourceGroup.getChildren();
    for( int i = 0; i < cArray.length; i++ )
    {
      Combo combo = (Combo) cArray[i];
      combo.setData( SOURCE_KEY, null );
      combo.removeAll();
      combo.setSize( maxSourceComboWidth * 10, SWT.DEFAULT );
      for( int j = 0; j < ftp.length; j++ )
      {
        if( j == 0 )
          combo.add( NULL_KEY );
        // checks if the mapping between types is possible
        if( ftp[j] instanceof IValuePropertyType && targetFTP[i] instanceof IValuePropertyType )
        {
          final IValuePropertyType fromPT = (IValuePropertyType) ftp[j];
          final IValuePropertyType toPT = (IValuePropertyType) targetFTP[i];
          if( SpecialPropertyMapper.isValidMapping( fromPT.getValueClass(), toPT.getValueClass() ) )
            combo.add( ftp[j].getQName().getLocalPart() );
        }
      }// for j
      combo.select( 0 );
    }// for i

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

  protected void validateFileField( )
  {
    if( !skipRadioButton.getSelection() )
    {
      setPageComplete( true );
      setMessage( WizardMessages.getString( "KalypsoNAProjectWizardPage.SkipMessage" ) ); //$NON-NLS-1$
      return;
    }
    // checks catchment field entry and file suffix
    if( m_fileField.getText().length() == 0 )
    {
      setErrorMessage( WizardMessages.getString( "KalypsoNAProjectWizardPage.ErrorMessageChooseFile" ) ); //$NON-NLS-1$
      setPageComplete( false );
    }
    else if( checkSuffix( m_fileField ) == false )
    {
      setErrorMessage( WizardMessages.getString( "KalypsoNAProjectWizardPage.ErrorMessageWrongSuffix" ) ); //$NON-NLS-1$
      setPageComplete( false );
    }
    else if( validateFile( fileURL ) == false )
    {
      setErrorMessage( WizardMessages.getString( "KalypsoNAProjectWizardPage.ErrorMessageNotValidFile" ) ); //$NON-NLS-1$
      setPageComplete( false );
    }
    // setPageComplete(true);
    setErrorMessage( null );
    setMessage( null );
  }

  public List getFeatureList( )
  {
    Feature rootFeature = sourceWorkspace.getRootFeature();
    List featureList = (List) rootFeature.getProperty( ShapeSerializer.PROPERTY_FEATURE_MEMBER );
    return featureList;
  }

  @Override
  public void dispose( )
  {
    okButton.removeSelectionListener( this );
    resetButton.removeSelectionListener( this );
    browseButton.removeSelectionListener( this );
    skipRadioButton.removeSelectionListener( this );
  }

  protected void storeSelectionData( Widget w )
  {
    StringTokenizer st = new StringTokenizer( w.toString() );
    st.nextToken();
    String str = st.nextToken();
    String name = str.substring( 1, str.length() - 1 );
    w.setData( SOURCE_KEY, name );
    // System.out
    // .println( "Quelle: " + w.getData( SOURCE_KEY ) + "\tZiel: " + w.getData(
    // TARGET_KEY ) );
  }

  /**
   * This method gets a specified Annotation of a IFeatureType
   */
  public String getAnnotation( IPropertyType ftp, int type )
  {
    final IAnnotation annotation = AnnotationUtilities.getAnnotation( ftp );
    if( annotation != null )
    {
      if( type == TOOL_TIP )
        return annotation.getTooltip();
      else if( type == LABEL )
        return annotation.getLabel();
      else if( type == DESCRIPTION )
        return annotation.getDescription();
    }
    return ftp.getQName().getLocalPart();
  }

  /**
   * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
   */
  public void widgetSelected( SelectionEvent e )
  {
    Combo c;
    Widget w = e.widget;
    if( w instanceof Button )
    {
      Button b = (Button) w;
      if( b == okButton )
        handleOKSelection();
      if( b == resetButton )
        handelResetSelection();
      if( b == browseButton )
        handleFileBrowse();
      if( b == skipRadioButton )
      {
        if( !b.getSelection() )
        {
          // setPageComplete( true );
          topSCLMappingComposite.setVisible( false );
          buttonGroup.setVisible( false );
          // remove mapping
          mapping = null;
          // clear filefield
          m_fileField.setText( "" ); //$NON-NLS-1$
        }
        else
        {
          // setPageComplete( true );
          topSCLMappingComposite.setVisible( true );
          buttonGroup.setVisible( true );
        }
      }
    }
    if( e.widget instanceof Combo )
    {
      if( e.widget == m_checkCRS )
      {
        c = (Combo) e.widget;
        if( c == m_checkCRS )
        {
          customCS = ConvenienceCSFactory.getInstance().getOGCCSByName( c.getText() );
          readShapeFile( fileURL );
          setPageComplete( true );
        }
      }
    }

    validateFileField();
  }

  /**
   * @see org.eclipse.swt.events.SelectionListener#widgetDefaultSelected(org.eclipse.swt.events.SelectionEvent)
   */
  public void widgetDefaultSelected( SelectionEvent e )
  {
    // do nothing
  }

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
    // do nothing
  }

  public String getDefaultCRS( )
  {
    return defaultCRS;
  }
}