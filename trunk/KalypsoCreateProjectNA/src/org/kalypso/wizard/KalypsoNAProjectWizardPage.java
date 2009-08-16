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
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.transformation.CRSHelper;
import org.kalypso.wizard.i18n.Messages;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.gml.schema.SpecialPropertyMapper;

/**
 * @author kuepfer
 */
public class KalypsoNAProjectWizardPage extends WizardPage implements SelectionListener, KeyListener
{
  // constants
  private static final int SIZING_TEXT_FIELD_WIDTH = 250;

  private static final String SOURCE_KEY = "source"; //$NON-NLS-1$

  private static final String TARGET_KEY = "target"; //$NON-NLS-1$

  private static final int MIN_COLUMN_SIZE = 150;

  // widgets
  private Group fileGroup;

  private Label fileLabel;

  private Group sourceGroup;

  private final IFeatureType targetFT;

  private Combo m_checkCRS;

  private Group targetGroup;

  private Text m_fileField;

  private Composite m_topComposite;

  private Button skipRadioButton;

  private Button browseButton;

  private Button okButton;

  // Geodata
  private String customCS = null;

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

  private Group m_mappingGroup;

  /**
   * @param pageName
   */
  public KalypsoNAProjectWizardPage( final String pageName, final IFeatureType featureType )
  {
    super( pageName );
    setDescription( Messages.get( "KalypsoNAProjectWizardPage.PageDescription" ) ); //$NON-NLS-1$
    targetFT = featureType;
    setPageComplete( false );
  }

  /**
   * @param pageName
   * @param title
   * @param titleImage
   */
  public KalypsoNAProjectWizardPage( final String pageName, final String title, final ImageDescriptor titleImage, final IFeatureType featureType )
  {
    super( pageName, title, titleImage );
    setDescription( Messages.get( "KalypsoNAProjectWizardPage.PageDescription" ) ); //$NON-NLS-1$
    targetFT = featureType;
    setPageComplete( false );

  }

  private void availableCoordinateSystems( final Combo checkCRS )
  {
    checkCRS.setItems( CRSHelper.getAllNames().toArray( new String[] {} ) );
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
      setErrorMessage( Messages.get( "KalypsoNAProjectWizardPage.ErrorMessageNotSupportedCS" ) ); //$NON-NLS-1$
      pageComplete = false;
    }

    setPageComplete( pageComplete );
  }

  private boolean checkCRS( final String customCRS )
  {
    return CRSHelper.isKnownCRS( customCRS );
  }

  private boolean checkSuffix( final Text path )
  {
    boolean test = false;
    final int dotLoc = path.getText().lastIndexOf( '.' );
    if( dotLoc != -1 )
    {
      final String ext = path.getText().substring( dotLoc + 1 );
      if( ext.equalsIgnoreCase( "shp" ) == false ) //$NON-NLS-1$
        test = false;
      else
        test = true;
    }
    return test;
  }

  /*
   * (non-Javadoc)
   * @see wizard.eclipse.jface.dialogs.IDialogPage#createControl(wizard.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    m_topComposite = new Composite( parent, SWT.NULL );
    m_topComposite.setFont( parent.getFont() );

    initializeDialogUnits( parent );

    m_topComposite.setLayout( new GridLayout() );
    m_topComposite.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    // build wizard page

    skipRadioButton = new Button( m_topComposite, SWT.CHECK );
    skipRadioButton.setText( getTitle() );
    skipRadioButton.setSelection( true );
    skipRadioButton.addSelectionListener( this );
    
    createFileGroup( m_topComposite );
    createMappingGroup( m_topComposite );
    setControl( m_topComposite );
  }

  private void createFileGroup( final Composite parent )
  {

    fileGroup = new Group( parent, SWT.NULL );
    final GridLayout topGroupLayout = new GridLayout();
    final GridData topGroupData = new GridData();
    topGroupLayout.numColumns = 3;
    topGroupData.horizontalAlignment = GridData.FILL;
    fileGroup.setLayout( topGroupLayout );
    fileGroup.setLayoutData( topGroupData );
    fileGroup.setText( Messages.get( "KalypsoNAProjectWizardPage.FileGroupText" ) ); //$NON-NLS-1$

    fileLabel = new Label( fileGroup, SWT.NONE );
    fileLabel.setText( Messages.get( "KalypsoNAProjectWizardPage.FileLabelText" ) ); //$NON-NLS-1$

    // Set width of Text fields
    final GridData dataCatchment = new GridData( GridData.FILL_HORIZONTAL );
    dataCatchment.widthHint = SIZING_TEXT_FIELD_WIDTH;

    m_fileField = new Text( fileGroup, SWT.BORDER );
    m_fileField.setLayoutData( dataCatchment );
    m_fileField.addModifyListener( new ModifyListener()
    {
      public void modifyText( final ModifyEvent e )
      {
        validateFileField();
      }
    } );

    browseButton = new Button( fileGroup, SWT.PUSH );
    browseButton.setText( Messages.get( "KalypsoNAProjectWizardPage.BrowseButtonText" ) ); //$NON-NLS-1$
    browseButton.setLayoutData( new GridData( GridData.END ) );
    browseButton.addSelectionListener( this );
    final Label crsLabel = new Label( fileGroup, SWT.NONE );
    crsLabel.setText( Messages.get( "KalypsoNAProjectWizardPage.CRSLabelText" ) ); //$NON-NLS-1$

    m_checkCRS = new Combo( fileGroup, SWT.NONE );

    availableCoordinateSystems( m_checkCRS );

    // String defaultCRS = KalypsoGisPlugin.getDefault().getCoordinatesSystem().getName();
    defaultCRS = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
    m_checkCRS.select( m_checkCRS.indexOf( defaultCRS ) );

    m_checkCRS.setToolTipText( Messages.get( "KalypsoNAProjectWizardPage.CRSTooltip" ) ); //$NON-NLS-1$
    final GridData data = new GridData( GridData.FILL_HORIZONTAL );
    data.widthHint = SIZING_TEXT_FIELD_WIDTH;
    m_checkCRS.setLayoutData( data );
    m_checkCRS.addSelectionListener( this );
    m_checkCRS.addKeyListener( this );

    fileGroup.pack();
  }

  private void createMappingGroup( final Composite parent )
  {
    m_mappingGroup = new Group( parent, SWT.NONE );
    m_mappingGroup.setText( Messages.get( "KalypsoNAProjectWizardPage.MappingGroupText" ) ); //$NON-NLS-1$
    final GridLayout topGroupLayout = new GridLayout( 2, false );
    m_mappingGroup.setLayout( topGroupLayout );

    final GridData topGroupGridData = new GridData( SWT.FILL, SWT.FILL, true, true );
    m_mappingGroup.setLayoutData( topGroupGridData );

    final ScrolledComposite topSCLMappingComposite = new ScrolledComposite( m_mappingGroup, SWT.V_SCROLL | SWT.BORDER );
    topSCLMappingComposite.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true, 2, 1 ) );

    final GridData topSCLData = new GridData( SWT.FILL, SWT.FILL, true, true, 2, 1 );
    topSCLData.heightHint = 200;
    topSCLMappingComposite.setLayoutData( topSCLData );

    
    final Composite topMappingComposite = new Composite( topSCLMappingComposite, SWT.NONE );
    topMappingComposite.setLayout( new GridLayout( 2, true ) );

    topSCLMappingComposite.setContent( topMappingComposite );

    // Source
    sourceGroup = new Group( topMappingComposite, SWT.NONE );
    sourceGroup.setLayout( new GridLayout() );
    sourceGroup.setLayoutData( new GridData( SWT.FILL, SWT.BEGINNING, true, true ) );
    sourceGroup.setVisible( true );
    sourceGroup.setText( Messages.get( "KalypsoNAProjectWizardPage.SourceGroupText" ) ); //$NON-NLS-1$
    final IPropertyType[] targetFtp = targetFT.getProperties();
    for( int j = 0; j < targetFtp.length; j++ )
    {
      if( !targetFtp[j].getQName().getLocalPart().equals( "boundedBy" ) && targetFtp[j] instanceof IValuePropertyType ) //$NON-NLS-1$
      {
        final Combo combo = new Combo( sourceGroup, SWT.READ_ONLY | SWT.DROP_DOWN | SWT.SINGLE );
        combo.setData( TARGET_KEY, targetFtp[j].getQName().getLocalPart() );
        combo.addSelectionListener( new SelectionAdapter()
        {
          @Override
          public void widgetSelected( final SelectionEvent e )
          {
            final Widget w = e.widget;
            storeSelectionData( w );
          }
        } );
        combo.add( KalypsoNAProjectWizard.NULL_KEY );
        combo.select( 0 );
      }
    }

    // Target
    targetGroup = new Group( topMappingComposite, SWT.NONE );
    targetGroup.setLayout( new GridLayout() );
    targetGroup.setLayoutData( new GridData( SWT.FILL, SWT.BEGINNING, true, true ) );
    targetGroup.setText( Messages.get( "KalypsoNAProjectWizardPage.TargetGroupText" ) ); //$NON-NLS-1$
    for( int i = 0; i < targetFtp.length; i++ )
    {
      if( !targetFtp[i].getQName().getLocalPart().equals( "boundedBy" ) && targetFtp[i] instanceof IValuePropertyType ) //$NON-NLS-1$
      {
        final IPropertyType featureTypeProperty = targetFtp[i];
        final Text text = new Text( targetGroup, SWT.NONE | SWT.READ_ONLY );
        text.setText( getAnnotation( featureTypeProperty, LABEL ) );
        text.setToolTipText( getAnnotation( featureTypeProperty, TOOL_TIP ) );
      }
    }

    // OK and Reset buttons group
    okButton = new Button( m_mappingGroup, SWT.PUSH );
    okButton.setText( Messages.get( "KalypsoNAProjectWizardPage.OKButtonText" ) ); //$NON-NLS-1$
    okButton.addSelectionListener( this );
    resetButton = new Button( m_mappingGroup, SWT.PUSH );
    resetButton.setText( Messages.get( "KalypsoNAProjectWizardPage.ResetButtonText" ) ); //$NON-NLS-1$
    resetButton.addSelectionListener( this );

    topMappingComposite.pack();
    topMappingComposite.layout();
  }

  private IFeatureType getSourceFT( )
  {
    final Feature rootFeature = sourceWorkspace.getRootFeature();

    final IFeatureType rootFT = rootFeature.getFeatureType();
    final IRelationType ftp = (IRelationType) rootFT.getProperty( ShapeSerializer.PROPERTY_FEATURE_MEMBER );

    final IFeatureType associationFeatureType = ftp.getTargetFeatureType();
    // TODO: Why substitutes? Only valid shape types (exact match) should be possible
    // SpecialPropertyMapper does not exist for GM_Object -> GM_Surface
    // final IFeatureType[] associationFeatureTypes = GMLSchemaUtilities.getSubstituts( associationFeatureType, null,
    // false, true );
    // final IFeatureType shapeFT = associationFeatureTypes[0];
    // return shapeFT;
    return associationFeatureType;
  }

  private void handelResetSelection( )
  {
    final Control[] cArray = sourceGroup.getChildren();
    for( final Control element : cArray )
    {
      final Combo combo = (Combo) element;
      combo.setData( SOURCE_KEY, KalypsoNAProjectWizard.NULL_KEY );
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
    final FileDialog fdialog = new FileDialog( getShell(), SWT.OPEN | SWT.SINGLE );
    fdialog.setFilterExtensions( new String[] { "shp" } ); //$NON-NLS-1$
    fdialog.setText( Messages.get( "KalypsoNAProjectWizardPage.BrowseText" ) ); //$NON-NLS-1$
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

        fileURL = (new File( fdialog.getFilterPath() + File.separator + fdialog.getFileName() )).toURI().toURL();
        textStr = fileURL.toString();
        m_fileField.setText( textStr );
        readShapeFile( fileURL );

      }
      catch( final MalformedURLException e )
      {
        e.printStackTrace();
      }

    }// if fdialog
    final IFeatureType ft = getSourceFT();
    setMaxSourceCombo( ft );
    updateSourceList( ft );
  }// handleFileBrowse

  /**
   * @param ft
   */
  private void setMaxSourceCombo( final IFeatureType ft )
  {
    int max = 0;
    final IPropertyType[] ftp = ft.getProperties();
    for( final IPropertyType propertyType : ftp )
    {
      final int length = propertyType.getQName().getLocalPart().length();
      if( length > max )
        max = length;
    }
    maxSourceComboWidth = max;
  }

  private void handleOKSelection( )
  {
    final Control[] cArray = sourceGroup.getChildren();
    mapping = new HashMap<Object, Object>();
    for( final Control c : cArray )
    {
      final Object source = c.getData( SOURCE_KEY );
      final Object target = c.getData( TARGET_KEY );
      mapping.put( target, source );
    }
    setPageComplete( true );
  }

  /**
   * This method returns a HashMap with the user defined mapping of the source to the target. key = (String) target
   * property value = (String) source property
   * 
   * @return map HashMap with the custom mapping
   */

  public HashMap<Object, Object> getMapping( )
  {
    final HashMap<Object, Object> map = new HashMap<Object, Object>();
    final Control[] cArray = sourceGroup.getChildren();
    for( final Control element : cArray )
    {
      final Combo c = (Combo) element;
      final Object target = c.getData( TARGET_KEY );
      final Object source = c.getData( SOURCE_KEY );
      if( source != null )
        map.put( target, source );
    }// for i
    return map;
  }// getMapping

  private void readShapeFile( final URL url )
  {
    String cs = null;

    if( customCS == null )
      cs = getDefaultCRS();
    else
      cs = customCS;

    final int index = url.getPath().lastIndexOf( "." ); //$NON-NLS-1$
    final String fileBase = url.getPath().substring( 1, index );

    try
    {
      sourceWorkspace = ShapeSerializer.deserialize( fileBase, cs );
    }
    catch( final GmlSerializeException e )
    {
      e.printStackTrace();
      if( m_topComposite != null && !m_topComposite.isDisposed() )
      {
        final MessageBox message = new MessageBox( m_topComposite.getShell(), SWT.OK );
        message.setText( Messages.get( "KalypsoNAProjectWizardPage.TextMessageReadError" ) ); //$NON-NLS-1$
        message.setMessage( Messages.get( "KalypsoNAProjectWizardPage.MessageReadError" ) + url.getFile() ); //$NON-NLS-1$
        message.open();
      }
    }

  }// getData

  /**
   * This method clears the mapping
   */

  public void updateSourceList( final IFeatureType srcFT )
  {
    final IPropertyType[] ftp = srcFT.getProperties();
    final IPropertyType[] targetFTP = targetFT.getProperties();
    final Control[] cArray = sourceGroup.getChildren();
    int k = 0;
    for( int i = 0; i < targetFTP.length; i++ )
    {
      if( targetFTP[i] instanceof IValuePropertyType && !targetFTP[i].getQName().getLocalPart().equals( "boundedBy" ) ) //$NON-NLS-1$
      {
        final Combo combo = (Combo) cArray[k];
        combo.setData( SOURCE_KEY, null );
        combo.removeAll();
        combo.setSize( maxSourceComboWidth * 10, SWT.DEFAULT );
        for( int j = 0; j < ftp.length; j++ )
        {
          if( j == 0 )
            combo.add( KalypsoNAProjectWizard.NULL_KEY );
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
        k += 1;
      }// for i
    }
  }

  public boolean validateFile( final URL url )
  {
    try
    {
      final InputStream ios = url.openStream();
      ios.close();
    }
    catch( final IOException e )
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
      setMessage( Messages.get( "KalypsoNAProjectWizardPage.SkipMessage" ) ); //$NON-NLS-1$
      return;
    }
    // checks catchment field entry and file suffix
    if( m_fileField.getText().length() == 0 )
    {
      setErrorMessage( Messages.get( "KalypsoNAProjectWizardPage.ErrorMessageChooseFile" ) ); //$NON-NLS-1$
      setPageComplete( false );
    }
    else if( checkSuffix( m_fileField ) == false )
    {
      setErrorMessage( Messages.get( "KalypsoNAProjectWizardPage.ErrorMessageWrongSuffix" ) ); //$NON-NLS-1$
      setPageComplete( false );
    }
    else if( validateFile( fileURL ) == false )
    {
      setErrorMessage( Messages.get( "KalypsoNAProjectWizardPage.ErrorMessageNotValidFile" ) ); //$NON-NLS-1$
      setPageComplete( false );
    }
    // setPageComplete(true);
    setErrorMessage( null );
    setMessage( null );
  }

  public List< ? > getFeatureList( )
  {
    final Feature rootFeature = sourceWorkspace.getRootFeature();
    final List< ? > featureList = (List< ? >) rootFeature.getProperty( ShapeSerializer.PROPERTY_FEATURE_MEMBER );
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

  protected void storeSelectionData( final Widget w )
  {
    final StringTokenizer st = new StringTokenizer( w.toString() );
    st.nextToken();
    final String str = st.nextToken();
    final String name = str.substring( 1, str.length() - 1 );
    w.setData( SOURCE_KEY, name );
    // System.out
    // .println( "Quelle: " + w.getData( SOURCE_KEY ) + "\tZiel: " + w.getData(
    // TARGET_KEY ) );
  }

  /**
   * This method gets a specified Annotation of a IFeatureType
   */
  public String getAnnotation( final IPropertyType ftp, final int type )
  {
    final IAnnotation annotation = ftp.getAnnotation();
    if( type == TOOL_TIP )
      return annotation.getTooltip();
    else if( type == LABEL )
      return annotation.getLabel();
    else if( type == DESCRIPTION )
      return annotation.getDescription();

    throw new IllegalArgumentException( Messages.get( "org.kalypso.wizard.KalypsoNAProjectWizardPage.3" ) + type ); //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
   */
  public void widgetSelected( final SelectionEvent e )
  {
    Combo c;
    final Widget w = e.widget;
    if( w instanceof Button )
    {
      final Button b = (Button) w;
      if( b == okButton )
        handleOKSelection();
      if( b == resetButton )
        handelResetSelection();
      if( b == browseButton )
        handleFileBrowse();
      if( b == skipRadioButton )
      {
        final boolean selection = b.getSelection();

        m_mappingGroup.setVisible( selection );
        fileGroup.setVisible( selection );

        if( !selection )
        {
          // remove mapping
          mapping = null;
          // clear filefield
          m_fileField.setText( "" ); //$NON-NLS-1$
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
          customCS = c.getText();
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
  public void widgetDefaultSelected( final SelectionEvent e )
  {
    // do nothing
  }

  /**
   * @see org.eclipse.swt.events.KeyListener#keyPressed(org.eclipse.swt.events.KeyEvent)
   */
  public void keyPressed( final KeyEvent e )
  {
    final Widget w = e.widget;
    if( w instanceof Combo && e.character == SWT.CR )
    {
      validate();
    }

  }

  /**
   * @see org.eclipse.swt.events.KeyListener#keyReleased(org.eclipse.swt.events.KeyEvent)
   */
  public void keyReleased( final KeyEvent e )
  {
    // do nothing
  }

  public String getDefaultCRS( )
  {
    return defaultCRS;
  }
}