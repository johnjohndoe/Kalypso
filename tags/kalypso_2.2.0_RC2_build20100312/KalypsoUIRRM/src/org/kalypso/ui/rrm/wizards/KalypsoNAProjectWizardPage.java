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
package org.kalypso.ui.rrm.wizards;

import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.StringTokenizer;

import org.apache.commons.io.FilenameUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
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
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Widget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.transformation.CRSHelper;
import org.kalypso.ui.rrm.i18n.Messages;
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

  // widgets
  private Group fileGroup;

  private Label fileLabel;

  private Composite m_sourceGroup;

  private final IFeatureType targetFT;

  private Combo m_checkCRS;

  private Text m_fileField;

  private Composite m_topComposite;

  private Button skipRadioButton;

  private Button browseButton;

  // Geodata
  private String customCS = null;

  // aus Preferences
  private String defaultCRS = null;

  private GMLWorkspace m_shapeWorkspace;

  private File m_shapeFile;

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
    setDescription( Messages.getString( "KalypsoNAProjectWizardPage.PageDescription" ) ); //$NON-NLS-1$
    targetFT = featureType;
  }

  /**
   * @param pageName
   * @param title
   * @param titleImage
   */
  public KalypsoNAProjectWizardPage( final String pageName, final String title, final ImageDescriptor titleImage, final IFeatureType featureType )
  {
    super( pageName, title, titleImage );
    setDescription( Messages.getString( "KalypsoNAProjectWizardPage.PageDescription" ) ); //$NON-NLS-1$
    targetFT = featureType;

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
      setErrorMessage( Messages.getString( "KalypsoNAProjectWizardPage.ErrorMessageNotSupportedCS" ) ); //$NON-NLS-1$
      pageComplete = false;
    }

    setPageComplete( pageComplete );
  }

  private boolean checkCRS( final String customCRS )
  {
    return CRSHelper.isKnownCRS( customCRS );
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
    fileGroup.setText( Messages.getString( "KalypsoNAProjectWizardPage.FileGroupText" ) ); //$NON-NLS-1$

    fileLabel = new Label( fileGroup, SWT.NONE );
    fileLabel.setText( Messages.getString( "KalypsoNAProjectWizardPage.FileLabelText" ) ); //$NON-NLS-1$

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
    browseButton.setText( Messages.getString( "KalypsoNAProjectWizardPage.BrowseButtonText" ) ); //$NON-NLS-1$
    browseButton.setLayoutData( new GridData( GridData.END ) );
    browseButton.addSelectionListener( this );
    final Label crsLabel = new Label( fileGroup, SWT.NONE );
    crsLabel.setText( Messages.getString( "KalypsoNAProjectWizardPage.CRSLabelText" ) ); //$NON-NLS-1$

    m_checkCRS = new Combo( fileGroup, SWT.NONE );

    availableCoordinateSystems( m_checkCRS );

    // String defaultCRS = KalypsoGisPlugin.getDefault().getCoordinatesSystem().getName();
    defaultCRS = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
    m_checkCRS.select( m_checkCRS.indexOf( defaultCRS ) );

    m_checkCRS.setToolTipText( Messages.getString( "KalypsoNAProjectWizardPage.CRSTooltip" ) ); //$NON-NLS-1$
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
    m_mappingGroup.setText( Messages.getString( "KalypsoNAProjectWizardPage.MappingGroupText" ) ); //$NON-NLS-1$
    final GridLayout topGroupLayout = new GridLayout( 2, false );
    m_mappingGroup.setLayout( topGroupLayout );

    final GridData topGroupGridData = new GridData( SWT.FILL, SWT.FILL, true, true );
    m_mappingGroup.setLayoutData( topGroupGridData );

    final ScrolledComposite topSCLMappingComposite = new ScrolledComposite( m_mappingGroup, SWT.V_SCROLL | SWT.BORDER );
    topSCLMappingComposite.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true, 2, 1 ) );
    topSCLMappingComposite.setExpandHorizontal( true );

    final GridData topSCLData = new GridData( SWT.FILL, SWT.FILL, true, true, 2, 1 );
    topSCLData.heightHint = 200;
    topSCLMappingComposite.setLayoutData( topSCLData );

    m_sourceGroup = new Composite( topSCLMappingComposite, SWT.NONE );
    m_sourceGroup.setLayout( new GridLayout( 2, true ) );

    topSCLMappingComposite.setContent( m_sourceGroup );

    // Source
    m_sourceGroup.setLayout( new GridLayout( 2, false ) );
    m_sourceGroup.setLayoutData( new GridData( SWT.FILL, SWT.BEGINNING, true, true ) );
    m_sourceGroup.setVisible( true );
//    m_sourceGroup.setText( Messages.get( "KalypsoNAProjectWizardPage.SourceGroupText" ) ); //$NON-NLS-1$
    final IPropertyType[] targetFtp = targetFT.getProperties();
    for( final IPropertyType ftp : targetFtp )
    {
      if( !ftp.getQName().getLocalPart().equals( "boundedBy" ) && ftp instanceof IValuePropertyType ) //$NON-NLS-1$
      {
        final Label text = new Label( m_sourceGroup, SWT.NONE );
        text.setText( getAnnotation( ftp, LABEL ) );
        text.setToolTipText( getAnnotation( ftp, TOOL_TIP ) );

        final Combo combo = new Combo( m_sourceGroup, SWT.READ_ONLY | SWT.DROP_DOWN | SWT.SINGLE );
        combo.setData( TARGET_KEY, ftp );
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

    // Reset buttons group
    resetButton = new Button( m_mappingGroup, SWT.PUSH );
    resetButton.setText( Messages.getString( "KalypsoNAProjectWizardPage.ResetButtonText" ) ); //$NON-NLS-1$
    resetButton.addSelectionListener( this );

    m_sourceGroup.setSize( m_sourceGroup.computeSize( SWT.DEFAULT, SWT.DEFAULT, true ) );
  }

  private IFeatureType getSourceFT( )
  {
    final Feature rootFeature = m_shapeWorkspace.getRootFeature();

    final IFeatureType rootFT = rootFeature.getFeatureType();
    final IRelationType ftp = (IRelationType) rootFT.getProperty( ShapeSerializer.PROPERTY_FEATURE_MEMBER );
    return ftp.getTargetFeatureType();
  }

  private void handelResetSelection( )
  {
    final Control[] cArray = m_sourceGroup.getChildren();
    for( final Control element : cArray )
    {
      if( element instanceof Combo )
      {
        final Combo combo = (Combo) element;
        combo.setData( SOURCE_KEY, KalypsoNAProjectWizard.NULL_KEY );
        combo.select( 0 );
      }
    }
    m_sourceGroup.redraw();
  }

  /**
   * Uses the standard container selection dialog to choose the new value for the container field.
   */

  private void handleFileBrowse( )
  {
    final FileDialog fdialog = new FileDialog( getShell(), SWT.OPEN | SWT.SINGLE );
    fdialog.setFilterExtensions( new String[] { "shp" } ); //$NON-NLS-1$
    fdialog.setText( Messages.getString( "KalypsoNAProjectWizardPage.BrowseText" ) ); //$NON-NLS-1$
    fdialog.setFilterNames( new String[] { "Shape Files (*.shp)", "All Files (*.*)" } ); //$NON-NLS-1$ //$NON-NLS-2$
    fdialog.setFilterExtensions( new String[] { "*.shp", //$NON-NLS-1$
        "*.*" } ); //$NON-NLS-1$
    fdialog.setFileName( "*.shp" ); //$NON-NLS-1$
    if( fdialog.open() != null )
      m_fileField.setText( fdialog.getFilterPath() + File.separator + fdialog.getFileName() );
  }

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

  /**
   * This method returns a HashMap with the user defined mapping of the source to the target. key = (String) target
   * property value = (String) source property
   *
   * @return map HashMap with the custom mapping
   */
  public HashMap<Object, Object> getMapping( )
  {
    final HashMap<Object, Object> map = new HashMap<Object, Object>();
    final Control[] cArray = m_sourceGroup.getChildren();
    for( final Control element : cArray )
    {
      if( element instanceof Combo )
      {
        final Combo c = (Combo) element;
        final IPropertyType target = (IPropertyType) c.getData( TARGET_KEY );
        final Object source = c.getData( SOURCE_KEY );
        if( source != null )
          map.put( target.getQName().getLocalPart(), source );
      }
    }// for i
    return map;
  }

  private boolean readShapeFile( final File shapeFile )
  {
    String cs = null;

    if( customCS == null )
      cs = getDefaultCRS();
    else
      cs = customCS;

    final String shapePath = shapeFile.getAbsolutePath();
    final String fileBase = FilenameUtils.removeExtension( shapePath );

    m_shapeWorkspace = null;
    setErrorMessage( null );
    try
    {
      m_shapeWorkspace = ShapeSerializer.deserialize( fileBase, cs );
      return true;
    }
    catch( final GmlSerializeException e )
    {
      e.printStackTrace();
      setErrorMessage( e.getLocalizedMessage() );

      final IStatus status = StatusUtilities.createStatus( IStatus.ERROR, e.getLocalizedMessage(), e );
      ErrorDialog.openError( getShell(), Messages.getString( "KalypsoNAProjectWizardPage.TextMessageReadError" ), Messages.getString( "KalypsoNAProjectWizardPage.MessageReadError", shapePath ), status ); //$NON-NLS-1$ //$NON-NLS-2$

      return false;
    }
  }

  /**
   * This method clears the mapping
   */

  public void updateSourceList( final IFeatureType srcFT )
  {
    final IPropertyType[] ftp = srcFT.getProperties();
    final Control[] cArray = m_sourceGroup.getChildren();

    for( final Control c : cArray )
    {
      if( c instanceof Combo )
      {
        final Combo combo = (Combo) c;
        combo.setData( SOURCE_KEY, null );
        combo.removeAll();
        combo.setSize( maxSourceComboWidth * 10, SWT.DEFAULT );

        combo.add( KalypsoNAProjectWizard.NULL_KEY );
        for( final IPropertyType element : ftp )
        {
          // checks if the mapping between types is possible
          if( element instanceof IValuePropertyType )
          {
            final IValuePropertyType fromPT = (IValuePropertyType) element;
            final IValuePropertyType toPT = (IValuePropertyType) combo.getData(TARGET_KEY);
            if( SpecialPropertyMapper.isValidMapping( fromPT.getValueClass(), toPT.getValueClass() ) )
              combo.add( element.getQName().getLocalPart() );
          }
        }// for j
        combo.select( 0 );
      }
    }
  }

  public boolean validateFile( final File file )
  {
    return file.exists();
  }

  protected void validateFileField( )
  {
    if( !skipRadioButton.getSelection() )
    {
      setPageComplete( true );
      setMessage( Messages.getString( "KalypsoNAProjectWizardPage.SkipMessage" ) ); //$NON-NLS-1$
      return;
    }

    m_shapeFile = null;

    // checks catchment field entry and file suffix
    final String fileFieldStr = m_fileField.getText();
    if( fileFieldStr.length() == 0 )
    {
      setErrorMessage( Messages.getString( "KalypsoNAProjectWizardPage.ErrorMessageChooseFile" ) ); //$NON-NLS-1$
      setPageComplete( false );
      return;
    }

    if( !fileFieldStr.toLowerCase().endsWith( ".shp" ) ) //$NON-NLS-1$
    {
      setErrorMessage( Messages.getString( "KalypsoNAProjectWizardPage.ErrorMessageWrongSuffix" ) ); //$NON-NLS-1$
      setPageComplete( false );
      return;
    }

    m_shapeFile = new File( fileFieldStr );
    if( validateFile( m_shapeFile ) == false )
    {
      setErrorMessage( Messages.getString( "KalypsoNAProjectWizardPage.ErrorMessageNotValidFile" ) ); //$NON-NLS-1$
      setPageComplete( false );
      return;
    }

    if( !readShapeFile( m_shapeFile ) )
      return;

    final IFeatureType ft = getSourceFT();
    setMaxSourceCombo( ft );
    updateSourceList( ft );

    setPageComplete( true );
    setErrorMessage( null );
    setMessage( null );
  }

  public List< ? > getFeatureList( )
  {
    final Feature rootFeature = m_shapeWorkspace.getRootFeature();
    final List< ? > featureList = (List< ? >) rootFeature.getProperty( ShapeSerializer.PROPERTY_FEATURE_MEMBER );
    return featureList;
  }

  @Override
  public void dispose( )
  {
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

    throw new IllegalArgumentException( Messages.getString( "org.kalypso.ui.rrm.wizards.KalypsoNAProjectWizardPage.3" ) + type ); //$NON-NLS-1$
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
          readShapeFile( m_shapeFile );
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