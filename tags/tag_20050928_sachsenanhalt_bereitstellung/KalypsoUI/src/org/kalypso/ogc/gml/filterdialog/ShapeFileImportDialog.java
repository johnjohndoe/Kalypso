/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.ogc.gml.filterdialog;

import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.MalformedURLException;
import java.net.URL;
import java.rmi.RemoteException;
import java.util.List;
import java.util.Vector;

import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.ui.dialogs.KalypsoResourceSelectionDialog;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree.graphics.sld.Layer;
import org.kalypsodeegree.graphics.sld.Style;
import org.kalypsodeegree.graphics.sld.StyledLayerDescriptor;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureAssociationTypeProperty;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.xml.XMLParsingException;
import org.kalypsodeegree_impl.graphics.sld.SLDFactory;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * 
 * TODO: insert type comment here
 * 
 * @author kuepfer
 */
public class ShapeFileImportDialog extends Dialog
{

  private static final int SIZING_TEXT_FIELD_WIDTH = 250;

  private Composite m_topComposite;

  private Group m_group;

  private Label m_sourceFileLabel;

  private Text m_sourceFileText;

  private Button m_browseButton;

  private Combo m_checkCRS;

  private Label m_styleLabel;

  private Text m_styleTextField;

  private Button m_browseButton2;

  private Label m_styleNameLabel;

  private Combo m_styleNameCombo;

  private Button m_checkDefaultStyleButton;

  private boolean m_checkDefaultStyle = false;

  private Path m_relativeSourcePath;

  private Path m_stylePath;

  private String m_styleName;

  private IWorkspaceRoot m_eclipseWorkspace = ResourcesPlugin.getWorkspace().getRoot();

  private CS_CoordinateSystem m_coordinateSystem;

  private GMLWorkspace m_workspace;

  private boolean m_withStyleSelection;

  /**
   *  
   */

  public ShapeFileImportDialog( Shell parentShell, boolean withStyleSelection )
  {
    super( parentShell );
    m_withStyleSelection = withStyleSelection;

  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  protected Control createDialogArea( Composite parent )
  {
    m_topComposite = (Composite)super.createDialogArea( parent );
    m_topComposite.setFont( parent.getFont() );

    initializeDialogUnits( parent );

    m_topComposite.setLayout( new GridLayout() );
    m_topComposite.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    // build wizard page
    createFileGroup( m_topComposite );
    if( m_withStyleSelection )
      createStyleGroup( m_topComposite );
    return m_topComposite;
  }

  /**
   * @param composite
   */
  private void createStyleGroup( Composite composite )
  {
    //  style
    Group styleGroup = new Group( composite, SWT.NULL );
    styleGroup.setText( "Style" );

    GridData data3 = new GridData();
    data3.horizontalAlignment = GridData.FILL;
    data3.grabExcessHorizontalSpace = true;
    styleGroup.setLayoutData( data3 );
    GridLayout gridLayout1 = new GridLayout();
    gridLayout1.numColumns = 3;
    styleGroup.setLayout( gridLayout1 );

    m_styleLabel = new Label( styleGroup, SWT.NONE );
    m_styleLabel.setText( "Datei : " );

    m_styleTextField = new Text( styleGroup, SWT.BORDER );
    GridData data4 = new GridData();
    data4.horizontalAlignment = GridData.FILL;
    data4.grabExcessHorizontalSpace = true;
    m_styleTextField.setLayoutData( data4 );
    m_styleTextField.setEditable( false );

    m_browseButton2 = new Button( styleGroup, SWT.PUSH );
    m_browseButton2.setText( "Durchsuchen..." );
    m_browseButton2.setLayoutData( new GridData( GridData.END ) );
    m_browseButton2.addSelectionListener( new SelectionAdapter()
    {

      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      public void widgetSelected( SelectionEvent e )
      {

        KalypsoResourceSelectionDialog dialog = createResourceDialog( new String[]
        { "sld" } );
        dialog.open();
        Object[] result = dialog.getResult();
        if( result != null )
        {
          Path resultPath = (Path)result[0];
          m_styleTextField.setText( resultPath.toString() );
          m_stylePath = resultPath;
          try
          {
            IPath basePath = m_eclipseWorkspace.getLocation();
            String styleUrl = basePath.toFile().toURL() + m_stylePath.removeFirstSegments( 1 ).toString();
            Reader reader = new InputStreamReader( ( new URL( styleUrl ) ).openStream() );
            StyledLayerDescriptor styledLayerDescriptor = SLDFactory.createSLD( reader );
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
            m_styleNameCombo.setItems( styleNames );
            m_styleNameCombo.select( 0 );
            m_styleName = styleNames[0];
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

    } );

    m_styleNameLabel = new Label( styleGroup, SWT.NONE );
    m_styleNameLabel.setText( "UserStyle name: " );

    m_styleNameCombo = new Combo( styleGroup, SWT.READ_ONLY );
    GridData data5 = new GridData();
    data5.horizontalAlignment = GridData.FILL;
    data5.grabExcessHorizontalSpace = true;
    m_styleNameCombo.setLayoutData( data5 );
    m_styleNameCombo.addSelectionListener( new SelectionAdapter()
    {

      public void widgetSelected( SelectionEvent e )
      {
        m_styleName = m_styleNameCombo.getText();
        //            validate();
      }
    } );

    Label dummyLabel = new Label( styleGroup, SWT.NONE );
    dummyLabel.setText( "" );

    m_checkDefaultStyleButton = new Button( styleGroup, SWT.CHECK );
    m_checkDefaultStyleButton.setSelection( m_checkDefaultStyle );
    m_checkDefaultStyleButton.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      public void widgetSelected( SelectionEvent e )
      {
        m_checkDefaultStyle = m_checkDefaultStyleButton.getSelection();
        if( m_checkDefaultStyleButton.getSelection() )
        {
          m_styleLabel.setEnabled( false );
          m_styleTextField.setEnabled( false );
          m_browseButton2.setEnabled( false );
          m_styleNameLabel.setEnabled( false );
          m_styleNameCombo.setEnabled( false );
        }
        else
        {
          m_styleLabel.setEnabled( true );
          m_styleTextField.setEnabled( true );
          m_browseButton2.setEnabled( true );
          m_styleNameLabel.setEnabled( true );
          m_styleNameCombo.setEnabled( true );
        }
      }
    } );

    Label defaultStyleLabel = new Label( styleGroup, SWT.NONE );
    defaultStyleLabel.setText( "Generate default style" );

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
    m_sourceFileText.addModifyListener( new ModifyListener()
    {

      public void modifyText( ModifyEvent e )
      {
      // TODO Auto-generated method stub

      }
    } );

    m_browseButton = new Button( m_group, SWT.PUSH );
    m_browseButton.setText( "Durchsuchen..." );
    m_browseButton.setLayoutData( new GridData( GridData.END ) );
    m_browseButton.addSelectionListener( new SelectionAdapter()
    {

      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      public void widgetSelected( SelectionEvent e )
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
    } );

    Label crsLabel = new Label( m_group, SWT.NONE );
    crsLabel.setText( "Koordinaten System: " );

    m_checkCRS = new Combo( m_group, SWT.NONE );

    availableCoordinateSystems( m_checkCRS );
    try
    {
      String defaultCS = KalypsoGisPlugin.getDefault().getCoordinatesSystem().getName();
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
    m_checkCRS.addSelectionListener( new SelectionAdapter()
    {

      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      public void widgetSelected( SelectionEvent e )
      {
        m_coordinateSystem = getCRS();
      }
    } );

    m_group.pack();

  }

  private void availableCoordinateSystems( Combo checkCRS )
  {
    ConvenienceCSFactoryFull factory = new ConvenienceCSFactoryFull();
    checkCRS.setItems( factory.getKnownCS() );
  }

  private KalypsoResourceSelectionDialog createResourceDialog( String[] fileResourceExtensions )
  {
    return new KalypsoResourceSelectionDialog( getShell(), m_eclipseWorkspace, "Select resource",
        fileResourceExtensions, m_eclipseWorkspace );
  }

  protected CS_CoordinateSystem getCRS()
  {
    return ConvenienceCSFactory.getInstance().getOGCCSByName( m_checkCRS.getText() );
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#okPressed()
   */
  protected void okPressed()
  {
    try
    {
      m_workspace = ShapeSerializer.deserialize( getShapeBaseFile().toString(), getCRS() );
    }
    catch( GmlSerializeException e )
    {
      IStatus status = StatusUtilities.createErrorStatus( e.getLocalizedMessage() );
      ErrorDialog.openError( getShell(), "Fehler beim laden einer ESRI Shapedatei", e.getMessage(), status );
    }
    super.okPressed();
  }

  public File getShapeBaseFile()
  {
    return new File( m_eclipseWorkspace.getLocation() + "/"
        + FileUtilities.nameWithoutExtension( m_relativeSourcePath.toString() ) );
  }

  public GM_Object getGeometry()
  {
    Feature root = m_workspace.getRootFeature();
    FeatureType featureType = root.getFeatureType();
    FeatureAssociationTypeProperty property2 = (FeatureAssociationTypeProperty)featureType
        .getProperty( ShapeSerializer.PROPERTY_FEATURE_MEMBER );
    int propertyPosition = property2.getAssociationFeatureType()
        .getPropertyPosition( ShapeSerializer.PROPERTY_GEOMETRY );
    FeatureList featureCollection = (FeatureList)root.getProperty( ShapeSerializer.PROPERTY_FEATURE_MEMBER );
    Feature feature = (Feature)featureCollection.get( 0 );
    GM_Object property = (GM_Object)feature.getProperty( propertyPosition );

    return property;
  }

  public GMLWorkspace getWorkspace()
  {
    return m_workspace;
  }
}
