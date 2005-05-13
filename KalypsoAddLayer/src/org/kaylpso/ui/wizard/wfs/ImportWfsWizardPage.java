package org.kaylpso.ui.wizard.wfs;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.HashSet;

import javax.naming.OperationNotSupportedException;

import org.deegree.services.wfs.capabilities.FeatureType;
import org.deegree.services.wfs.capabilities.WFSCapabilities;
import org.deegree.services.wms.StyleNotDefinedException;
import org.deegree_impl.services.wfs.capabilities.WFSCapabilitiesFactory;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
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
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Widget;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree.model.feature.FeatureAssociationTypeProperty;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree_impl.gml.schema.GMLSchema;
import org.kalypsodeegree_impl.gml.schema.GMLSchemaCache;
import org.kalypsodeegree_impl.gml.schema.GMLSchemaCatalog;
import org.xml.sax.SAXException;

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
/**
 * 
 * @author Kuepferle
 * 
 * */
public class ImportWfsWizardPage extends WizardPage implements ModifyListener, SelectionListener,
    FocusListener
{
  private Combo m_url = null;

  private Text m_pass = null;

  private Text m_user = null;

  protected Button getDefault;

  protected Button postDefault;

  protected Text bufferText;

  protected Text timeoutText;

  protected static final String timeoutDefault = "3";

  protected static final String bufferDefault = "10";

  private Composite m_layerSelection;

  private FeatureType[] m_featureType = null;

  private List m_listLeftSide;

  private Button m_addLayer;

  private Button m_removeLayer;

  private List m_listRightSide;

  private Group m_layerGroup;

  private Composite m_buttonComposite;

  private Label m_labelUser;

  private Label m_labelPass;

  private Button m_authentification;

  private Label m_labelUrl;

  private static final int MIN_LIST_WITH = 150;

  private static final int MIN_DIALOG_WIDTH = 400;

  private static final int MIN_LIST_HIGHT = 150;

  /**
   * 
   * @author kuepfer
   */
  public ImportWfsWizardPage( String pageName )
  {
    super( pageName );
    setTitle( "Web Feature Service einbinden" );
    setMessage( "Web Feature Service Daten einbinden." );
    setPageComplete( false );

  }

  /*
   * 
   * @author kuepfer
   */
  public ImportWfsWizardPage( String pageName, String title, ImageDescriptor titleImage )
  {
    super( pageName, title, titleImage );
    setPageComplete( false );

  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( Composite arg0 )
  {
    Composite composite = new Group( arg0, SWT.NULL );
    GridLayout layout = new GridLayout( 1, false );
    composite.setLayout( layout );

    createSourceFields( composite );
    m_authentification = new Button( composite, SWT.CHECK );
    m_authentification.setText( "Authentifizierung" );
    m_authentification
        .setToolTipText( "Eingabe von Benutzername und Passwort für den Zugriff auf den Service" );
    m_authentification.setSelection( true );
    m_authentification.addSelectionListener( this );

    createLayerSelectionControl( composite );
    composite.layout();
    composite.pack();
    setControl( arg0 );
    setPageComplete( false );

  }

  private void createSourceFields( Composite composite )
  {
    Group fieldGroup = new Group( composite, SWT.NULL );
    fieldGroup.setLayout( new GridLayout( 2, false ) );
    fieldGroup.setText( "Verbindungsdaten" );
    //  add url
    m_labelUrl = new Label( fieldGroup, SWT.NONE );
    m_labelUrl.setText( "URL:" );
    m_labelUrl.setToolTipText( "URL des Web Feature Servers (WFS)" );
    m_labelUrl.setLayoutData( new GridData( SWT.END, SWT.DEFAULT, false, false ) );

    java.util.List recent = new ArrayList();
    if( true )
    {
      recent.add( "http://134.28.87.75:8000/deegreewms/wfs" );
      recent.add( "http://localhost:8080/deegreewms/wfs" );
    }
    GridData gridData = new GridData( GridData.FILL_HORIZONTAL );
    gridData.widthHint = 400;

    m_url = new Combo( fieldGroup, SWT.BORDER | SWT.READ_ONLY );
    m_url.setItems( (String[])recent.toArray( new String[recent.size()] ) );
    m_url.setVisibleItemCount( 15 );
    m_url.setLayoutData( gridData );
    m_url.select( 0 );
    m_url.addModifyListener( this );
    m_url.addFocusListener( this );

    // add spacer
    Label label = new Label( fieldGroup, SWT.SEPARATOR | SWT.HORIZONTAL );
    label.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 3, 3 ) );

    // usr
    m_labelUser = new Label( fieldGroup, SWT.NONE );
    m_labelUser.setText( "Benutzername:" );
    m_labelUser.setToolTipText( "Server Login Benutzername" );
    m_labelUser.setLayoutData( new GridData( SWT.END, SWT.DEFAULT, false, false ) );

    m_user = new Text( fieldGroup, SWT.BORDER );
    m_user.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    m_user.addModifyListener( this );

    // pass
    m_labelPass = new Label( fieldGroup, SWT.NONE );
    m_labelPass.setText( "Passwort:" );
    m_labelPass.setToolTipText( "Server Login Passwort" );
    m_labelPass.setLayoutData( new GridData( SWT.END, SWT.DEFAULT, false, false ) );

    m_pass = new Text( fieldGroup, SWT.BORDER | SWT.PASSWORD );
    m_pass.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    m_pass.addModifyListener( this );

    /*
     * add advanced stuff
     * 
     * advancedTag = new Button( composite, SWT.CHECK );
     * advancedTag.setLayoutData( new GridData( SWT.LEFT, SWT.DEFAULT, false,
     * false ) ); advancedTag.setSelection( false );
     * advancedTag.addSelectionListener( this ); advancedTag.setText(
     * "Erweitert" ); advancedTag.setToolTipText( "Erweiterte Einstellungen" );
     * 
     * label = new Label( composite, SWT.NONE ); label.setLayoutData( new
     * GridData( SWT.DEFAULT, SWT.DEFAULT, false, false ) );
     * 
     * advanced = createAdvancedControl( composite ); advanced.setLayoutData(
     * new GridData( SWT.CENTER, SWT.DEFAULT, true, true, 2, 1 ) );
     */
  }

  private void createLayerSelectionControl( Composite composite )
  {
    m_layerGroup = new Group( composite, SWT.CENTER );
    m_layerGroup.setText( "Verfügbare Themen des Web Feature Servers" );
    GridLayout gridLayout = new GridLayout();
    gridLayout.marginHeight = 10;
    gridLayout.horizontalSpacing = 10;
    gridLayout.verticalSpacing = 10;
    m_layerGroup.setLayout( gridLayout );
    m_layerGroup.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );

    m_layerSelection = new Composite( m_layerGroup, SWT.NULL );
    m_layerSelection.setLayout( new GridLayout( 3, false ) );
    m_layerSelection.setLayoutData( new GridData( MIN_DIALOG_WIDTH, SWT.DEFAULT ) );

    m_listLeftSide = new List( m_layerSelection, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL
        | SWT.H_SCROLL );
    m_listLeftSide.setLayoutData( new GridData( MIN_LIST_WITH, MIN_LIST_HIGHT ) );
    m_listLeftSide.setItems( getNamesFeatureType() );
    m_listLeftSide.addSelectionListener( this );

    m_buttonComposite = new Composite( m_layerSelection, SWT.NULL );
    m_buttonComposite.setLayout( new GridLayout() );
    m_buttonComposite.setLayoutData( new GridData( 50, SWT.DEFAULT ) );

    m_addLayer = new Button( m_buttonComposite, SWT.PUSH );
    m_addLayer.setImage( ImageProvider.IMAGE_STYLEEDITOR_FORWARD.createImage() );
    m_addLayer.setToolTipText( "Hinzufügen eines Themas zur Kartenansicht" );
    m_addLayer.addSelectionListener( this );
    m_removeLayer = new Button( m_buttonComposite, SWT.PUSH );
    m_removeLayer.setImage( ImageProvider.IMAGE_STYLEEDITOR_BACKWARD.createImage() );
    m_removeLayer.setToolTipText( "Entfernen des gewählten Themas aus der Kartenansicht" );
    m_removeLayer.addSelectionListener( this );

    m_listRightSide = new List( m_layerSelection, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL );
    m_listRightSide.setLayoutData( new GridData( MIN_LIST_WITH, MIN_LIST_HIGHT ) );
    m_listRightSide.addSelectionListener( this );

    m_layerGroup.setVisible( false );
    m_layerGroup.pack();

  }

  // private Composite createAdvancedControl( Composite arg0 )
  //  {
  //    advanced = new Group( arg0, SWT.BORDER );
  //    advanced.setLayout( new GridLayout( 2, false ) );
  //
  //    // get
  //    Label label = new Label( advanced, SWT.NONE );
  //    label.setText( "Get" );
  //    label.setToolTipText( "Http Get Protocol" );
  //    label.setLayoutData( new GridData( SWT.CENTER, SWT.DEFAULT, false, false )
  // );
  //
  //    getDefault = new Button( advanced, SWT.CHECK );
  //    getDefault.setLayoutData( new GridData( SWT.CENTER, SWT.DEFAULT, false,
  // false ) );
  //    getDefault.setSelection( false );
  //    getDefault.addSelectionListener( this );
  //
  //    // post
  //    label = new Label( advanced, SWT.NONE );
  //    label.setText( "Post" );
  //    label.setToolTipText( "Http Post Protocol" );
  //    label.setLayoutData( new GridData( SWT.CENTER, SWT.DEFAULT, false, false )
  // );
  //
  //    postDefault = new Button( advanced, SWT.CHECK );
  //    postDefault.setLayoutData( new GridData( SWT.CENTER, SWT.DEFAULT, false,
  // false ) );
  //    postDefault.setSelection( false );
  //    postDefault.addSelectionListener( this );
  //
  //    // add spacer
  //    label = new Label( advanced, SWT.SEPARATOR | SWT.HORIZONTAL );
  //    label.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 3
  // ) );
  //
  //    // buffer
  //    label = new Label( advanced, SWT.NONE );
  //    label.setText( "Buffer Groesse (Features)" );
  //    label
  //        .setToolTipText( "Groessere buffer sizes benötigen mehr Speicher, es erhöht
  // aber die Geschwindigkeit. Die buffer-size wird in Anzahl Features
  // gemessen." );
  //    label.setLayoutData( new GridData( SWT.CENTER, SWT.DEFAULT, false, false )
  // );
  //
  //    bufferText = new Text( advanced, SWT.BORDER | SWT.RIGHT );
  //    bufferText.setLayoutData( new GridData( GridData.FILL, SWT.DEFAULT, true,
  // false, 1, 1 ) );
  //    bufferText.setText( bufferDefault );
  //    bufferText.setTextLimit( 5 );
  //    bufferText.addModifyListener( this );
  //
  //    // timeout
  //    label = new Label( advanced, SWT.NONE );
  //    label.setText( "Timeout (Sekunden)" );
  //    label
  //        .setToolTipText( "Längeres timeouts unterstützt unzuverlässigere
  // Netzwerkverbindungen, führt unter umständen aber auch zu längeren render
  // Zeiten. Die timeout-size wird in Sekunden gemessen." );
  //    label.setLayoutData( new GridData( SWT.CENTER, SWT.DEFAULT, false, false )
  // );
  //
  //    timeoutText = new Text( advanced, SWT.BORDER | SWT.RIGHT );
  //    timeoutText.setLayoutData( new GridData( GridData.FILL, SWT.DEFAULT, true,
  // false, 1, 1 ) );
  //    timeoutText.setText( timeoutDefault );
  //    timeoutText.setTextLimit( 5 );
  //    timeoutText.addModifyListener( this );
  //
  //    advanced.setVisible( false );
  //
  //    return advanced;
  //  }

  /**
   * TODO summary sentence for modifyText ...
   * 
   * @see org.eclipse.swt.events.ModifyListener#modifyText(org.eclipse.swt.events.ModifyEvent)
   * @param e
   */
  public void modifyText( ModifyEvent e )
  {
    if( e.widget != null && e.widget instanceof Text )
      ( (Text)e.widget ).setForeground( null );
    if( e.widget == m_url )
    {
      if( validateURLField() )
      {
        m_featureType = getCapabilites( m_url.getText() );
        if( m_featureType == null || m_url.getText().length() < 1 )
          m_layerSelection.setVisible( false );
        updateLayerSelection();
        setErrorMessage( null );
      }
      else
        setPageComplete( false );
    }
    getContainer().updateButtons();
  }

  private void updateLayerSelection()
  {
    if( m_featureType.length < 1 || m_featureType == null )
      m_layerSelection.setVisible( false );
    if( m_listRightSide.getItemCount() < 1 )
      m_removeLayer.setEnabled( false );
    else
      m_removeLayer.setEnabled( true );
    if( m_listRightSide.getSelectionCount() > 1 || m_featureType != null )
    {
      m_layerGroup.setVisible( true );
      m_listLeftSide.setItems( getNamesFeatureType() );
      //      listLeftSide.redraw();
      //      listLeftSide.pack();
      //      listRightSide.redraw();
      //      listRightSide.pack();
      //      layerSelection.pack();
      m_layerGroup.redraw();
      m_layerGroup.pack();
      setPageComplete( true );
    }
    getControl().redraw();
  }

  private boolean isvalidURL( String urlToCheck )
  {
    try
    {
      URL validUrl = new URL( urlToCheck );
      URLConnection con = validUrl.openConnection();
      System.out.println( urlToCheck );
      con.connect();

    }
    catch( MalformedURLException e )
    {
      e.printStackTrace();
      setErrorMessage( "Die gewählte URL ist ungültig" );
      return false;
    }
    catch( IOException e )
    {
      e.printStackTrace();
      setErrorMessage( "Konnte keine Verbindung zum Server aufbauen" );
      return false;
    }
    return true;
  }

  /**
   * TODO summary sentence for widgetSelected ...
   * 
   * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
   * @param e
   */
  public void widgetSelected( SelectionEvent e )
  {
    Button b;
    if( e.widget instanceof Button )
    {
      b = (Button)e.widget;
      //    if( b.equals( getDefault ) )
      //    {
      //      // allow get was clicked
      //      if( getDefault.getSelection() && postDefault.getSelection() )
      //      {
      //        postDefault.setSelection( false );
      //      }
      //    }
      //    else
      //    {
      //      if( b.equals( postDefault ) )
      //      {
      //        if( postDefault.getSelection() && getDefault.getSelection() )
      //        {
      //          getDefault.setSelection( false );
      //        }
      //      }
      //      else
      //      {
      //        if( b.equals( advancedTag ) )
      //        {
      //          advanced.setVisible( advancedTag.getSelection() );
      //        }
      //      }
      if( b.equals( m_addLayer ) )
      {
        String[] selection = m_listLeftSide.getSelection();
        if( selection != null )
        {
          addNewSelection( selection );
          updateLayerSelection();
        }
      }
      if( b.equals( m_removeLayer ) )
      {

        String[] selection = m_listRightSide.getSelection();
        if( selection != null )
        {
          removeSelection( selection );
          updateLayerSelection();
        }
      }
      if( b.equals( m_authentification ) )
      {
        if( m_authentification.getSelection() )
        {
          m_labelUser.setVisible( true );
          m_labelPass.setVisible( true );
          m_pass.setVisible( true );
          m_user.setVisible( true );
        }
        else
        {
          m_labelUser.setVisible( false );
          m_labelPass.setVisible( false );
          m_pass.setVisible( false );
          m_user.setVisible( false );
        }
      }
    }
    getContainer().updateButtons();
  }

  //  private FeatureType getFeatureType( String ftName )
  //  {
  //    for( int i = 0; i < m_featureType.length; i++ )
  //    {
  //      FeatureType ft = m_featureType[i];
  //      if( ft.getName().equals( ftName ) )
  //      {
  //        org.kalypsodeegree.model.feature.FeatureType featureType = getFeatureType(
  // ft );
  //        FeatureTypeProperty[] properties = featureType.getProperties();
  //      }
  //
  //    }
  //    return null;
  //  }

  public URL[] getDescribeFeatureTypeURLs( String[] layers ) throws MalformedURLException
  {
    URL[] urls = new URL[layers.length];
    for( int i = 0; i < layers.length; i++ )
    {
      String layer = layers[i];
      urls[i] = getSchemaURL( layer );

    }
    return urls;
  }

  private URL getSchemaURL( String layer ) throws MalformedURLException
  {
    return new URL( getUrl() + "?SERVICE=WFS&VERSION=1.0.0&REQUEST=DescribeFeatureType&typeName="
        + layer );
  }

  //  private org.kalypsodeegree.model.feature.FeatureType getFeatureType(
  // FeatureType ft )
  //  {
  //    try
  //    {
  //      URL schemaURL = getDescribeFeatureTypeURL( ft.getName() );
  //      URLConnection cox = schemaURL.openConnection();
  //      InputStream is = cox.getInputStream();
  //      Reader reader = new InputStreamReader( is );
  //      Document doc = XMLTools.parse( reader );
  //      GMLDocument_Impl gml = new GMLDocument_Impl( doc );
  //      GMLProperty[] properties = gml.getRoot().getProperties();
  //      for( int i = 0; i < properties.length; i++ )
  //      {
  //        GMLProperty property = properties[i];
  //        System.out.println( property.getPropertyValue() );
  //        if( property.getPropertyValue().equals( ft.getName() ) )
  //          System.out.println( "" );
  //      }
  //
  //    }
  //    catch( MalformedURLException e )
  //    {
  //      e.printStackTrace();
  //    }
  //    catch( IOException e )
  //    {
  //      e.printStackTrace();
  //    }
  //    catch( SAXException e )
  //    {
  //      e.printStackTrace();
  //    }
  //    return null;
  //  }

  private void removeSelection( String[] selection )
  {
    String[] list = m_listRightSide.getItems();
    for( int i = 0; i < selection.length; i++ )
    {
      String item = selection[i];
      if( contains( item, list ) )
        m_listRightSide.remove( item );

    }
  }

  private void addNewSelection( String[] selection )
  {
    String[] original = m_listRightSide.getItems();
    if( original.length > 0 )
      for( int i = 0; i < selection.length; i++ )
      {
        String item = selection[i];
        if( !contains( item, original ) )
          m_listRightSide.add( item );

      }
    else
      m_listRightSide.setItems( selection );
  }

  /**
   * Double click in list, or return from url control.
   * 
   * @see org.eclipse.swt.events.SelectionListener#widgetDefaultSelected(org.eclipse.swt.events.SelectionEvent)
   * @param e
   */
  public void widgetDefaultSelected( SelectionEvent e )
  {
    if( getWizard().canFinish() )
    {
      getWizard().performFinish();
    }
  }

  private FeatureType[] getCapabilites( String service )
  {
    WFSCapabilities wfsCapabilites = null;
    try
    {

      final URL urlGetCap = new URL( service + "?"
          + "SERVICE=WFS&VERSION=1.0.0&REQUEST=GetCapabilities" );
      final URLConnection conGetCap = urlGetCap.openConnection();
      conGetCap.addRequestProperty( "SERVICE", "WFS" );
      conGetCap.addRequestProperty( "VERSION", "1.0.0" );
      conGetCap.addRequestProperty( "REQUEST", "GetCapabilities" );
      InputStream isGetCap = conGetCap.getInputStream();
      wfsCapabilites = WFSCapabilitiesFactory
          .createCapabilities( new InputStreamReader( isGetCap ) );
      //search all availabele layers on this service
      return wfsCapabilites.getFeatureTypeList().getFeatureTypes();

    }
    catch( IOException e )
    {
      e.printStackTrace();
      setErrorMessage( "Die URL des Servers ist ungültig, der Server ist off-line oder verweigert die Verbindung." );
      return null;
    }
    catch( SAXException e )
    {
      e.printStackTrace();
      //TODO handel exeption
      setErrorMessage( "Fehler beim lesen der Capabilies: " + e );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      setErrorMessage( "Fehler beim absetzen des GetCapabilites Request" + e );
    }
    return null;
  }

  private String[] getNamesFeatureType()
  {
    if( m_featureType != null )
    {
      String[] res = new String[m_featureType.length];

      for( int i = 0; i < m_featureType.length; i++ )
      {
        res[i] = m_featureType[i].getName();
      }
      return res;
    }
    return new String[]
    {
      ""
    };
  }

  private boolean contains( String item, String[] list )
  {

    for( int i = 0; i < list.length; i++ )
    {
      String listItem = list[i];
      if( item.equals( listItem ) )
        return true;
    }

    return false;
  }

  private boolean validateURLField()
  {
    //   checks catchment field entry and file suffix
    if( m_url.getText().length() == 0 )
    {
      setErrorMessage( "Das URL Feld darf nicht leer sein" );
      setPageComplete( false );
      return false;
    }
    else if( isvalidURL( m_url.getText() ) == false )
    {
      setPageComplete( false );
      return false;
    }
    //setPageComplete(true);
    setErrorMessage( null );
    setMessage( null );
    return true;
  }

  public String[] getSelectedFeatureNames()
  {
    return m_listRightSide.getItems();
  }

  public org.kalypsodeegree.model.feature.FeatureType[] getSelectedFeatureTypes() throws Exception
  {
    String[] selectedLayers = m_listRightSide.getSelection();
    org.kalypsodeegree.model.feature.FeatureType[] res = new org.kalypsodeegree.model.feature.FeatureType[selectedLayers.length];
    for( int i = 0; i < selectedLayers.length; i++ )
    {
      String name = selectedLayers[i];
      GMLSchema schema = getFeatureTypeSchema( name );
      org.kalypsodeegree.model.feature.FeatureType[] ft = schema.getFeatureTypes();
      for( int j = 0; j < ft.length; j++ )
      {
        org.kalypsodeegree.model.feature.FeatureType featureType = ft[j];
        if( featureType.getName().equals( name ) )
        {
          res[i] = featureType;
        }
      }
    }
    return res;
  }

  public String getUrl()
  {
    return m_url.getText();
  }

  public void cacheFeatureTypeSchema() throws Exception
  {
    URL[] urls = getDescribeFeatureTypeURLs( getSelectedFeatureNames() );
    for( int i = 0; i < urls.length; i++ )
    {
      URL url = urls[i];
      GMLSchemaCatalog.getSchema( url );
    }
  }

  public String guessGeometryType() throws Exception
  {
    final org.kalypsodeegree.model.feature.FeatureType[] featureTypes = getSelectedFeatureTypes();
    for( int i = 0; i < featureTypes.length; i++ )
    {
      final org.kalypsodeegree.model.feature.FeatureType type = featureTypes[i];
      // TODO create style that fits to schema
      final FeatureTypeProperty property = type.getProperty( "GEOM" );
      if( property != null )
        return property.getType();
      // check for virtual properties
      FeatureTypeProperty[] vProperty = type.getVirtuelFeatureTypeProperty();
      if( vProperty != null )
      {
        for( int j = 0; j < vProperty.length; j++ )
        {
          FeatureTypeProperty vp = vProperty[i];
          if( vp.isGeometryProperty() )
            return vp.getType();

        }
      }
    }
    return null;
  }

  /**
   * @see org.eclipse.swt.events.FocusListener#focusGained(org.eclipse.swt.events.FocusEvent)
   */
  public void focusGained( FocusEvent e )
  {
    //do nothing
  }

  /**
   * @see org.eclipse.swt.events.FocusListener#focusLost(org.eclipse.swt.events.FocusEvent)
   */
  public void focusLost( FocusEvent e )
  {
    Widget w = e.widget;
    if( w.equals( m_url ) )
    {
      if( validateURLField() )
      {
        m_featureType = getCapabilites( getUrl() );
        if( m_featureType == null || m_url.getText().length() < 1 )
          m_layerSelection.setVisible( false );
        updateLayerSelection();
        setErrorMessage( null );
      }
      else
      {
        setPageComplete( false );
        setErrorMessage( "Die eingegebene URL ist ungültig" );
      }
    }
  }

  /**
   * This method returns the schema of a selected feature.
   * 
   * @param layer
   *          name of feature (layer from WFS)
   * @return schema of the feature
   * 
   *  
   */
  public GMLSchema getFeatureTypeSchema( String layer ) throws Exception
  {
    return GMLSchemaCatalog.getSchema( getSchemaURL( layer ) );
  }

  /**
   * This method returns a featureType from a specific feature property passed
   * as a java.lang.Class object.
   * 
   * @param layer
   *          name of layer to get the FeatureType
   * @return returns a hash set of feature type properties that is passed trough
   *         <em>clazz</em> parameter.
   *  
   */
  private org.kalypsodeegree.model.feature.FeatureType[] getFeatureTypes( String[] layer )

  {
    HashSet res = new HashSet();
    GMLSchema featureTypeSchema = null;
    for( int i = 0; i < layer.length; i++ )
    {
      String l = layer[i];

      try
      {
        featureTypeSchema = getFeatureTypeSchema( l );
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
      org.kalypsodeegree.model.feature.FeatureType[] featureTypes = featureTypeSchema
          .getFeatureTypes();
      for( int j = 0; j < featureTypes.length; j++ )
      {
        org.kalypsodeegree.model.feature.FeatureType property = featureTypes[j];

        if( property.getName().equals( l ) )
        {
          res.add( property );
        }
      }
    }
    return (org.kalypsodeegree.model.feature.FeatureType[])res
        .toArray( new org.kalypsodeegree.model.feature.FeatureType[res.size()] );
  }

  /**
   * This Method guesses a geometry type for a feature layer from a wfs
   * response. Does not support multi geometry features.
   * 
   * @param layer
   *          layer name of the requestet feature
   * @return type name of geometry property (first one found)
   */
  public String guessGeometryType( String layer )
  {
    try
    {
      GMLSchema schema = getFeatureTypeSchema( layer );
      org.kalypsodeegree.model.feature.FeatureType[] featureTypes = schema.getFeatureTypes();
      for( int i = 0; i < featureTypes.length; i++ )
      {
        org.kalypsodeegree.model.feature.FeatureType featureType = featureTypes[i];
        FeatureTypeProperty[] properties = featureType.getProperties();
        for( int j = 0; j < properties.length; j++ )
        {
          FeatureTypeProperty property = properties[j];
          if( property.isGeometryProperty() )
          {
            return property.getType();
          }
        }
        FeatureTypeProperty[] vProperty = featureType.getVirtuelFeatureTypeProperty();
        if( vProperty != null )
        {
          for( int j = 0; j < vProperty.length; j++ )
          {
            FeatureTypeProperty vp = vProperty[i];
            if( vp.isGeometryProperty() )
              return vp.getType();
          }
        }
      }
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    return null;
  }

  /**
   * @throws Exception,
   *           OperationNotSupportedException
   * @throws MalformedURLException
   * 
   * 
   *  
   */
  public String guessFeaturePath( String layer ) throws MalformedURLException, Exception,
      OperationNotSupportedException

  {
    GMLSchema schema = GMLSchemaCatalog.getSchema( getSchemaURL( layer ) );
    org.kalypsodeegree.model.feature.FeatureType[] featureTypes = schema.getFeatureTypes();
    for( int i = 0; i < featureTypes.length; i++ )
    {
      org.kalypsodeegree.model.feature.FeatureType ft = featureTypes[i];
      FeatureTypeProperty[] properties = ft.getProperties();
      for( int j = 0; j < properties.length; j++ )
      {
        FeatureTypeProperty property = properties[j];
        if( property instanceof FeatureAssociationTypeProperty )
        {
          return property.getName();
        }

      }
    }
    throw new OperationNotSupportedException( "Guess of feature path failed!" );
  }

  public URL setDefautltStyle( String layer ) throws StyleNotDefinedException
  {

    return KalypsoGisPlugin.getDefault().getDefaultStyleFactory().getDefaultStyle(
        ( getFeatureTypes( new String[]
        {
          layer
        } )[0] ), null );
  }

  public void removeListeners()
  {
    m_addLayer.removeSelectionListener( this );
    m_listLeftSide.removeSelectionListener( this );
    m_listRightSide.removeSelectionListener( this );
    m_url.removeModifyListener( this );
    m_url.removeFocusListener( this );
    m_removeLayer.removeSelectionListener( this );

  }
}