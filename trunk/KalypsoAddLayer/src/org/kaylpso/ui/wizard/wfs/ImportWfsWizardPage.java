package org.kaylpso.ui.wizard.wfs;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.util.ArrayList;

import org.deegree.gml.GMLProperty;
import org.deegree.services.wfs.capabilities.FeatureType;
import org.deegree.services.wfs.capabilities.WFSCapabilities;
import org.deegree_impl.gml.GMLDocument_Impl;
import org.deegree_impl.services.wfs.capabilities.WFSCapabilitiesFactory;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
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
import org.kalypso.ui.ImageProvider;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.xml.XMLTools;
import org.kalypsodeegree_impl.gml.schema.GMLSchema;
import org.kalypsodeegree_impl.gml.schema.GMLSchemaCache;
import org.w3c.dom.Document;
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

public class ImportWfsWizardPage extends WizardPage implements ModifyListener, SelectionListener
{
  private Combo url = null;

  private Text pass = null;

  private Text user = null;

  protected Button getDefault;

  protected Button postDefault;

  protected Text bufferText;

  protected Text timeoutText;

  protected static final String timeoutDefault = "3";

  protected static final String bufferDefault = "10";

  private Composite layerSelection;

  private FeatureType[] ftl = null;

  private List listLeftSide;

  private Button addLayer;

  private Button removeLayer;

  private List listRightSide;

  private Group layerGroup;

  private Composite buttonComposite;

  private Label labelUser;

  private Label labelPass;

  private Button m_authentification;

  private Label labelUrl;
  private static final int MIN_LIST_WITH = 150;

  private static final int MIN_DIALOG_WIDTH = 400;

  private static final int MIN_LIST_HIGHT = 150;

  //private Button m_multiLayers;

  /*
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
    labelUrl = new Label( fieldGroup, SWT.NONE );
    labelUrl.setText( "URL:" );
    labelUrl.setToolTipText( "URL des Web Feature Servers (WFS)" );
    labelUrl.setLayoutData( new GridData( SWT.END, SWT.DEFAULT, false, false ) );

    java.util.List recent = new ArrayList();
    if( true )
    { // FIXME: store in preferences like WMSWizardPage
      recent.add( "http://134.28.87.71:8080/deegreewms/wfs" );
      recent.add( "http://www.refractions.net:8080/geoserver/wfs" );
      // );
      recent.add( "http://www2.dmsolutions.ca/cgi-bin/mswfs_gmap" );
      // );
      recent.add( "http://gws2.pcigeomatics.com/wfs1.0.0/wfs" );
      recent.add("http://134.28.77.120/flowswms/wfs");
    }
    GridData gridData = new GridData( GridData.FILL_HORIZONTAL );
    gridData.widthHint = 400;

    url = new Combo( fieldGroup, SWT.BORDER | SWT.READ_ONLY );
    url.setItems( (String[])recent.toArray( new String[recent.size()] ) );
    url.setVisibleItemCount( 15 );
    url.setLayoutData( gridData );
    url.select( 0 );
    url.addModifyListener( this );

    // add spacer
    Label label = new Label( fieldGroup, SWT.SEPARATOR | SWT.HORIZONTAL );
    label.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 3, 3 ) );

    // usr
    labelUser = new Label( fieldGroup, SWT.NONE );
    labelUser.setText( "Benutzername:" );
    labelUser.setToolTipText( "Server Login Benutzername" );
    labelUser.setLayoutData( new GridData( SWT.END, SWT.DEFAULT, false, false ) );

    user = new Text( fieldGroup, SWT.BORDER );
    user.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    user.addModifyListener( this );

    // pass
    labelPass = new Label( fieldGroup, SWT.NONE );
    labelPass.setText( "Passwort:" );
    labelPass.setToolTipText( "Server Login Passwort" );
    labelPass.setLayoutData( new GridData( SWT.END, SWT.DEFAULT, false, false ) );

    pass = new Text( fieldGroup, SWT.BORDER | SWT.PASSWORD );
    pass.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    pass.addModifyListener( this );

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
    layerGroup = new Group( composite, SWT.CENTER );
    layerGroup.setText("Verfügbare Themen des Web Feature Servers");
    GridLayout gridLayout = new GridLayout();
    gridLayout.marginHeight = 10;
    gridLayout.horizontalSpacing = 10;
    gridLayout.verticalSpacing = 10;
    layerGroup.setLayout( gridLayout );
    layerGroup.setLayoutData( new GridData ( GridData.FILL_HORIZONTAL ) );

    layerSelection = new Composite( layerGroup, SWT.NULL );
    layerSelection.setLayout( new GridLayout( 3, false ) );
    layerSelection.setLayoutData(new GridData(MIN_DIALOG_WIDTH, SWT.DEFAULT));

    listLeftSide = new List( layerSelection, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL | SWT.H_SCROLL );
    listLeftSide.setLayoutData(new GridData(MIN_LIST_WITH ,MIN_LIST_HIGHT));
    listLeftSide.setItems( getNamesFeatureType() );
    listLeftSide.addSelectionListener( this );

    buttonComposite = new Composite( layerSelection, SWT.NULL );
    buttonComposite.setLayout( new GridLayout() );
    buttonComposite.setLayoutData(new GridData( 50, SWT.DEFAULT));

    addLayer = new Button( buttonComposite, SWT.PUSH );
    addLayer.setImage( ImageProvider.IMAGE_STYLEEDITOR_FORWARD.createImage() );
    addLayer.setToolTipText( "Hinzufügen eines Themas aus der Kartenansicht" );
    addLayer.addSelectionListener( this );
    removeLayer = new Button( buttonComposite, SWT.PUSH );
    removeLayer.setImage( ImageProvider.IMAGE_STYLEEDITOR_BACKWARD.createImage() );
    removeLayer.setToolTipText( "Entfernen eines Themas aus der Kartenansicht" );
    removeLayer.addSelectionListener( this );

    listRightSide = new List( layerSelection, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL );
    listRightSide.setLayoutData(new GridData( MIN_LIST_WITH, MIN_LIST_HIGHT ));
    listRightSide.addSelectionListener(this); //(TODO implementation of next page to choose table)

    
    //    m_multiLayers = new Button( layerSelection, SWT.CHECK );
//    m_multiLayers.setText( "Themen zusammenfassen" );
//    m_multiLayers.setToolTipText( "Alle ausgewählte Themen als Gruppe in Karte einfügen" );
//    m_multiLayers.setSelection( false );
//    m_multiLayers.setEnabled( false );

    layerGroup.setVisible( false );
    layerGroup.pack();

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
    if( e.widget == url )
    {
      if( validateURLField() )
      {
        ftl = getCapabilites( url.getText() );
        if( ftl == null || url.getText().length() < 1 )
          layerSelection.setVisible( false );
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
    if( ftl.length < 1 || ftl == null )
      layerSelection.setVisible( false );
    if( listRightSide.getItemCount() < 1 )
      removeLayer.setEnabled( false );
    else
      removeLayer.setEnabled( true );
    if( listRightSide.getSelectionCount() > 1 || ftl != null )
    {
      layerGroup.setVisible( true );
      listLeftSide.setItems( getNamesFeatureType() );
//      listLeftSide.redraw();
//      listLeftSide.pack();
//      listRightSide.redraw();
//      listRightSide.pack();
//      layerSelection.pack();
      layerGroup.redraw();
      layerGroup.pack();
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
    if(e.widget instanceof Button ){
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
    if( b.equals( addLayer ) )
    {
      String[] selection = listLeftSide.getSelection();
      if( selection != null )
      {
        addNewSelection( selection );
        updateLayerSelection();
      }
    }
    if( b.equals( removeLayer ) )
    {

      String[] selection = listRightSide.getSelection();
      if( selection != null )
      {
//        m_multiLayers.setEnabled( true );
        removeSelection( selection );
        updateLayerSelection();
      }
    }
    if( b.equals( m_authentification ) )
    {
      if( m_authentification.getSelection() )
      {
        labelUser.setVisible( true );
        labelPass.setVisible( true );
        pass.setVisible( true );
        user.setVisible( true );
      }
      else
      {
        labelUser.setVisible( false );
        labelPass.setVisible( false );
        pass.setVisible( false );
        user.setVisible( false );
      }
    }
//    if( listRightSide.getItemCount() > 1 )
//      m_multiLayers.setEnabled( true );
//    else
//      m_multiLayers.setEnabled( false );
    }
    if(e.widget instanceof List ){
//     l = (List)e.widget;
//     if(l == listRightSide && listRightSide.getItemCount() > 0)
//     {
//      String[] selection = listRightSide.getSelection();
//      for( int i = 0; i < selection.length; i++ )
//      {
//        FeatureType ft = getFeatureType(selection[i]);
//      }
//     }
    }
    getContainer().updateButtons();
  }

  private FeatureType getFeatureType( String ftName )
  {
    for( int i = 0; i < ftl.length; i++ )
    {
      FeatureType ft = ftl[i];
      if(ft.getName().equals(ftName) )
      {
      org.kalypsodeegree.model.feature.FeatureType featureType = getFeatureType( ft );
      FeatureTypeProperty[] properties = featureType.getProperties();
      //make table with properties and add to Tab to show in dialog to choose for creating a table
      }
      
    }
    return null;
  }

  public URL getDescribeFeatureTypeURL( String layer) throws MalformedURLException
  {
    String s = url.getText();
    System.out.print(s);
    return new URL( s
        + "?SERVICE=WFS&VERSION=1.0.0&REQUEST=DescribeFeatureType&typeName=" + layer );  
  }
  private org.kalypsodeegree.model.feature.FeatureType getFeatureType( FeatureType ft )
  {
    try
    {
      URL schemaURL =getDescribeFeatureTypeURL(ft.getName());
      URLConnection cox = schemaURL.openConnection();
      InputStream is = cox.getInputStream();
      Reader reader = new InputStreamReader(is);
      Document doc = XMLTools.parse(reader);
      GMLDocument_Impl gml = new GMLDocument_Impl(doc);
      GMLProperty[] properties = gml.getRoot().getProperties();
      for( int i = 0; i < properties.length; i++ )
      {
        GMLProperty property = properties[i];
        System.out.println(property.getPropertyValue());
        if(property.getPropertyValue().equals(ft.getName()))
        System.out.println("");
      }
      
    }
    catch( MalformedURLException e )
    {
      e.printStackTrace();
    }
    catch( IOException e )
    {
      e.printStackTrace();
    }
    catch( SAXException e )
    {
      e.printStackTrace();
    }
    return null;
  }

  private void removeSelection( String[] selection )
  {
    String[] list = listRightSide.getItems();
    for( int i = 0; i < selection.length; i++ )
    {
      String item = selection[i];
      if( contains( item, list ) )
        listRightSide.remove( item );

    }
  }

  private void addNewSelection( String[] selection )
  {
    String[] original = listRightSide.getItems();
    if( original.length > 0 )
      for( int i = 0; i < selection.length; i++ )
      {
        String item = selection[i];
        if( !contains( item, original ) )
          listRightSide.add( item );

      }
    else
      listRightSide.setItems( selection );
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
    if( ftl != null )
    {
      String[] res = new String[ftl.length];

      for( int i = 0; i < ftl.length; i++ )
      {
        res[i] = ftl[i].getName();
      }
      return res;
    }
    String[] dummy =
    {
      ""
    };
    return dummy;
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
    if( url.getText().length() == 0 )
    {
      setErrorMessage( "Das URL Feld darf nicht leer sein" );
      setPageComplete( false );
      return false;
    }
    else if( isvalidURL( url.getText() ) == false )
    {
      setPageComplete( false );
      return false;
    }
    //setPageComplete(true);
    setErrorMessage( null );
    setMessage( null );
    return true;
  }

  public String[] getLayers()
  {
    //multilayer does not make sense
//    if( !isMultiLayer() )
      return listRightSide.getItems();

//    String[] array = listRightSide.getItems();
//    String result = "";
//    for( int i = 0; i < array.length; i++ )
//    {
//      String layer = array[i];
//      if( i > 1 )
//        result = result + "," + layer;
//      else
//        result = result + layer;
//    }
//    return new String[]
//    {
//      result
//    };

  }

//  private boolean isMultiLayer()
//  {
//    return m_multiLayers.getSelection();
//  }

  public String getUrl()
  {
    return url.getText();
  }
 
  

public String guessGeometryType(String layer ) throws Exception
{
  final GMLSchema schema = GMLSchemaCache.getSchema(getDescribeFeatureTypeURL(layer));
  final org.kalypsodeegree.model.feature.FeatureType[] featureTypes = schema.getFeatureTypes();
  for( int i = 0; i < featureTypes.length; i++ )
  {
    final org.kalypsodeegree.model.feature.FeatureType type = featureTypes[i];    
    // TODO create style that fits to schema
    final FeatureTypeProperty property = type.getProperty("GEOM");    
    if(property!=null)
      return property.getType();
  }
  return null;
}
     
}