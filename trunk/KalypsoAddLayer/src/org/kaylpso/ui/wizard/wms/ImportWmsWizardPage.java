package org.kaylpso.ui.wizard.wms;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.TreeMap;

import org.deegree.services.wms.capabilities.Layer;
import org.deegree.services.wms.capabilities.WMSCapabilities;
import org.deegree.xml.XMLParsingException;
import org.deegree_impl.services.wms.capabilities.OGCWMSCapabilitiesFactory;
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
import org.kalypso.ui.ImageProvider;
import org.kalypsodeegree_impl.tools.NetWorker;

import sun.misc.BASE64Encoder;

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
public class ImportWmsWizardPage extends WizardPage implements ModifyListener, SelectionListener, FocusListener
{

  private Combo m_url = null;

  private Text m_pass = null;

  private Text m_user = null;

  private Layer[] m_wmsLayers = null;

  private List m_listLeftSide;

  private List m_listRightSide;
  
  private static final int MIN_DIALOG_WIDTH = 400;
  
  private static final int MIN_LIST_WITH = 150;  

//  private Button advancedTag = null;

  // private Composite advanced = null;

  //protected Button getDefault;

  //protected Button postDefault;

  //protected Text bufferText;

  //protected Text timeoutText;

//  protected static final String timeoutDefault = "3";
//
//  protected static final String bufferDefault = "10";

  private Composite m_layerSelection;

  private Button m_removeLayer;

  private Button m_addLayer;

  private Group m_layerGroup;

  private Composite m_buttonComposite;

  private Button m_multiLayers;

  private Button m_checkAuthentification;

  private Label m_labelUrl;

  private Label m_labelUser;

  private Label m_labelPass;

  private HashMap m_allRequestedLayers = new HashMap();

  //capabilites cache in this wizard
  private TreeMap m_capabilites = new TreeMap();

  private static final int MIN_LIST_HIGHT = 150;

  /*
   * 
   * @author kuepfer
   */
  public ImportWmsWizardPage( String pageName )
  {
    super( pageName );
    setTitle( "Web Map Service" );
    setMessage( "Layer eines Web Map Service einbinden" );
    setImageDescriptor( null );
    setPageComplete( false );
  }

  /*
   * 
   * @author kuepfer
   */
  public ImportWmsWizardPage( String pageName, String title, ImageDescriptor titleImage )
  {
    super( pageName, title, titleImage );
    setPageComplete( false );
  }

  public void createControl( Composite arg0 )
  {
    Composite composite = new Group( arg0, SWT.NULL );
    composite.setLayout( new GridLayout( 1, false ) );
    createUrlField( composite );
    m_checkAuthentification = new Button( composite, SWT.CHECK );
    m_checkAuthentification.setText( "Authentifizierung" );
    m_checkAuthentification
        .setToolTipText( "Eingabe von Benutzername und Passwort für den Zugriff auf den Service" );
    m_checkAuthentification.setSelection( true );
    m_checkAuthentification.addSelectionListener( this );
    createLayerSelection( composite );
    m_layerSelection.setVisible( false );
    setControl( arg0 );
    setPageComplete( false );
  }

  private void createLayerSelection( Composite composite )
  {
    m_layerGroup = new Group( composite, SWT.CENTER );
    m_layerGroup.setText( "Verfügbare Themen des Web Map Servers" );
    GridLayout gridLayout = new GridLayout();
    gridLayout.marginHeight = 10;
    gridLayout.horizontalSpacing = 10;
    gridLayout.verticalSpacing = 10;
    m_layerGroup.setLayout( gridLayout );
    m_layerGroup.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ));

    m_layerSelection = new Composite( m_layerGroup, SWT.NULL );
    m_layerSelection.setLayout( new GridLayout( 3, false ) );
    m_layerSelection.setLayoutData(new GridData(MIN_DIALOG_WIDTH, SWT.DEFAULT));

    m_listLeftSide = new List( m_layerSelection, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL | SWT.H_SCROLL );
    m_listLeftSide.setLayoutData(new GridData(MIN_LIST_WITH ,MIN_LIST_HIGHT));
    m_listLeftSide.setItems( getNamesFeatureType() );
//    m_listLeftSide.addSelectionListener( this );

    m_buttonComposite = new Composite( m_layerSelection, SWT.NULL );
    m_buttonComposite.setLayout( new GridLayout() );
    m_buttonComposite.setLayoutData(new GridData( 50, SWT.DEFAULT));
    
    m_addLayer = new Button( m_buttonComposite, SWT.PUSH );
    m_addLayer.setImage( ImageProvider.IMAGE_STYLEEDITOR_FORWARD.createImage() );
    m_addLayer.setToolTipText( "Hinzufügen eines Themas zu der Kartenansicht" );
    m_addLayer.addSelectionListener( this );
    m_removeLayer = new Button( m_buttonComposite, SWT.PUSH );
    m_removeLayer.setImage( ImageProvider.IMAGE_STYLEEDITOR_BACKWARD.createImage() );
    m_removeLayer.setToolTipText( "Entfernen eines Themas aus der Kartenansicht" );
    m_removeLayer.addSelectionListener( this );

    m_listRightSide = new List( m_layerSelection, SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL );
    m_listRightSide.setLayoutData(new GridData( MIN_LIST_WITH, MIN_LIST_HIGHT ));
//    m_listRightSide.addSelectionListener( this );

    m_multiLayers = new Button( m_layerSelection, SWT.CHECK );
    m_multiLayers.setText( "Themen zusammenfassen" );
    m_multiLayers
        .setToolTipText( "Alle ausgewählte Themen als Gruppe in Karte einfügen" );
    m_multiLayers.setSelection( false );
    m_multiLayers.setEnabled( false );
    m_layerGroup.setVisible( false );
    m_layerGroup.pack();
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createUrlField( Composite arg0 )
  {
    Group fieldGroup = new Group( arg0, SWT.NULL );
    fieldGroup.setText( "Verbindungsdaten" );
    fieldGroup.setLayout( new GridLayout( 2, false ) );

    // add url
    m_labelUrl = new Label( fieldGroup, SWT.NONE );
    m_labelUrl.setText( "URL:" );
    m_labelUrl.setToolTipText( "URL des Web Map Servers (WMS)" );
    m_labelUrl.setLayoutData( new GridData( SWT.END, SWT.DEFAULT, false, false ) );

    java.util.List recent = new ArrayList();
    if( true )
    { // FIXME: store in preferences like WMSWizardPage
      recent.add( "http://134.28.87.75:8000/deegreewms/wms" );
      recent.add("http://localhost:8080/deegreewms/wms");
    }
    GridData gridData = new GridData( GridData.FILL_HORIZONTAL );
    gridData.widthHint = 400;

    m_url = new Combo( fieldGroup, SWT.BORDER | SWT.READ_ONLY );
    m_url.addFocusListener(this);
   // m_url.addModifyListener( this );
    m_url.setItems( (String[])recent.toArray( new String[recent.size()] ) );
    m_url.setVisibleItemCount( 15 );
    m_url.setLayoutData( gridData );
    m_url.select( 0 );

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


  }

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
      m_listRightSide.removeAll();
      if( validateURLField() )
      {
        m_wmsLayers = getCapabilites( m_url.getText() );
        if( ( m_wmsLayers != null || m_wmsLayers.length > 0 ) || m_url.getText().length() < 1 )
          m_layerSelection.setVisible( true );
        else m_layerSelection.setVisible(false);
        updateLayerSelection();
        setErrorMessage( null );
      }
      else
      {
        setPageComplete( false );
        setMessage( "Die gewählte URL ist ungültig" );
      }
    }
    getContainer().updateButtons();
  }

  /**
   * TODO summary sentence for widgetSelected ...
   * 
   * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
   * @param e
   */
  public void widgetSelected( SelectionEvent e )
  {
    Button b = (Button)e.widget;
    if( b.equals( m_addLayer ) )
    {
      String[] selection = m_listLeftSide.getSelection();
      if( selection != null )
      {
        addNewSelection( selection );
        updateLayerSelection();
      }
      if( m_listRightSide.getItemCount() > 1 )
        m_multiLayers.setEnabled( true );
      else
        m_multiLayers.setEnabled( false );
    }
    if( b.equals( m_removeLayer ) )
    {
      String[] selection = m_listRightSide.getSelection();
      if( selection != null )
      {
        m_multiLayers.setEnabled( true );
        removeSelection( selection );
        updateLayerSelection();
      }
      if( m_listRightSide.getItemCount() > 1 )
        m_multiLayers.setEnabled( true );
      else
        m_multiLayers.setEnabled( false );
      updateLayerSelection();
    }
    if( b.equals( m_checkAuthentification ) )
    {
      if( m_checkAuthentification.getSelection() )
      {
        m_user.setVisible( true );
        m_pass.setVisible( true );
        m_labelUser.setVisible( true );
        m_labelPass.setVisible( true );
        updateLayerSelection();
      }
      else if( !m_checkAuthentification.getSelection() )
      {
        m_user.setVisible( false );
        m_pass.setVisible( false );
        m_labelUser.setVisible( false );
        m_labelPass.setVisible( false );
        updateLayerSelection();
      }

    }
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
    //    }

    getContainer().updateButtons();
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

  private void updateLayerSelection()
  {
    if( m_listRightSide.getItemCount() < 1 )
      m_removeLayer.setEnabled( false );
    else
      m_removeLayer.setEnabled( true );
    if( m_listRightSide.getSelectionCount() > 1 || m_wmsLayers != null )
    {
      m_layerGroup.setVisible(true);
      m_listLeftSide.setItems( getNamesFeatureType() );
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

  private Layer[] getCapabilites( String service )
  {
    WMSCapabilities capabilities = null;
    HashSet layers = new HashSet();

    if( m_capabilites.size() > 0 && m_capabilites.containsKey( service ) )
      capabilities = (WMSCapabilities)m_capabilites.get( service );
    if( capabilities == null )
      try
      {
        final OGCWMSCapabilitiesFactory wmsCapFac = new OGCWMSCapabilitiesFactory();

        URL urlService = new URL( service + "?SERVICE=WMS&VERSION=1.1.1&REQUEST=GetCapabilities" );
        final URLConnection c = urlService.openConnection();
        //checks authentification if needed adds the password and login to the stream
        if( NetWorker.requiresAuthentification(c) )
        {
          final String pw = m_user.getText() + ":" + m_pass.getText();
          final String epw = "Basic " + ( new BASE64Encoder() ).encode( pw.getBytes() );

          c.addRequestProperty( "Proxy-Authorization", epw ); 
        }
        NetWorker.configureProxy( c );
        
        c.addRequestProperty( "SERVICE", "WMS" );
        c.addRequestProperty( "VERSION", "1.1.1" );
        c.addRequestProperty( "REQUEST", "GetCapabilities" );
        final Reader reader = new InputStreamReader( c.getInputStream() );

        capabilities = wmsCapFac.createCapabilities( reader );
        //store url and capabilites in a map so it is only loaded once
        m_capabilites.put( service, capabilities );
      }
      catch( IOException e )
      {
        e.printStackTrace();
        // TODO: handle exception
        return null;
      }
      catch( XMLParsingException xmle )
      {
        // TODO: handle exception
        xmle.printStackTrace();
        return null;
      }
    Layer topLayer = null;
    topLayer = capabilities.getCapability().getLayer();
    Layer[] firstLayerSet = topLayer.getLayer();
    if( firstLayerSet == null )
      return ( new Layer[]
      {
        topLayer
      } );
    if( firstLayerSet.length < 1 )
      return ( new Layer[]
      {
        topLayer
      } );
    //recursive function call to get all available layers
    //TODO cascading WMS Layers on the server
    getAvaliableLayers( layers, firstLayerSet );
    m_allRequestedLayers.put( service, layers );
    return objectToLayer( layers );
  }

  private String[] getNamesFeatureType()
  {
    if( m_wmsLayers != null )
    {
      String[] res = new String[m_wmsLayers.length];

      for( int i = 0; i < m_wmsLayers.length; i++ )
      {
        res[i] = m_wmsLayers[i].getName();
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
    setPageComplete( true );
    setErrorMessage( null );
    setMessage( null );
    return true;
  }

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

  public String[] getLayers()
  {
    if( !isMultiLayer() )
      return m_listRightSide.getItems();

    String[] array = m_listRightSide.getItems();
    String result = "";
    for( int i = 0; i < array.length; i++ )
    {
      String layer = array[i];
      if( i > 0 )
        result = result + "," + layer;
      else
        result = result + layer;
    }
    return new String[]{result};

  }

  public URL getUrl()
  {
    URL serviceUrl = null;
    try
    {
      serviceUrl = new URL( this.m_url.getText() );
    }
    catch( MalformedURLException e )
    {
      //do nothing
    }
    return serviceUrl;
  }

  public boolean isMultiLayer()
  {
    return m_multiLayers.getSelection();
  }

  /**
   * Recursive function to collect all availabel layers on this Service TODO
   * cascading wms
   */
  private void getAvaliableLayers( HashSet layers, Layer[] layerTree )
  {
    for( int i = 0; i < layerTree.length; i++ )
    {
      Layer[] newLayer = layerTree[i].getLayer();
      if( newLayer.length > 0 )
      {
        //recursive function call
        getAvaliableLayers( layers, newLayer );
      }
      else
      {
//        System.out.println( layerTree[i].getName() );
        layers.add( layerTree[i] );
        continue;
      }
    }//for
  }//getAvailableLayers

  public Layer[] objectToLayer( HashSet map )
  {

    Layer[] res = new Layer[map.size()];
    int i = 0;
    for( Iterator iter = map.iterator(); iter.hasNext(); )
    {
      Layer l = (Layer)iter.next();
      res[i] = l;
      i++;

    }
    return res;

  }
  public String getPassWord()
  {
    return m_pass.getText();
  }

  public String getUserName()
  {
    return m_user.getText();
  }
  protected void removeListners(){
    m_checkAuthentification.removeSelectionListener( this );
    m_multiLayers.removeSelectionListener(this);
    m_listRightSide.removeSelectionListener(this);
    m_listLeftSide.removeSelectionListener(this);
    m_user.removeModifyListener(this);
    m_pass.removeModifyListener(this);
    m_url.removeSelectionListener(this);
    m_addLayer.removeSelectionListener(this);
    m_removeLayer.removeSelectionListener(this);
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
    if( e.widget == m_url ){
      m_listRightSide.removeAll();
      if( validateURLField() )
      {
        m_wmsLayers = getCapabilites( m_url.getText() );
        if( ( m_wmsLayers != null || m_wmsLayers.length > 0 ) || m_url.getText().length() < 1 )
          m_layerSelection.setVisible( true );
        else m_layerSelection.setVisible(false);
        updateLayerSelection();
        setErrorMessage( null );
      }
      else
      {
        setPageComplete( false );
        setMessage( "Die gewählte URL ist ungültig" );
      }
    }
    getContainer().updateButtons();
  }
}