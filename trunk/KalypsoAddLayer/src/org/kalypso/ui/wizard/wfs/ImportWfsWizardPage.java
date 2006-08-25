/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.ui.wizard.wfs;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import javax.naming.OperationNotSupportedException;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
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
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.GMLSchemaCatalog;
import org.kalypso.gmlschema.KalypsoGMLSchemaPlugin;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.filterdialog.dialog.FilterDialog;
import org.kalypso.ogc.wfs.IWFSCapabilities;
import org.kalypso.ogc.wfs.IWFSLayer;
import org.kalypso.ogc.wfs.WFSUtilities;
import org.kalypso.ui.ImageProvider;
import org.kalypsodeegree.filterencoding.Filter;

/**
 * @author Kuepferle
 */
public class ImportWfsWizardPage extends WizardPage
{
  private Combo m_url = null;

  Text m_pass = null;

  Text m_user = null;

  protected Button getDefault;

  protected Button postDefault;

  protected Text bufferText;

  protected Text timeoutText;

  protected static final String timeoutDefault = "3";

  protected static final String bufferDefault = "10";

  // private HashMap<String, IFeatureType> m_featureTypes = new HashMap<String, IFeatureType>();

  private ListViewer m_listLeftSide;

  private Button m_addLayer;

  private Button m_removeLayer;

  private ListViewer m_listRightSide;

  private Group m_layerGroup;

  // private Composite m_buttonComposite;

  Label m_labelUser;

  Label m_labelPass;

  Button m_authentification;

  private Label m_labelUrl;

  // private static final int MIN_LIST_WITH = 150;
  //
  // private static final int MIN_DIALOG_WIDTH = 400;
  //
  // private static final int MIN_LIST_HIGHT = 150;

  private IWFSCapabilities m_wfsCapabilites = null;

  private Button m_addFilterButton;

  private HashMap<IWFSLayer, Filter> m_filter = new HashMap<IWFSLayer, Filter>();

  private final SelectionListener m_urlSelectionListener = new SelectionListener()
  {

    public void widgetSelected( final SelectionEvent e )
    {
      reloadServer();
    }

    public void widgetDefaultSelected( SelectionEvent e )
    {
      reloadServer();
    }
  };

  private final SelectionListener m_authSelectionListener = new SelectionAdapter()
  {
    @Override
    public void widgetSelected( SelectionEvent e )
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
      reloadServer();
    }
  };

  private final SelectionListener m_userSelectionListener = new SelectionAdapter()
  {
    @Override
    public void widgetSelected( SelectionEvent e )
    {
      reloadServer();
    }

    @Override
    public void widgetDefaultSelected( SelectionEvent e )
    {
      reloadServer();
    }
  };

  private final SelectionListener m_passSelectionListener = new SelectionAdapter()
  {
    @Override
    public void widgetSelected( SelectionEvent e )
    {
      reloadServer();
    }

    @Override
    public void widgetDefaultSelected( SelectionEvent e )
    {
      reloadServer();
    }
  };

  private final ISelectionChangedListener m_leftSelectionListener = new ISelectionChangedListener()
  {

    public void selectionChanged( SelectionChangedEvent event )
    {
      updateButtons();
    }
  };

  private final SelectionListener m_addButtonSelectionListener = new SelectionAdapter()
  {
    @Override
    public void widgetSelected( SelectionEvent e )
    {
      addButtonPressed();
    }
  };

  private final SelectionListener m_removeButtonSelectionListener = new SelectionAdapter()
  {
    @Override
    public void widgetSelected( SelectionEvent e )
    {
      removeButtonPressed();
    }
  };

  private final SelectionListener m_filterButtonSelectionListener = new SelectionAdapter()
  {
    @Override
    public void widgetSelected( SelectionEvent e )
    {
      filterPressed();
    }
  };

  private final ISelectionChangedListener m_rightSelectionListener = new ISelectionChangedListener()
  {
    public void selectionChanged( SelectionChangedEvent event )
    {
      updateButtons();

    }
  };

  private final ModifyListener m_urlModifyListener = new ModifyListener()
  {
    public void modifyText( final ModifyEvent e )
    {
      revalidatePage();
    }
  };

  final static ILabelProvider labelProvider = new LabelProvider()
  {
    @Override
    public String getText( Object element )
    {
      if( element instanceof IWFSLayer )
      {
        String title = ((IWFSLayer) element).getTitle();
        if( title == null )
          return ((IWFSLayer) element).getQName().getLocalPart();
        return title;
      }
      return "...";
    }
  };

  final static IStructuredContentProvider contentProvider = new IStructuredContentProvider()
  {

    public void inputChanged( Viewer viewer, Object oldInput, Object newInput )
    {
      // TODO Auto-generated method stub
    }

    public void dispose( )
    {
      // TODO Auto-generated method stub
    }

    public Object[] getElements( Object input )
    {
      if( input instanceof List )
        return ((List) input).toArray();
      return null;
    }
  };

  private Composite m_leftsideButtonC;

  // private Label m_authLabel;

  public ImportWfsWizardPage( final String pageName )
  {
    super( pageName );
    setTitle( "Web Feature Service einbinden" );
    setMessage( "Web Feature Service Daten einbinden." );
    setPageComplete( false );
  }

  public ImportWfsWizardPage( final String pageName, final String title, final ImageDescriptor titleImage )
  {
    super( pageName, title, titleImage );
    setPageComplete( false );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    final Composite top = new Composite( parent, SWT.NULL );
    top.setLayout( new GridLayout() );
    top.setLayoutData( new GridData() );
    final Group composite = new Group( top, SWT.NULL );
    composite.setLayout( new GridLayout( 1, false ) );
    composite.setLayoutData( new GridData() );

    createSourceFields( composite );

    createLayerSelectionControl( composite );

    setControl( top );
    setPageComplete( false );
  }

  private void createSourceFields( final Composite parent )
  {
    final Group fieldGroup = new Group( parent, SWT.NULL );
    fieldGroup.setLayout( new GridLayout( 2, false ) );
    fieldGroup.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    fieldGroup.setText( "Verbindungsdaten" );

    // add url
    m_labelUrl = new Label( fieldGroup, SWT.NONE );
    m_labelUrl.setText( "URL:" );
    m_labelUrl.setToolTipText( "URL des Web Feature Servers (WFS)" );
    m_labelUrl.setLayoutData( new GridData( SWT.END, SWT.DEFAULT, false, false ) );

    // initialize availabel Servers
    ArrayList<String> catalog = ((ImportWfsSourceWizard) getWizard()).getCatalog();
    if( catalog == null )
      catalog = new ArrayList<String>();
    m_url = new Combo( fieldGroup, SWT.BORDER );
    m_url.setItems( catalog.toArray( new String[catalog.size()] ) );
    m_url.setVisibleItemCount( 15 );

    final GridData gridData = new GridData( GridData.FILL_HORIZONTAL );
    gridData.widthHint = 400;
    m_url.setLayoutData( gridData );
    m_url.addSelectionListener( m_urlSelectionListener );
    m_url.addModifyListener( m_urlModifyListener );

    // add spacer
    final Label label = new Label( fieldGroup, SWT.SEPARATOR | SWT.HORIZONTAL );
    label.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 3, 3 ) );

    // dummy to keep the layoutaut
    // new Label( fieldGroup, SWT.NONE ).setLayoutData( new GridData() );
    m_authentification = new Button( fieldGroup, SWT.CHECK | SWT.LEFT );
    GridData gridDataAuth = new GridData( GridData.FILL_HORIZONTAL );
    gridDataAuth.horizontalSpan = 2;
    m_authentification.setLayoutData( gridDataAuth );
    m_authentification.setText( "Authentifizierung" );
    m_authentification.setToolTipText( "Einblenden der Eingabefelder f�r Benutzername und Passwort f�r den Zugriff auf gesch�tzte Services" );
    m_authentification.setSelection( false );
    m_authentification.addSelectionListener( m_authSelectionListener );

    // usr
    m_labelUser = new Label( fieldGroup, SWT.NONE );
    m_labelUser.setText( "Benutzername:" );
    m_labelUser.setToolTipText( "Benutzername f�r den gew�hlten Server" );
    m_labelUser.setLayoutData( new GridData( SWT.END, SWT.DEFAULT, false, false ) );
    m_labelUser.setVisible( false );

    m_user = new Text( fieldGroup, SWT.BORDER );
    m_user.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    m_user.addSelectionListener( m_userSelectionListener );
    m_user.setVisible( false );

    // pass
    m_labelPass = new Label( fieldGroup, SWT.NONE );
    m_labelPass.setText( "Passwort:" );
    m_labelPass.setToolTipText( "Passwort f�r den gew�hlten Server" );
    m_labelPass.setLayoutData( new GridData( SWT.END, SWT.DEFAULT, false, false ) );
    m_labelPass.setVisible( false );

    m_pass = new Text( fieldGroup, SWT.BORDER | SWT.PASSWORD );
    m_pass.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    m_pass.addSelectionListener( m_passSelectionListener );
    m_pass.setVisible( false );

    /*
     * add advanced stuff advancedTag = new Button( composite, SWT.CHECK ); advancedTag.setLayoutData( new GridData(
     * SWT.LEFT, SWT.DEFAULT, false, false ) ); advancedTag.setSelection( false ); advancedTag.addSelectionListener(
     * this ); advancedTag.setText( "Erweitert" ); advancedTag.setToolTipText( "Erweiterte Einstellungen" ); label = new
     * Label( composite, SWT.NONE ); label.setLayoutData( new GridData( SWT.DEFAULT, SWT.DEFAULT, false, false ) );
     * advanced = createAdvancedControl( composite ); advanced.setLayoutData( new GridData( SWT.CENTER, SWT.DEFAULT,
     * true, true, 2, 1 ) );
     */
    fieldGroup.pack();
  }

  private void createLayerSelectionControl( Composite composite )
  {
    m_layerGroup = new Group( composite, SWT.CENTER );
    m_layerGroup.setText( "Verf�gbare Themen des Web Feature Servers" );
    GridLayout gridLayout = new GridLayout( 4, false );
    gridLayout.marginHeight = 10;
    gridLayout.horizontalSpacing = 10;
    gridLayout.verticalSpacing = 10;
    m_layerGroup.setLayout( gridLayout );
    m_layerGroup.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );

    // = new List( m_layerGroup, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL | SWT.H_SCROLL );
    m_listLeftSide = new ListViewer( m_layerGroup, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL | SWT.H_SCROLL );

    m_listLeftSide.getControl().setLayoutData( new GridData( GridData.FILL_BOTH ) );
    m_listLeftSide.setLabelProvider( labelProvider );
    m_listLeftSide.setContentProvider( contentProvider );

    m_listLeftSide.addSelectionChangedListener( m_leftSelectionListener );
    // addSelectionListener( m_leftSelectionListener );

    m_addLayer = new Button( m_layerGroup, SWT.PUSH );
    m_addLayer.setImage( ImageProvider.IMAGE_STYLEEDITOR_FORWARD.createImage() );
    m_addLayer.setToolTipText( "Hinzuf�gen eines Themas zur Kartenansicht" );
    m_addLayer.addSelectionListener( m_addButtonSelectionListener );
    m_addLayer.setEnabled( false );
    m_listRightSide = new ListViewer( m_layerGroup, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL );
    m_listRightSide.getControl().setLayoutData( new GridData( GridData.FILL_BOTH ) );
    m_listRightSide.setLabelProvider( labelProvider );
    m_listRightSide.setContentProvider( contentProvider );

    m_listRightSide.addSelectionChangedListener( m_rightSelectionListener );
    m_leftsideButtonC = new Composite( m_layerGroup, SWT.NULL );
    m_leftsideButtonC.setLayout( new GridLayout() );
    m_leftsideButtonC.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    m_leftsideButtonC.setLayoutData( new GridData( 40, SWT.DEFAULT ) );
    m_removeLayer = new Button( m_leftsideButtonC, SWT.PUSH );
    m_removeLayer.setEnabled( false );
    m_removeLayer.setImage( ImageProvider.IMAGE_STYLEEDITOR_REMOVE.createImage() );
    m_removeLayer.setToolTipText( "Entfernen des gew�hlten Themas aus der Kartenansicht" );
    m_removeLayer.addSelectionListener( m_removeButtonSelectionListener );
    m_addFilterButton = new Button( m_leftsideButtonC, SWT.NULL );
    m_addFilterButton.setImage( ImageProvider.IMAGE_FILTERDIALOG_ADD_FILTER.createImage() );
    m_addFilterButton.setToolTipText( "Erstellen/hinzuf�gen eines Filters f�r ein WFS Querable Request" );
    m_addFilterButton.addSelectionListener( m_filterButtonSelectionListener );
    m_addFilterButton.setEnabled( false );
    m_layerGroup.setVisible( true );
    m_layerGroup.pack();
  }

  // private Composite createAdvancedControl( Composite arg0 )
  // {
  // advanced = new Group( arg0, SWT.BORDER );
  // advanced.setLayout( new GridLayout( 2, false ) );
  //
  // // get
  // Label label = new Label( advanced, SWT.NONE );
  // label.setText( "Get" );
  // label.setToolTipText( "Http Get Protocol" );
  // label.setLayoutData( new GridData( SWT.CENTER, SWT.DEFAULT, false, false )
  // );
  //
  // getDefault = new Button( advanced, SWT.CHECK );
  // getDefault.setLayoutData( new GridData( SWT.CENTER, SWT.DEFAULT, false,
  // false ) );
  // getDefault.setSelection( false );
  // getDefault.addSelectionListener( this );
  //
  // // post
  // label = new Label( advanced, SWT.NONE );
  // label.setText( "Post" );
  // label.setToolTipText( "Http Post Protocol" );
  // label.setLayoutData( new GridData( SWT.CENTER, SWT.DEFAULT, false, false )
  // );
  //
  // postDefault = new Button( advanced, SWT.CHECK );
  // postDefault.setLayoutData( new GridData( SWT.CENTER, SWT.DEFAULT, false,
  // false ) );
  // postDefault.setSelection( false );
  // postDefault.addSelectionListener( this );
  //
  // // add spacer
  // label = new Label( advanced, SWT.SEPARATOR | SWT.HORIZONTAL );
  // label.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 3
  // ) );
  //
  // // buffer
  // label = new Label( advanced, SWT.NONE );
  // label.setText( "Buffer Groesse (Features)" );
  // label
  // .setToolTipText( "Groessere buffer sizes ben�tigen mehr Speicher, es erh�ht
  // aber die Geschwindigkeit. Die buffer-size wird in Anzahl Features
  // gemessen." );
  // label.setLayoutData( new GridData( SWT.CENTER, SWT.DEFAULT, false, false )
  // );
  //
  // bufferText = new Text( advanced, SWT.BORDER | SWT.RIGHT );
  // bufferText.setLayoutData( new GridData( GridData.FILL, SWT.DEFAULT, true,
  // false, 1, 1 ) );
  // bufferText.setText( bufferDefault );
  // bufferText.setTextLimit( 5 );
  // bufferText.addModifyListener( this );
  //
  // // timeout
  // label = new Label( advanced, SWT.NONE );
  // label.setText( "Timeout (Sekunden)" );
  // label
  // .setToolTipText( "L�ngeres timeouts unterst�tzt unzuverl�ssigere
  // Netzwerkverbindungen, f�hrt unter umst�nden aber auch zu l�ngeren render
  // Zeiten. Die timeout-size wird in Sekunden gemessen." );
  // label.setLayoutData( new GridData( SWT.CENTER, SWT.DEFAULT, false, false )
  // );
  //
  // timeoutText = new Text( advanced, SWT.BORDER | SWT.RIGHT );
  // timeoutText.setLayoutData( new GridData( GridData.FILL, SWT.DEFAULT, true,
  // false, 1, 1 ) );
  // timeoutText.setText( timeoutDefault );
  // timeoutText.setTextLimit( 5 );
  // timeoutText.addModifyListener( this );
  //
  // advanced.setVisible( false );
  //
  // return advanced;
  // }

  /**
   * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
   * @param e
   */
  public void widgetSelected( SelectionEvent e )
  {
    // Button b;
    if( e.widget instanceof Button )
    {
      // b = (Button) e.widget;
      // if( b.equals( getDefault ) )
      // {
      // // allow get was clicked
      // if( getDefault.getSelection() && postDefault.getSelection() )
      // {
      // postDefault.setSelection( false );
      // }
      // }
      // else
      // {
      // if( b.equals( postDefault ) )
      // {
      // if( postDefault.getSelection() && getDefault.getSelection() )
      // {
      // getDefault.setSelection( false );
      // }
      // }
      // else
      // {
      // if( b.equals( advancedTag ) )
      // {
      // advanced.setVisible( advancedTag.getSelection() );
      // }
      // }

    }
    getContainer().updateButtons();
  }

  private URL getSchemaURL( String layer ) throws MalformedURLException
  {
    return new URL( getUrl() + "?SERVICE=WFS&VERSION=1.0.0&REQUEST=DescribeFeatureType&typeName=" + layer );
  }

  private IWFSCapabilities getCapabilites( ) throws MalformedURLException, IOException, Exception
  {

    final URL urlGetCap = new URL( getUrl().toString().trim() );
    // + "?" + "SERVICE=WFS&VERSION=1.0.0&REQUEST=GetCapabilities" );
    // final URLConnection conGetCap = urlGetCap.openConnection();
    // conGetCap.addRequestProperty( "SERVICE", "WFS" );
    // conGetCap.addRequestProperty( "VERSION", "1.0.0" );
    // conGetCap.addRequestProperty( "REQUEST", "GetCapabilities" );
    return WFSUtilities.getCapabilites( urlGetCap );
    // InputStream isGetCap = conGetCap.getInputStream();
    // return WFSCapabilitiesFactory.createCapabilities( new InputStreamReader( isGetCap ) );
  }

  // /**
  // * This method returns the layer names in a sorted array
  // *
  // * @return an array of strings sorted alphabetically
  // */
  // private String[] getLayerNamesFromCapabilities( )
  // {
  // if( m_wfsCapabilites != null )
  // {
  // final TreeSet<String> list = new TreeSet<String>();
  // final IWFSLayer[] featureTypes = m_wfsCapabilites.getFeatureTypes();
  // // org.deegree.services.wfs.capabilities.FeatureType[] featureTypes =
  // // m_wfsCapabilites.getFeatureTypeList().getFeatureTypes();
  // for( int i = 0; i < featureTypes.length; i++ )
  // {
  // list.add( featureTypes[i].getTitle() );
  // }
  // return list.toArray( new String[list.size()] );
  // }
  // return new String[0];
  // }

  // public String[] getSelectedFeatureNames( )
  // {
  // return m_listRightSide.getItems();
  // }

  // public IFeatureType[] getSelectedFeatureTypes( ) throws Exception
  // {
  // String[] selectedFeatureNames = getSelectedFeatureNames();
  // return getFeatureTypes( selectedFeatureNames );
  // }

  public URL getUrl( ) throws MalformedURLException
  {
    return new URL( m_url.getText().trim() );
  }

  /**
   * This method returns a featureType from a specific feature property passed as a java.lang.Class object.
   * 
   * @param layer
   *          name of layer to get the IFeatureType
   * @return returns a hash set of feature type properties that is passed trough <em>clazz</em> parameter.
   */
  // private IFeatureType[] getFeatureTypes( String[] layer )
  //
  // {
  // HashSet<IFeatureType> res = new HashSet<IFeatureType>();
  // GMLSchema featureTypeSchema = null;
  // for( int i = 0; i < layer.length; i++ )
  // {
  // final String l = layer[i];
  // if( !m_featureTypes.containsKey( l ) )
  // {
  // try
  // {
  // final URL url = new URL( getUrl().toString().trim() +
  // "?SERVICE=WFS&VERSION=1.0.0&REQUEST=DescribeFeatureType&typeName=" + l );
  // featureTypeSchema = GMLSchemaCatalog.getSchema( url );
  // final IFeatureType featureType = featureTypeSchema.getFeatureType( l );
  // if( featureType != null )
  // m_featureTypes.put( l, featureType );
  // else if( l.indexOf( ":" ) >= 0 )
  // {
  // final String hackName = l.replaceAll( "^.+:", "" );
  // m_featureTypes.put( l, featureTypeSchema.getFeatureType( hackName ) );
  // }
  // }
  // catch( Exception e )
  // {
  // setMessage( e.getMessage() );
  // setPageComplete( false );
  // }
  // }
  // final IFeatureType featureType = m_featureTypes.get( l );
  // res.add( featureType );
  // }
  // return res.toArray( new IFeatureType[res.size()] );
  // }
  /**
   * @throws Exception,
   *           OperationNotSupportedException
   * @throws MalformedURLException
   */
  public String guessFeaturePath( String layer ) throws MalformedURLException, Exception, OperationNotSupportedException
  {
    // FIXME this method is not working properly, it has to be adjusted
    // when the Schema parser is adjusted!!!!!!
    final GMLSchemaCatalog schemaCatalog = KalypsoGMLSchemaPlugin.getDefault().getSchemaCatalog();
    final GMLSchema schema = schemaCatalog.getSchema( null, getSchemaURL( layer ) );
    final IFeatureType[] featureTypes = schema.getAllFeatureTypes();
    if( featureTypes.length == 1 )
      return featureTypes[0].getQName().getLocalPart();
    for( int i = 0; i < featureTypes.length; i++ )
    {
      IFeatureType ft = featureTypes[i];
      IPropertyType[] properties = ft.getProperties();
      for( int j = 0; j < properties.length; j++ )
      {
        IPropertyType property = properties[j];
        if( property instanceof IRelationType )
        {
          return property.getQName().getLocalPart();
        }

      }
    }
    throw new OperationNotSupportedException( "Guess of feature path failed!" );
  }

  public void removeListeners( )
  {
    m_addLayer.removeSelectionListener( m_addButtonSelectionListener );
    m_removeLayer.removeSelectionListener( m_removeButtonSelectionListener );
    m_listLeftSide.removeSelectionChangedListener( m_leftSelectionListener );
    m_listRightSide.removeSelectionChangedListener( m_rightSelectionListener );
  }

  public Filter getFilter( IWFSLayer wfsLayer )
  {
    return m_filter.get( wfsLayer );
  }

  protected void reloadServer( )
  {

    try
    {
      m_wfsCapabilites = getCapabilites();
    }
    catch( MalformedURLException e )
    {
      e.printStackTrace();
      setErrorMessage( "Ung�ltige URL" );
      setPageComplete( false );
    }
    catch( IOException e )
    {
      e.printStackTrace();
      setErrorMessage( "Die Verbindung zum WFS-Dienst konnte nicht hergestellt werden" );
      setPageComplete( false );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      setErrorMessage( "Fehler beim Lesen der Capabilites des WFS-Dienstes" );
      setPageComplete( false );
    }
    final IWFSLayer[] featureTypes = m_wfsCapabilites.getFeatureTypes();
    final List<IWFSLayer> list = Arrays.asList( featureTypes );
    m_listLeftSide.setInput( list );
    m_listRightSide.setInput( new ArrayList<IWFSLayer>() );
    updateButtons();
  }

  protected void addButtonPressed( )
  {
    final IStructuredSelection selection = (IStructuredSelection) m_listLeftSide.getSelection();
    if( selection != null )
    {
      final Object[] original = selection.toArray();
      for( int i = 0; i < original.length; i++ )
      {
        IWFSLayer wfsFT = (IWFSLayer) original[i];
        final List<IWFSLayer> input = getLayerList();
        input.add( wfsFT );
        m_listRightSide.add( wfsFT );
        // if( !ArrayUtils.contains( original, item ) )
      }
      updateButtons();
    }
  }

  /** Just for casting the list and suppress the warning */
  @SuppressWarnings("unchecked")
  private List<IWFSLayer> getLayerList( )
  {
    return (List<IWFSLayer>) m_listRightSide.getInput();
  }

  protected void removeButtonPressed( )
  {
    IStructuredSelection selection = (IStructuredSelection) m_listRightSide.getSelection();
    if( selection != null )
    {
      Object[] list = selection.toArray();
      for( int i = 0; i < list.length; i++ )
      {
        final IWFSLayer wfsFT = (IWFSLayer) list[i];
        final List<IWFSLayer> input = getLayerList();
        input.remove( wfsFT );
        m_listRightSide.remove( wfsFT );
        // if( ArrayUtils.contains( list, wfsFT ) )
      }
      updateButtons();
    }
  }

  protected void filterPressed( )
  {
    // the add filter button is only enabled if the selection size == 1
    final IStructuredSelection selection = (IStructuredSelection) m_listRightSide.getSelection();
    final IWFSLayer wfsFT = (IWFSLayer) selection.getFirstElement();
    // TODO: wenn es schon filter gibt zwischen erster Seite und dieser Seite ausw�hlen
    Filter oldFilter = m_filter.get( wfsFT );
    final IFeatureType ft = wfsFT.getFeatureType();
    final IWizardPage startingPage = getWizard().getStartingPage();
    if( startingPage instanceof ImportWfsFilterWizardPage )
      oldFilter = ((ImportWfsFilterWizardPage) startingPage).getFilter( ft );

    final FilterDialog dialog = new FilterDialog( getShell(), ft, null, oldFilter, null, false );
    int open = dialog.open();
    if( open == Window.OK )
    {
      m_filter.put( wfsFT, dialog.getFilter() );
    }
    revalidatePage();
  }

  protected void revalidatePage( )
  {
    final List<IWFSLayer> input = getLayerList();
    setPageComplete( input != null && !input.isEmpty() );
  }

  void updateButtons( )
  {
    final IStructuredSelection selection = (IStructuredSelection) m_listRightSide.getSelection();
    final Object firstElement = selection.getFirstElement();
    IFeatureType selectedFT = null;
    if( firstElement instanceof IWFSLayer )
    {
      final IWFSLayer wfsLayer = (IWFSLayer) firstElement;
      selectedFT = wfsLayer.getFeatureType();
    }
    m_addFilterButton.setEnabled( (selection).size() == 1 && selectedFT != null );
    m_removeLayer.setEnabled( (selection).size() > 0 );
    m_addLayer.setEnabled( ((IStructuredSelection) m_listLeftSide.getSelection()).size() > 0 );
    revalidatePage();
  }

  public IWFSLayer[] getChoosenFeatureLayer( )
  {
    final List<IWFSLayer> input = getLayerList();
    if( input == null )
      return new IWFSLayer[0];
    return input.toArray( new IWFSLayer[input.size()] );

  }

  // private IFeatureType getFeatureType( String layerName )
  // {
  // IFeatureType[] featureTypes = getFeatureTypes( new String[] { layerName } );
  // return featureTypes[0];
  // }
}