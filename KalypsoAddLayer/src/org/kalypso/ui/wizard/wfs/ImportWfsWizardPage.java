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
package org.kalypso.ui.wizard.wfs;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.TreeSet;

import javax.naming.OperationNotSupportedException;

import org.apache.commons.lang.ArrayUtils;
import org.deegree.services.wfs.capabilities.WFSCapabilities;
import org.deegree_impl.services.wfs.capabilities.WFSCapabilitiesFactory;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.window.Window;
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
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Text;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.GMLSchemaCatalog;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.filterdialog.dialog.FilterDialog;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ui.ImageProvider;
import org.kalypsodeegree.filterencoding.Filter;

/**
 * @author Kuepferle
 */
public class ImportWfsWizardPage extends WizardPage
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

  private HashMap m_featureTypes = new HashMap();

  private List m_listLeftSide;

  private Button m_addLayer;

  private Button m_removeLayer;

  private List m_listRightSide;

  private Group m_layerGroup;

  // private Composite m_buttonComposite;

  private Label m_labelUser;

  private Label m_labelPass;

  private Button m_authentification;

  private Label m_labelUrl;

  private static final int MIN_LIST_WITH = 150;

  private static final int MIN_DIALOG_WIDTH = 400;

  private static final int MIN_LIST_HIGHT = 150;

  private WFSCapabilities m_wfsCapabilites = null;

  private Button m_addFilterButton;

  private HashMap m_filter = new HashMap();

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

  private final SelectionListener m_leftSelectionListener = new SelectionAdapter()
  {
    @Override
    public void widgetSelected( final SelectionEvent e )
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

  private final SelectionListener m_rightSelectionListener = new SelectionAdapter()
  {
    @Override
    public void widgetSelected( SelectionEvent e )
    {
      updateButtons();
    }

    @Override
    public void widgetDefaultSelected( SelectionEvent e )
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

  private Composite m_leftsideButtonC;

  private Label m_authLabel;

  private final IMapModell m_mapModel;

  public ImportWfsWizardPage( String pageName, IMapModell model )
  {
    super( pageName );
    m_mapModel = model;
    setTitle( "Web Feature Service einbinden" );
    setMessage( "Web Feature Service Daten einbinden." );
    setPageComplete( false );
  }

  public ImportWfsWizardPage( String pageName, String title, ImageDescriptor titleImage, IMapModell model )
  {
    super( pageName, title, titleImage );
    m_mapModel = model;
    setPageComplete( false );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    final Composite composite = new Group( parent, SWT.NULL );
    final GridLayout layout = new GridLayout( 1, false );
    composite.setLayout( layout );

    createSourceFields( composite );

    createLayerSelectionControl( composite );

    setControl( parent );
    setPageComplete( false );
  }

  private void createSourceFields( final Composite parent )
  {
    final Group fieldGroup = new Group( parent, SWT.NULL );
    fieldGroup.setLayout( new GridLayout( 2, false ) );
    fieldGroup.setText( "Verbindungsdaten" );
    fieldGroup.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );

    // add url
    m_labelUrl = new Label( fieldGroup, SWT.NONE );
    m_labelUrl.setText( "URL:" );
    m_labelUrl.setToolTipText( "URL des Web Feature Servers (WFS)" );
    m_labelUrl.setLayoutData( new GridData( SWT.END, SWT.DEFAULT, false, false ) );

    // initialize availabel Servers
    ArrayList catalog = ((ImportWfsSourceWizard) getWizard()).getCatalog();
    if( catalog == null )
      catalog = new ArrayList();
    m_url = new Combo( fieldGroup, SWT.BORDER );
    m_url.setItems( (String[]) catalog.toArray( new String[catalog.size()] ) );
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
    m_authentification.setToolTipText( "Einblenden der Eingabefelder für Benutzername und Passwort für den Zugriff auf geschützte Services" );
    m_authentification.setSelection( false );
    m_authentification.addSelectionListener( m_authSelectionListener );

    // usr
    m_labelUser = new Label( fieldGroup, SWT.NONE );
    m_labelUser.setText( "Benutzername:" );
    m_labelUser.setToolTipText( "Benutzername für den gewählten Server" );
    m_labelUser.setLayoutData( new GridData( SWT.END, SWT.DEFAULT, false, false ) );
    m_labelUser.setVisible( false );

    m_user = new Text( fieldGroup, SWT.BORDER );
    m_user.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    m_user.addSelectionListener( m_userSelectionListener );
    m_user.setVisible( false );

    // pass
    m_labelPass = new Label( fieldGroup, SWT.NONE );
    m_labelPass.setText( "Passwort:" );
    m_labelPass.setToolTipText( "Passwort für den gewählten Server" );
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
  }

  private void createLayerSelectionControl( Composite composite )
  {
    m_layerGroup = new Group( composite, SWT.CENTER );
    m_layerGroup.setText( "Verfügbare Themen des Web Feature Servers" );
    GridLayout gridLayout = new GridLayout( 4, false );
    gridLayout.marginHeight = 10;
    gridLayout.horizontalSpacing = 10;
    gridLayout.verticalSpacing = 10;
    m_layerGroup.setLayout( gridLayout );
    m_layerGroup.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );

    m_listLeftSide = new List( m_layerGroup, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL | SWT.H_SCROLL );
    m_listLeftSide.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    m_listLeftSide.addSelectionListener( m_leftSelectionListener );

    m_addLayer = new Button( m_layerGroup, SWT.PUSH );
    m_addLayer.setImage( ImageProvider.IMAGE_STYLEEDITOR_FORWARD.createImage() );
    m_addLayer.setToolTipText( "Hinzufügen eines Themas zur Kartenansicht" );
    m_addLayer.addSelectionListener( m_addButtonSelectionListener );
    m_addLayer.setEnabled( false );
    m_listRightSide = new List( m_layerGroup, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL );
    m_listRightSide.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    m_listRightSide.addSelectionListener( m_rightSelectionListener );
    m_leftsideButtonC = new Composite( m_layerGroup, SWT.NULL );
    m_leftsideButtonC.setLayout( new GridLayout() );
    m_leftsideButtonC.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    m_leftsideButtonC.setLayoutData( new GridData( 40, SWT.DEFAULT ) );
    m_removeLayer = new Button( m_leftsideButtonC, SWT.PUSH );
    m_removeLayer.setEnabled( false );
    m_removeLayer.setImage( ImageProvider.IMAGE_STYLEEDITOR_REMOVE.createImage() );
    m_removeLayer.setToolTipText( "Entfernen des gewählten Themas aus der Kartenansicht" );
    m_removeLayer.addSelectionListener( m_removeButtonSelectionListener );
    m_addFilterButton = new Button( m_leftsideButtonC, SWT.NULL );
    m_addFilterButton.setImage( ImageProvider.IMAGE_FILTERDIALOG_ADD_FILTER.createImage() );
    m_addFilterButton.setToolTipText( "Erstellen/hinzufügen eines Filters für ein WFS Querable Request" );
    m_addFilterButton.addSelectionListener( m_filterButtonSelectionListener );
    m_addFilterButton.setEnabled( false );
    m_layerGroup.setVisible( true );
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
  // .setToolTipText( "Groessere buffer sizes benötigen mehr Speicher, es erhöht
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
  // .setToolTipText( "Längeres timeouts unterstützt unzuverlässigere
  // Netzwerkverbindungen, führt unter umständen aber auch zu längeren render
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
    Button b;
    if( e.widget instanceof Button )
    {
      b = (Button) e.widget;
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

  private WFSCapabilities getCapabilites( ) throws MalformedURLException, IOException, Exception
  {

    final URL urlGetCap = new URL( getUrl().toString().trim() + "?" + "SERVICE=WFS&VERSION=1.0.0&REQUEST=GetCapabilities" );
    final URLConnection conGetCap = urlGetCap.openConnection();
    conGetCap.addRequestProperty( "SERVICE", "WFS" );
    conGetCap.addRequestProperty( "VERSION", "1.0.0" );
    conGetCap.addRequestProperty( "REQUEST", "GetCapabilities" );
    InputStream isGetCap = conGetCap.getInputStream();
    return WFSCapabilitiesFactory.createCapabilities( new InputStreamReader( isGetCap ) );
  }

  /**
   * This method returns the layer names in a sorted array
   * 
   * @return an array of strings sorted alphabetically
   */
  private String[] getLayerNamesFromCapabilities( )
  {
    if( m_wfsCapabilites != null )
    {
      TreeSet list = new TreeSet();
      org.deegree.services.wfs.capabilities.FeatureType[] featureTypes = m_wfsCapabilites.getFeatureTypeList().getFeatureTypes();
      for( int i = 0; i < featureTypes.length; i++ )
      {
        list.add( featureTypes[i].getName() );
      }
      return (String[]) list.toArray( new String[list.size()] );
    }
    return new String[0];
  }

  public String[] getSelectedFeatureNames( )
  {
    return m_listRightSide.getItems();
  }

  public IFeatureType[] getSelectedFeatureTypes( ) throws Exception
  {
    String[] selectedFeatureNames = getSelectedFeatureNames();
    return getFeatureTypes( selectedFeatureNames );
  }

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
  private IFeatureType[] getFeatureTypes( String[] layer )

  {
    HashSet res = new HashSet();
    GMLSchema featureTypeSchema = null;
    for( int i = 0; i < layer.length; i++ )
    {
      final String l = layer[i];
      if( !m_featureTypes.containsKey( l ) )
      {
        try
        {
          final URL url = new URL( getUrl().toString().trim() + "?SERVICE=WFS&VERSION=1.0.0&REQUEST=DescribeFeatureType&typeName=" + l );
          // featureTypeSchema = GMLSchemaCatalog.getSchema( url );
          // HACK
          featureTypeSchema = GMLSchemaCatalog.getSchema( "http://bsu.hamburg.de/huis" );
          final IFeatureType featureType = featureTypeSchema.getFeatureType( l );
          if( featureType != null )
            m_featureTypes.put( l, featureType );
          else if( l.indexOf( ":" ) >= 0 )
          {
            final String hackName = l.replaceAll( "^.+:", "" );
            m_featureTypes.put( l, featureTypeSchema.getFeatureType( hackName ) );
          }
        }
        catch( Exception e )
        {
          setMessage( e.getMessage() );
          setPageComplete( false );
        }
      }
      final IFeatureType featureType = (IFeatureType) m_featureTypes.get( l );
      res.add( featureType );
    }
    return (IFeatureType[]) res.toArray( new IFeatureType[res.size()] );
  }

  /**
   * @throws Exception,
   *           OperationNotSupportedException
   * @throws MalformedURLException
   */
  public String guessFeaturePath( String layer ) throws MalformedURLException, Exception, OperationNotSupportedException

  {
    // FIXME this method is not working properly, it has to be adjusted
    // when the Schema parser is adjusted!!!!!!
    final GMLSchema schema = GMLSchemaCatalog.getSchema( getSchemaURL( layer ) );
    final IFeatureType[] featureTypes = schema.getAllFeatureTypes();
    if( featureTypes.length == 1 )
      return featureTypes[0].getName();
    for( int i = 0; i < featureTypes.length; i++ )
    {
      IFeatureType ft = featureTypes[i];
      IPropertyType[] properties = ft.getProperties();
      for( int j = 0; j < properties.length; j++ )
      {
        IPropertyType property = properties[j];
        if( property instanceof IRelationType )
        {
          return property.getName();
        }

      }
    }
    throw new OperationNotSupportedException( "Guess of feature path failed!" );
  }

  public void removeListeners( )
  {
    m_addLayer.removeSelectionListener( m_addButtonSelectionListener );
    m_removeLayer.removeSelectionListener( m_removeButtonSelectionListener );
    m_listLeftSide.removeSelectionListener( m_leftSelectionListener );
    m_listRightSide.removeSelectionListener( m_rightSelectionListener );
  }

  public Filter getFilter( String layerName )
  {
    IFeatureType featureType = getFeatureType( layerName );
    return (Filter) m_filter.get( featureType );
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
      setErrorMessage( "Ungültige URL" );
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
    String[] layerNames = getLayerNamesFromCapabilities();
    m_listLeftSide.setItems( layerNames );
    updateButtons();
  }

  protected void addButtonPressed( )
  {
    String[] selection = m_listLeftSide.getSelection();
    if( selection != null )
    {
      final String[] original = m_listRightSide.getItems();
      for( int i = 0; i < selection.length; i++ )
      {
        final String item = selection[i];
        if( !ArrayUtils.contains( original, item ) )
          m_listRightSide.add( item );
      }
      updateButtons();
    }
  }

  protected void removeButtonPressed( )
  {
    String[] selection = m_listRightSide.getSelection();
    if( selection != null )
    {
      String[] list = m_listRightSide.getItems();
      for( int i = 0; i < selection.length; i++ )
      {
        String item = selection[i];
        if( ArrayUtils.contains( list, item ) )
          m_listRightSide.remove( item );

      }
      updateButtons();
    }
  }

  protected void filterPressed( )
  {
    // the add filter button is only enabled if the selection size == 1
    IFeatureType ft = getFeatureType( m_listRightSide.getSelection()[0] );
    Filter filter = (Filter) m_filter.get( ft );
    final FilterDialog dialog = new FilterDialog( getShell(), ft, filter, m_mapModel );
    int open = dialog.open();
    if( open == Window.OK )
    {
      m_filter.put( ft, dialog.getFilter() );
    }
    revalidatePage();
  }

  protected void revalidatePage( )
  {
    setPageComplete( m_listRightSide.getItemCount() > 0 );
  }

  private void updateButtons( )
  {
    m_addFilterButton.setEnabled( m_listRightSide.getSelectionCount() == 1 );
    m_removeLayer.setEnabled( m_listRightSide.getSelectionCount() > 0 );
    m_addLayer.setEnabled( m_listLeftSide.getSelectionCount() > 0 );
    revalidatePage();
  }

  private IFeatureType getFeatureType( String layerName )
  {
    IFeatureType[] featureTypes = getFeatureTypes( new String[] { layerName } );
    return featureTypes[0];
  }
}