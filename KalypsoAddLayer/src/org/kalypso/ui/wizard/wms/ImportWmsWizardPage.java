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
package org.kalypso.ui.wizard.wms;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.io.IOUtils;
import org.deegree.services.wms.capabilities.Layer;
import org.deegree.services.wms.capabilities.WMSCapabilities;
import org.deegree_impl.services.wms.capabilities.OGCWMSCapabilitiesFactory;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IContentProvider;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
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
import org.kalypso.ui.ImageProvider;

/**
 * @author Kuepferle, Doemming
 */
public class ImportWmsWizardPage extends WizardPage
{
  TreeViewer m_capabilitiesTree;

  ListViewer m_selectedLayers;

  private static final int MIN_LIST_WITH = 150;

  // * composite that has the caps tree and the selected layer list*/
  private Composite m_layerSelection;

  /** capabilites cache in this wizard */
  private HashMap<URL, WMSCapabilities> m_capabilites = new HashMap<URL, WMSCapabilities>();

  private static final String MSG_BASEURL_ERROR = "Die gewählte URL ist ungültig ";

  Button m_multiLayerButton;

  /** current selected base url */
  private URL m_baseURL = null;

  protected boolean m_urlModified = false;

  Combo m_urlCombo;

  /**
   *  
   */
  public ImportWmsWizardPage( String pageName )
  {
    this( pageName, "Web Map Service", null );
  }

  /**
   *  
   */
  public ImportWmsWizardPage( final String pageName, final String title, final ImageDescriptor titleImage )
  {
    super( pageName, title, titleImage );
    setPageComplete( false );
  }

  public void createControl( final Composite parent )
  {
    final Composite topControl = new Composite( parent, SWT.NULL );
    topControl.setLayout( new GridLayout( 1, false ) );
    createUrlField( topControl );

    createLayerSelection( topControl );

    m_layerSelection.setVisible( true );
    setControl( topControl );
  }

  private void createLayerSelection( final Composite composite )
  {
    final Group group = new Group( composite, SWT.CENTER );
    group.setText( "Verfügbare Themen des Web Map Servers" );
    final GridLayout gridLayout = new GridLayout();
    gridLayout.marginHeight = 10;
    gridLayout.horizontalSpacing = 10;
    gridLayout.verticalSpacing = 10;
    group.setLayout( gridLayout );
    gridLayout.makeColumnsEqualWidth = false;
    group.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    //
    m_layerSelection = new Composite( group, SWT.NULL );
    m_layerSelection.setLayout( new GridLayout( 4, false ) );

    m_layerSelection.setLayoutData( new GridData( GridData.HORIZONTAL_ALIGN_CENTER, GridData.VERTICAL_ALIGN_CENTER, true, true ) );

    // 1. column
    m_capabilitiesTree = new TreeViewer( m_layerSelection, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL | SWT.H_SCROLL );
    GridData data = new GridData( GridData.FILL_BOTH );
    data.grabExcessHorizontalSpace = true;
    data.grabExcessVerticalSpace = true;
    data.widthHint = MIN_LIST_WITH;
    m_capabilitiesTree.getControl().setLayoutData( data );
    final IContentProvider contentProvider = new WMSCapabilitiesContentProvider();
    final WMSCapabilitiesLabelProvider wmsCapabilitiesLabelProvider = new WMSCapabilitiesLabelProvider();
    m_capabilitiesTree.setContentProvider( contentProvider );
    m_capabilitiesTree.setLabelProvider( wmsCapabilitiesLabelProvider );

    // 2. column
    final Button buttonAddLayer = new Button( m_layerSelection, SWT.PUSH );
    buttonAddLayer.setImage( ImageProvider.IMAGE_STYLEEDITOR_FORWARD.createImage() );
    buttonAddLayer.setToolTipText( "Hinzufügen eines Themas zu der Kartenansicht" );
    buttonAddLayer.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
      {
        final IStructuredSelection selection = (IStructuredSelection) m_capabilitiesTree.getSelection();
        final List<Layer> input = getLayers();
        List<Layer> selectableLayer = new ArrayList<Layer>();
        for( Iterator iter = selection.iterator(); iter.hasNext(); )
        {
          final Layer layer = (Layer) iter.next();
          selectableLayer = getSelectableLayer( selectableLayer, layer );
        }
        input.addAll( selectableLayer );
        m_selectedLayers.setInput( input );
        m_multiLayerButton.setEnabled( input.size() > 1 );
        setPageComplete( !input.isEmpty() );
      }

      private List<Layer> getSelectableLayer( final List<Layer> resultCollector, final Layer layer )
      {
        List<Layer> resultList;
        if( resultCollector == null )
          resultList = new ArrayList<Layer>();
        else
          resultList = resultCollector;
        final Layer[] subLayers = layer.getLayer();
        if( subLayers.length > 0 )
        {
          for( int i = 0; i < subLayers.length; i++ )
            resultList = getSelectableLayer( resultList, subLayers[i] );
        }
        else
          resultList.add( layer );
        return resultList;
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        // nothing to do
      }
    } );

    // 3. column
    m_selectedLayers = new ListViewer( m_layerSelection, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL | SWT.H_SCROLL );
    final GridData data2 = new GridData( GridData.FILL_BOTH );
    data2.grabExcessHorizontalSpace = true;
    data2.grabExcessVerticalSpace = true;
    data2.widthHint = MIN_LIST_WITH;
    m_selectedLayers.getControl().setLayoutData( data2 );

    m_selectedLayers.setContentProvider( new IStructuredContentProvider()
    {
      private Object m_input = null;

      public Object[] getElements( Object inputElement )
      {
        if( m_input instanceof List )
        {
          return ((List) m_input).toArray();
        }
        return new Object[] { m_input };
      }

      public void dispose( )
      {
        // nothing to dispose
      }

      public void inputChanged( Viewer viewer, Object oldInput, Object newInput )
      {
        m_input = newInput;
      }
    } );
    m_selectedLayers.setLabelProvider( wmsCapabilitiesLabelProvider );
    m_selectedLayers.setInput( new ArrayList() );

    // 4. column

    // TODO add button up and down
    final Button m_removeLayer = new Button( m_layerSelection, SWT.PUSH );
    m_removeLayer.setImage( ImageProvider.IMAGE_STYLEEDITOR_REMOVE.createImage() );
    m_removeLayer.setToolTipText( "Entfernen eines Themas aus der Kartenansicht" );
    m_removeLayer.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
      {
        final IStructuredSelection selection2 = (IStructuredSelection) m_selectedLayers.getSelection();
        final List<Layer> input = getLayers();
        input.removeAll( selection2.toList() );
        m_selectedLayers.setInput( input );
        m_multiLayerButton.setEnabled( input.size() > 1 );
        setPageComplete( !input.isEmpty() );
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        // nothing to do
      }
    } );

    m_multiLayerButton = new Button( m_layerSelection, SWT.CHECK );
    m_multiLayerButton.setText( "Themen zusammenfassen" );
    m_multiLayerButton.setToolTipText( "Alle ausgewählte Themen als Gruppe in Karte einfügen" );
    m_multiLayerButton.setSelection( false );
    m_multiLayerButton.setEnabled( true );
    group.setVisible( false );
    group.setVisible( true );
    group.pack();
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  private void createUrlField( final Composite parent )
  {
    final Group fieldGroup = new Group( parent, SWT.NULL );
    fieldGroup.setText( "Verbindungsdaten" );
    fieldGroup.setLayout( new GridLayout( 2, false ) );
    fieldGroup.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );

    final Label urlLabel = new Label( fieldGroup, SWT.NONE );
    urlLabel.setText( "URL:" );
    urlLabel.setToolTipText( "URL des Web Map Servers (WMS)" );
    urlLabel.setLayoutData( new GridData( SWT.END, SWT.DEFAULT, false, false ) );
    final GridData gridData = new GridData( GridData.FILL_HORIZONTAL );
    gridData.widthHint = 400;

    // inizialize catalog
    // TODO use property file
    // TODO: better: store/retrieve list of url in dialog setting
    List<String> catalog = ((ImportWmsSourceWizard) getWizard()).getCatalog();
    if( catalog == null )
      catalog = new ArrayList<String>();
    // fill base-url-combo
    final String[] baseURLItems = catalog.toArray( new String[catalog.size()] );
    m_urlCombo = new Combo( fieldGroup, SWT.BORDER );
    m_urlCombo.setItems( baseURLItems );
    m_urlCombo.setVisibleItemCount( 15 );
    m_urlCombo.setLayoutData( gridData );

    m_urlCombo.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
      {
        // System.out.println( "selected" );
        final String urlText = m_urlCombo.getText();
        updateAllFromURLText( urlText );
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        // nothing
      }
    } );

    m_urlCombo.addFocusListener( new FocusListener()
    {
      public void focusGained( FocusEvent e )
      {
        // nothing
      }

      public void focusLost( FocusEvent e )
      {
        // System.out.println( "focusLost" );
        final String urlText = m_urlCombo.getText();
        updateAllFromURLText( urlText );
      }
    } );
    m_urlCombo.addModifyListener( new ModifyListener()
    {
      public void modifyText( ModifyEvent e )
      {
        m_urlModified = true;
      }
    } );
    m_urlCombo.addKeyListener( new KeyListener()
    {
      public void keyPressed( KeyEvent e )
      {
        // nothing
      }

      public void keyReleased( KeyEvent e )
      {
        if( e.character == SWT.CR )
        {
          final String urlText = m_urlCombo.getText();
          updateAllFromURLText( urlText );
        }
      }
    } );

    m_urlCombo.select( 0 );

    // // add spacer
    // final Label label = new Label( fieldGroup, SWT.SEPARATOR | SWT.HORIZONTAL );
    // label.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 3, 3 ) );

  }

  // /**
  // * TODO summary sentence for modifyText ...
  // *
  // * @see org.eclipse.swt.events.ModifyListener#modifyText(org.eclipse.swt.events.ModifyEvent)
  // * @param e
  // */
  // public void modifyText( ModifyEvent e )
  // {
  // if( e.widget != null && e.widget instanceof Text )
  // ( (Text)e.widget ).setForeground( null );
  // getContainer().updateButtons();
  // }

  private synchronized WMSCapabilities getCapabilites( final URL service ) throws Exception
  {
    if( m_capabilites.containsKey( service ) )
      return m_capabilites.get( service );

    // create quuery url
    final String query = service.getQuery();
    final String getCapabilitiesQuery = "SERVICE=WMS&REQUEST=GetCapabilities";
    final String queryToken = (query == null || query.length() == 0) ? "?" : "&";
    final String urlGetCapabilitiesString = service.toString() + queryToken + getCapabilitiesQuery;
    final URL urlGetCapabilities = new URL( urlGetCapabilitiesString );

    // TODO set timeout somewhere
    // maybe inside the createHttpClient Method of the Plugin-Class
    // get the timeout from global preferences
    final int timeOut = 20000;
    final InputStream inputStream = URLGetter.getFromURL( getContainer(), urlGetCapabilities, timeOut );
    final Reader urlReader = new InputStreamReader( inputStream );

    final String capabilitiesAsString = IOUtils.toString( urlReader );
    final StringReader reader = new StringReader( capabilitiesAsString );

    System.out.println( capabilitiesAsString );

    // TODO: handle exceptions and at least close streams! this is no error handling!
    final OGCWMSCapabilitiesFactory wmsCapFac = new OGCWMSCapabilitiesFactory();
    final WMSCapabilities capabilities = wmsCapFac.createCapabilities( reader );
    // store url and capabilites in a map so it is only loaded once
    m_capabilites.put( service, capabilities );

    return capabilities;
  }

  private URL validateURLField( final String url )
  {
    URL validUrl = null;
    String errorMsg = null;
    try
    {
      // checks catchment field entry and file suffix
      if( url.length() == 0 )
        errorMsg = "Das URL Feld darf nicht leer sein";
      else
      {
        validUrl = new URL( url );
        validUrl.openConnection();
      }
    }
    catch( Exception e )
    {
      errorMsg = e.getLocalizedMessage();
      validUrl = null;
    }
    finally
    {
      final boolean valid = validUrl != null;
      if( valid )
        setMessage( "BasisURL ist gültig" );
      else
        setErrorMessage( MSG_BASEURL_ERROR + errorMsg );
    }
    return validUrl;
  }

  public URL getBaseURL( )
  {
    return m_baseURL;
  }

  /**
   * @param urlText
   */
  protected void updateAllFromURLText( final String urlText )
  {
    if( !m_urlModified )
      return;
    m_urlModified = false;
    setErrorMessage( null );
    setMessage( "" );
    setPageComplete( false );
    boolean valid = true;
    if( m_capabilitiesTree == null )
      return;
    m_urlCombo.setEnabled( false );
    m_capabilitiesTree.setInput( null );
    m_selectedLayers.setInput( new ArrayList() );
    final URL baseURL = validateURLField( urlText );
    valid = baseURL != null;
    WMSCapabilities capabilites = null;
    if( valid )
    {
      try
      {
        capabilites = getCapabilites( baseURL );
      }
      catch( Exception e )
      {
        valid = false;
        e.printStackTrace();
        setErrorMessage( MSG_BASEURL_ERROR + ": " + e.getLocalizedMessage() + "\n(" + e.getClass().getName() + ")" );
      }
    }
    m_baseURL = baseURL;
    m_capabilitiesTree.setInput( capabilites );
    m_layerSelection.setVisible( valid );
    m_urlCombo.setEnabled( true );
  }

  /**
   * @return the selected layers
   */
  public Layer[] getLayersList( )
  {
    final List<Layer> list = getLayers();
    return list.toArray( new Layer[list.size()] );
  }

  /**
   * For casting the input to List<Layer> and suppress the warning
   */
  @SuppressWarnings("unchecked")
  protected List<Layer> getLayers( )
  {
    return (List<Layer>) m_selectedLayers.getInput();
  }

  /**
   * @return if multilayer if wanted
   */
  public boolean isMultiLayer( )
  {
    return m_multiLayerButton.getSelection();
  }
}