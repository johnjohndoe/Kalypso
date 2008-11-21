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

import java.lang.reflect.InvocationTargetException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.apache.commons.lang.ObjectUtils;
import org.deegree.datatypes.QualifiedName;
import org.deegree.ogcwebservices.wfs.capabilities.WFSFeatureType;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
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
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.gmlschema.GMLSchemaCatalog;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.KalypsoGMLSchemaPlugin;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.ogc.gml.filterdialog.dialog.FilterDialog;
import org.kalypso.ogc.wfs.IWFSLayer;
import org.kalypso.ogc.wfs.WFSClient;
import org.kalypso.ui.ImageProvider;
import org.kalypso.util.swt.StatusComposite;
import org.kalypsodeegree.filterencoding.Filter;

/**
 * @author Kuepferle
 */
public class ImportWfsWizardPage extends WizardPage
{
  private static final IStatus LOADING_STATUS = StatusUtilities.createStatus( IStatus.INFO, "Lade Capabilities...", null );

  protected Button getDefault;

  protected Button postDefault;

  protected Text bufferText;

  protected Text timeoutText;

  protected static final String timeoutDefault = "3";

  protected static final String bufferDefault = "10";

  private ListViewer m_listLeftSide;

  private Button m_addLayer;

  private Button m_removeLayer;

  ListViewer m_listRightSide;

  private Group m_layerGroup;

  private Label m_labelUrl;

  private WFSClient m_wfsClient = null;

  private Button m_addFilterButton;

  private final HashMap<WFSFeatureType, Filter> m_filter = new HashMap<WFSFeatureType, Filter>();

  private String m_uri = null;

  private final ISelectionChangedListener m_leftSelectionListener = new ISelectionChangedListener()
  {

    public void selectionChanged( final SelectionChangedEvent event )
    {
      updateButtons();
    }
  };

  private final SelectionListener m_addButtonSelectionListener = new SelectionAdapter()
  {
    @Override
    public void widgetSelected( final SelectionEvent e )
    {
      addButtonPressed();
    }
  };

  private final SelectionListener m_removeButtonSelectionListener = new SelectionAdapter()
  {
    @Override
    public void widgetSelected( final SelectionEvent e )
    {
      removeButtonPressed();
    }
  };

  private final SelectionListener m_filterButtonSelectionListener = new SelectionAdapter()
  {
    @Override
    public void widgetSelected( final SelectionEvent e )
    {
      filterPressed();
    }
  };

  private final ISelectionChangedListener m_rightSelectionListener = new ISelectionChangedListener()
  {
    public void selectionChanged( final SelectionChangedEvent event )
    {
      updateButtons();

    }
  };

  final static ILabelProvider FEATURE_TYPE_LABEL_PROVIDER = new LabelProvider()
  {
    @Override
    public String getText( final Object element )
    {
      if( element instanceof WFSFeatureType )
      {
        final WFSFeatureType featureType = (WFSFeatureType) element;
        final String title = featureType.getTitle();
        if( title == null || title.isEmpty() )
          return featureType.getName().getLocalName();

        return title;
      }

      throw new IllegalArgumentException();
    }
  };

  private Composite m_leftsideButtonC;

  private final Map<URL, WFSClient> m_capabilites = new HashMap<URL, WFSClient>();

  final IDoubleClickListener m_doubleClickListener = new IDoubleClickListener()
  {
    public void doubleClick( final DoubleClickEvent event )
    {
      addButtonPressed();
    }
  };

  private StatusComposite m_statusComposite;

  public ImportWfsWizardPage( final String pageName )
  {
    this( pageName, null, null );
  }

  public ImportWfsWizardPage( final String pageName, final String title, final ImageDescriptor titleImage )
  {
    super( pageName, title, titleImage );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    final Group composite = new Group( parent, SWT.NULL );
    composite.setLayout( new GridLayout( 1, false ) );

    m_statusComposite = new StatusComposite( composite, StatusComposite.DETAILS );
    m_statusComposite.setLayoutData( new GridData( SWT.FILL, SWT.BEGINNING, true, false ) );

    createSourceFields( composite );
    createLayerSelectionControl( composite );

    setControl( composite );

    setMessage( "Auf dieser Seite können Sie die Basisadresse (URL) des WebFeatureService eingeben und die anzuzeigenden Themen auswählen." );
    setPageComplete( false );
    setStatus( null );
  }

  private void createSourceFields( final Composite parent )
  {
    final Group fieldGroup = new Group( parent, SWT.NULL );
    fieldGroup.setLayout( new GridLayout( 2, false ) );
    fieldGroup.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    fieldGroup.setText( "Verbindungsdaten" );

    // add url
    m_labelUrl = new Label( fieldGroup, SWT.NONE );
    m_labelUrl.setText( "URL:" );
    m_labelUrl.setToolTipText( "URL des Web Feature Servers (WFS)" );
    m_labelUrl.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, false, false ) );

    // initialize availabel Servers
    ArrayList<String> catalog = ((ImportWfsSourceWizard) getWizard()).getCatalog();
    if( catalog == null )
      catalog = new ArrayList<String>();

    final ComboViewer comboViewer = new ComboViewer( fieldGroup, SWT.BORDER );
    comboViewer.setContentProvider( new ArrayContentProvider() );
    comboViewer.setLabelProvider( new LabelProvider() );
    comboViewer.setInput( catalog );

    final Combo combo = comboViewer.getCombo();

    combo.setVisibleItemCount( 15 );

    final GridData gridData = new GridData( SWT.FILL, SWT.CENTER, true, false );
    gridData.widthHint = 400;
    combo.setLayoutData( gridData );
    combo.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( final SelectionEvent e )
      {
        handleUrlComboChanged( combo.getText() );
      }

      public void widgetDefaultSelected( final SelectionEvent e )
      {
        handleUrlComboChanged( combo.getText() );
      }
    } );

    combo.addModifyListener( new ModifyListener()
    {
      public void modifyText( final ModifyEvent e )
      {
        revalidatePage();
      }
    } );

    combo.addFocusListener( new FocusAdapter()
    {
      /**
       * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
       */
      @Override
      public void focusLost( final FocusEvent e )
      {
        handleUrlComboChanged( combo.getText() );
      }
    } );
  }

  private void createLayerSelectionControl( final Composite composite )
  {
    m_layerGroup = new Group( composite, SWT.CENTER );
    m_layerGroup.setText( "Verfügbare Themen des Web Feature Servers" );
    final GridLayout gridLayout = new GridLayout( 4, false );
    gridLayout.marginHeight = 10;
    gridLayout.horizontalSpacing = 10;
    gridLayout.verticalSpacing = 10;
    m_layerGroup.setLayout( gridLayout );
    m_layerGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    m_listLeftSide = new ListViewer( m_layerGroup, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL | SWT.H_SCROLL );

    m_listLeftSide.getControl().setLayoutData( new GridData( GridData.FILL_BOTH ) );
    m_listLeftSide.setLabelProvider( FEATURE_TYPE_LABEL_PROVIDER );
    m_listLeftSide.setContentProvider( new ArrayContentProvider() );

    m_listLeftSide.addSelectionChangedListener( m_leftSelectionListener );
    m_listLeftSide.addDoubleClickListener( m_doubleClickListener );

    m_addLayer = new Button( m_layerGroup, SWT.PUSH );
    m_addLayer.setImage( ImageProvider.IMAGE_STYLEEDITOR_FORWARD.createImage() );
    m_addLayer.setToolTipText( "Hinzufügen eines Themas zur Kartenansicht" );
    m_addLayer.addSelectionListener( m_addButtonSelectionListener );
    m_addLayer.setEnabled( false );
    m_listRightSide = new ListViewer( m_layerGroup, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL );
    m_listRightSide.getControl().setLayoutData( new GridData( GridData.FILL_BOTH ) );
    m_listRightSide.setLabelProvider( FEATURE_TYPE_LABEL_PROVIDER );
    m_listRightSide.setContentProvider( new ArrayContentProvider() );

    m_listRightSide.addSelectionChangedListener( m_rightSelectionListener );
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
    m_layerGroup.pack();
  }

  public Filter getFilter( final WFSFeatureType wfsLayer )
  {
    return m_filter.get( wfsLayer );
  }

  protected void reloadServer( )
  {
    setErrorMessage( null );
    setPageComplete( false );
    m_wfsClient = null;
    m_listLeftSide.setInput( new Object[0] );
    m_listRightSide.setInput( new ArrayList<IWFSLayer>() );
    setStatus( LOADING_STATUS );

    if( m_uri == null || m_uri.trim().isEmpty() )
    {
      setStatus( null );
      return;
    }

    try
    {
      final URL service = new URL( m_uri.trim() );
      if( m_capabilites.containsKey( service ) )
      {
        m_wfsClient = m_capabilites.get( service );
        return;
      }

      final CapabilitiesGetter runnable = new CapabilitiesGetter( service );
      final IStatus status = RunnableContextHelper.execute( getContainer(), false, false, runnable );
      setStatus( status );
      if( !status.isOK() )
        setErrorMessage( status.getMessage() );

      final WFSClient capabilities = runnable.getCapabilities();
      m_capabilites.put( service, capabilities );
      m_wfsClient = capabilities;
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();
      final IStatus status = StatusUtilities.createStatus( IStatus.ERROR, "Ungültige URL: " + m_uri, e );
      setErrorMessage( status.getMessage() );
      setStatus( status );
    }

    if( m_wfsClient != null )
      m_listLeftSide.setInput( m_wfsClient.getFeatureTypes() );

    updateButtons();
  }

  private void setStatus( final IStatus status )
  {
    m_statusComposite.setStatus( status );

    final boolean visible = status != null && !status.isOK();

    ((GridData) m_statusComposite.getLayoutData()).exclude = !visible;
    m_statusComposite.setVisible( visible );
    m_statusComposite.getParent().layout();
  }

  protected void addButtonPressed( )
  {
    final IStructuredSelection selection = (IStructuredSelection) m_listLeftSide.getSelection();
    if( selection != null )
    {
      final Object[] original = selection.toArray();
      for( final Object element : original )
      {
        final WFSFeatureType wfsFT = (WFSFeatureType) element;
        final List<WFSFeatureType> input = getLayerList();
        if( !input.contains( wfsFT ) )
        {
          input.add( wfsFT );
          m_listRightSide.add( wfsFT );
        }
      }

      m_listRightSide.setSelection( new StructuredSelection( original ) );

      updateButtons();
    }
  }

  /** Just for casting the list and suppress the warning */
  @SuppressWarnings("unchecked")
  List<WFSFeatureType> getLayerList( )
  {
    return (List<WFSFeatureType>) m_listRightSide.getInput();
  }

  protected void removeButtonPressed( )
  {
    final IStructuredSelection selection = (IStructuredSelection) m_listRightSide.getSelection();
    if( selection != null )
    {
      final Object[] list = selection.toArray();
      for( final Object element : list )
      {
        final List<WFSFeatureType> input = getLayerList();
        input.remove( element );
        m_listRightSide.remove( element );
      }
      updateButtons();
    }
  }

  protected void filterPressed( )
  {
    try
    {
      // the add filter button is only enabled if the selection size == 1, hence there is only one selected element
      final IStructuredSelection selection = (IStructuredSelection) m_listRightSide.getSelection();
      final WFSFeatureType wfsFT = (WFSFeatureType) selection.getFirstElement();
      final Filter oldFilter = m_filter.get( wfsFT );

      final QualifiedName name = wfsFT.getName();
      final GMLSchemaCatalog catalog = KalypsoGMLSchemaPlugin.getDefault().getSchemaCatalog();
      final String namespace = name.getNamespace().toString();
      final IGMLSchema schema = catalog.getSchema( namespace, "3.1.1" );
      final IFeatureType ft = schema.getFeatureType( new QName( namespace, name.getLocalName() ) );

      final String[] supportedOperations = m_wfsClient.getAllFilterCapabilitesOperations();
      final FilterDialog dialog = new FilterDialog( getShell(), ft, null, oldFilter, null, supportedOperations, false );
      final int open = dialog.open();
      if( open == Window.OK )
        m_filter.put( wfsFT, dialog.getFilter() );
      revalidatePage();
    }
    catch( final InvocationTargetException e )
    {
      e.getTargetException().printStackTrace();
      setErrorMessage( e.getTargetException().toString() );
    }
  }

  protected void revalidatePage( )
  {
    final List<WFSFeatureType> input = getLayerList();
    setPageComplete( input != null && !input.isEmpty() );
  }

  void updateButtons( )
  {
    final IStructuredSelection selection = (IStructuredSelection) m_listRightSide.getSelection();
    m_addFilterButton.setEnabled( selection.size() == 1 );
    m_removeLayer.setEnabled( selection.size() > 0 );
    m_addLayer.setEnabled( ((IStructuredSelection) m_listLeftSide.getSelection()).size() > 0 );
    revalidatePage();
  }

  public WFSFeatureType[] getChoosenFeatureLayer( )
  {
    final List<WFSFeatureType> input = getLayerList();
    if( input == null )
      return new WFSFeatureType[0];
    return input.toArray( new WFSFeatureType[input.size()] );
  }

  private final class CapabilitiesGetter implements ICoreRunnableWithProgress
  {
    private final URL m_service;

    private WFSClient m_wfs = null;

    protected CapabilitiesGetter( final URL service )
    {
      m_service = service;
    }

    public IStatus execute( final IProgressMonitor monitor )
    {
      m_wfs = new WFSClient( m_service );
      return m_wfs.load();
    }

    public WFSClient getCapabilities( )
    {
      return m_wfs;
    }
  }

  protected void handleUrlComboChanged( final String text )
  {
    if( ObjectUtils.equals( m_uri, text ) )
      return;

    m_uri = text;
    reloadServer();
  }

  public String getUri( )
  {
    return m_uri;
  }
}