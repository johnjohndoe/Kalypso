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
package org.kalypso.ogc.gml.filterdialog.dialog;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.ArrayList;

import org.apache.tools.ant.filters.StringInputStream;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IPartService;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPartSite;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.SaveAsDialog;
import org.kalypso.contribs.eclipse.ui.dialogs.KalypsoResourceSelectionDialog;
import org.kalypso.contribs.eclipse.ui.dialogs.ResourceSelectionValidator;
import org.kalypso.contribs.java.xml.XMLHelper;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.gml.KalypsoFeatureThemeSelection;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.ogc.gml.filterdialog.model.FilterRootElement;
import org.kalypso.ogc.gml.filterdialog.widgets.AbstractFilterComposite;
import org.kalypso.ogc.gml.filterdialog.widgets.FilterCompositeFactory;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.editor.mapeditor.GisMapEditor;
import org.kalypso.ui.editor.styleeditor.MessageBundle;
import org.kalypsodeegree.filterencoding.Filter;
import org.kalypsodeegree.filterencoding.FilterConstructionException;
import org.kalypsodeegree.filterencoding.Operation;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.filterencoding.AbstractFilter;
import org.kalypsodeegree_impl.filterencoding.FeatureFilter;
import org.kalypsodeegree_impl.filterencoding.FeatureId;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * @author kuepferle
 */
public class FilterDialog extends TitleAreaDialog implements IErrorMessageReciever
{
  private static final String EMPTY_FEATURE_LIST = Messages.getString( "org.kalypso.ogc.gml.filterdialog.dialog.FilterDialog.empty" ); //$NON-NLS-1$

  private static final int ID_BUTTON_APPLY = 100;

  private static final String LABEL_BUTTON_APPLY = Messages.getString( "org.kalypso.ogc.gml.filterdialog.dialog.FilterDialog.apply" ); //$NON-NLS-1$

  public static final int APPLY_FILTER = 101;

  private static final String FILTER_KEY = "filter";

  protected final IFeatureType m_ft;

  protected FilterRootElement m_root;

  protected Group m_propGroup;

  protected Composite m_main;

  protected TreeViewer m_viewer;

  protected final FilterLabelProvider m_labelProvider = new FilterLabelProvider();

  protected final FilterContentProvider m_contentProvider = new FilterContentProvider();

  private ToolBar m_toolBar;

  Feature m_spatialOperator;

  AbstractFilterComposite m_newOpsComposite;

  private Composite m_top;

  final private KalypsoUserStyle m_userStyle;

  private final boolean RESTOREABLE;

  final String[] m_supportedOperations;

  /**
   * Der Benutzer kann mit Hilfe dieses Dialogs ein Filter-Query (OGC-Filter-Specs. Version 1.1.1) auf eine Feature
   * Selektion anwenden oder einen Filter für einen SLD (Styled-Layer-Discribtor) erzeugen.
   */
  public FilterDialog( final Shell parent, final IFeatureType ftToSelectFrom, final KalypsoUserStyle style, final Filter filter, final Feature spatialOperator, final String[] supportedOperations, final boolean restorable )
  {
    super( parent );
    m_userStyle = style;
    m_ft = ftToSelectFrom;
    m_supportedOperations = supportedOperations;
    m_spatialOperator = spatialOperator;
    RESTOREABLE = restorable;
    m_root = new FilterRootElement();
    if( filter != null )
      m_root.addChild( filter );
    setShellStyle( getShellStyle() | SWT.MAX );

  }

  /**
   * @see org.eclipse.jface.dialogs.TitleAreaDialog#createContents(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createContents( final Composite parent )
  {
    final Control control = super.createContents( parent );
    createButton( (Composite) getButtonBar(), FilterDialog.ID_BUTTON_APPLY, FilterDialog.LABEL_BUTTON_APPLY, true );
    if( m_userStyle == null )
      getButton( ID_BUTTON_APPLY ).setEnabled( false );
    return control;
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
   */
  @Override
  protected void buttonPressed( final int buttonId )
  {
    super.buttonPressed( buttonId );
    try
    {
      if( buttonId == ID_BUTTON_APPLY )
      {
        m_userStyle.fireStyleChanged();
        setReturnCode( APPLY_FILTER );
      }

    }
    catch( final Exception e )
    {
      MessageDialog.openError( getShell(), Messages.getString( "org.kalypso.ogc.gml.filterdialog.dialog.FilterDialog.error" ), Messages.getString( "org.kalypso.ogc.gml.filterdialog.dialog.FilterDialog.error2" ) ); //$NON-NLS-1$ //$NON-NLS-2$
      e.printStackTrace();
    }

  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createDialogArea( final Composite parent )
  {
    // load old status from the local storage
    if( RESTOREABLE && m_root.getFilter() == null )
    {
      final IDialogSettings dialogSettings = KalypsoGisPlugin.getDefault().getDialogSettings();
      final String filterXML = dialogSettings.get( FILTER_KEY );
      if( filterXML != null )
      {
        final StringInputStream input = new StringInputStream( filterXML );
        try
        {
          final Filter filter = readFilterFragment( input );
          m_root.addChild( filter );
        }
        catch( final Exception e1 )
        {
          e1.printStackTrace();
        }
      }
    }
    m_main = (Composite) super.createDialogArea( parent );
    m_top = new Composite( m_main, SWT.NONE );
    final GridLayout gridLayout = new GridLayout( 2, true );
    m_top.setLayout( gridLayout );
    final GridData data2 = new GridData( GridData.FILL_BOTH );
    data2.grabExcessHorizontalSpace = true;
    data2.grabExcessVerticalSpace = true;
    m_top.setLayoutData( data2 );

    /* tree-group */
    final Group treeGroup = new Group( m_top, SWT.FILL );
    treeGroup.setText( Messages.getString( "org.kalypso.ogc.gml.filterdialog.dialog.FilterDialog.filterfor" ) );// +
    // AnnotationUtilities.getAnnotation(
    // m_featureType
    // ).getLabel()
    // );
    // //$NON-NLS-1$
    final GridData data3 = new GridData( GridData.FILL_BOTH );
    data3.grabExcessHorizontalSpace = true;
    data3.grabExcessVerticalSpace = true;
    treeGroup.setLayoutData( data3 );
    treeGroup.setLayout( new GridLayout() );

    /* filter tree-viewer */
    m_viewer = new TreeViewer( treeGroup, SWT.H_SCROLL | SWT.V_SCROLL )
    {
      @Override
      public ISelection getSelection( )
      {
        return new TreeSelection( (IStructuredSelection) super.getSelection() )
        {
          public void contentChanged( )
          {
            refresh( true );
            collapseAll();
            expandAll();
          }

          public void structureChanged( )
          {
            refresh( true );
            collapseAll();
            expandAll();
          }

          @Override
          public Object getModel( )
          {
            return m_root;
          }
        };
      }
    };
    final GridData data1 = new GridData( GridData.FILL_BOTH );
    data1.grabExcessHorizontalSpace = true;
    data1.grabExcessVerticalSpace = true;
    data1.heightHint = 200;
    data1.widthHint = 150;
    m_viewer.getControl().setLayoutData( data1 );
    m_viewer.setLabelProvider( m_labelProvider );
    m_viewer.setContentProvider( m_contentProvider );
    m_viewer.addSelectionChangedListener( new ISelectionChangedListener()
    {

      public void selectionChanged( final SelectionChangedEvent event )
      {
        final ISelection selection = event.getSelection();
        if( selection instanceof IStructuredSelection )
        {
          final Object firstElement = ((IStructuredSelection) selection).getFirstElement();

          if( firstElement instanceof Operation )
          {

            if( m_newOpsComposite != null )
            {
              if( !m_newOpsComposite.isDisposed() )
                m_newOpsComposite.dispose();
            }
            m_newOpsComposite = FilterCompositeFactory.createFilterElementComposite( m_propGroup, FilterDialog.this, (Operation) firstElement, m_supportedOperations, m_ft, m_spatialOperator );
            if( m_newOpsComposite != null )
            {
              m_newOpsComposite.setFilterDialog( FilterDialog.this );
              m_newOpsComposite.pack();
              // m_propGroup.pack();
            }
          }
          else if( firstElement instanceof FeatureFilter )
          {
            final FeatureFilter filter = (FeatureFilter) firstElement;
            m_propGroup.setText( Messages.getString( "org.kalypso.ogc.gml.filterdialog.dialog.FilterDialog.featfilter" ) ); //$NON-NLS-1$
            final Composite ffComposite = new Composite( m_propGroup, SWT.NONE );
            final GridLayout ffGridLayout = new GridLayout();
            ffComposite.setLayout( ffGridLayout );
            final GridData ffGridData = new GridData();
            ffGridData.horizontalIndent = 10;
            ffGridData.verticalIndent = 10;
            ffGridData.widthHint = 150;
            ffComposite.setLayoutData( ffGridData );
            final ArrayList<FeatureId> featureIds = filter.getFeatureIds();
            final Label featureFilterLabel = new Label( ffComposite, SWT.NULL );
            featureFilterLabel.setText( Messages.getString( "org.kalypso.ogc.gml.filterdialog.dialog.FilterDialog.featids" ) ); //$NON-NLS-1$
            final List idList = new List( ffComposite, SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL );
            final GridData gridData = new GridData();
            gridData.widthHint = 150;
            idList.setLayoutData( gridData );
            final String[] items = featureIds.toArray( new String[featureIds.size()] );
            if( items.length > 0 )
              idList.setItems( items );
            else
              idList.add( EMPTY_FEATURE_LIST );
            final Button importSelectedFeatures = new Button( ffComposite, SWT.CHECK );
            importSelectedFeatures.setText( Messages.getString( "org.kalypso.ogc.gml.filterdialog.dialog.FilterDialog.selection" ) ); //$NON-NLS-1$
            importSelectedFeatures.setToolTipText( Messages.getString( "org.kalypso.ogc.gml.filterdialog.dialog.FilterDialog.selection2" ) ); //$NON-NLS-1$
            importSelectedFeatures.addSelectionListener( new SelectionAdapter()
            {

              /**
               * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
               */
              @Override
              public void widgetSelected( final SelectionEvent e )
              {
                final Button source = (Button) e.getSource();
                if( source.getSelection() )
                {
                  final IWorkbench workbench = PlatformUI.getWorkbench();
                  final IWorkbenchWindow activeWorkbenchWindow = workbench.getActiveWorkbenchWindow();
                  final IWorkbenchPage activePage = activeWorkbenchWindow.getActivePage();
                  final IEditorPart activeEditor = activePage.getActiveEditor();
                  if( activeEditor instanceof GisMapEditor )
                  {
                    final MapPanel mapPanel = ((GisMapEditor) activeEditor).getMapPanel();
                    final IStructuredSelection s = (IStructuredSelection) mapPanel.getSelection();
                    if( s instanceof KalypsoFeatureThemeSelection )
                    {
                      final KalypsoFeatureThemeSelection fts = ((KalypsoFeatureThemeSelection) s);
                      final Object[] elements = fts.toArray();
                      final String[] features = new String[elements.length];
                      for( int i = 0; i < elements.length; i++ )
                      {
                        final Feature f = (Feature) elements[i];
                        if( f != null )
                          features[i] = f.getId();
                      }
                      idList.setItems( features );
                    }

                  }
                }
                else
                {
                  idList.setItems( new String[] { EMPTY_FEATURE_LIST } );
                }
                ffComposite.pack();
              }
            } );
            ffComposite.pack();
          }
        }
      }
    } );
    m_viewer.setInput( new Object[] { m_root } );
    m_viewer.expandAll();
    // property group
    m_propGroup = new Group( m_top, SWT.FILL );
    m_propGroup.setText( Messages.getString( "org.kalypso.ogc.gml.filterdialog.dialog.FilterDialog.properties" ) ); //$NON-NLS-1$
    m_propGroup.setLayout( new GridLayout( 2, true ) );
    final GridData data = new GridData( GridData.FILL_BOTH );
    data.grabExcessHorizontalSpace = true;
    data.grabExcessVerticalSpace = true;
    data.horizontalIndent = 10;
    data.verticalIndent = 10;
    m_propGroup.setLayoutData( data );

    // toolbar
    m_toolBar = new ToolBar( m_top, SWT.NULL | SWT.FLAT );
    final ToolItem m_loadFilterItem = new ToolItem( m_toolBar, SWT.NONE );
    m_loadFilterItem.setToolTipText( Messages.getString( "org.kalypso.ogc.gml.filterdialog.dialog.FilterDialog.load" ) ); //$NON-NLS-1$
    m_loadFilterItem.setImage( ImageProvider.IMAGE_UTIL_IMPORT_WIZARD.createImage() );
    m_loadFilterItem.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( final SelectionEvent e )
      {
        final IWorkspace workspace = ResourcesPlugin.getWorkspace();
        final KalypsoResourceSelectionDialog dialog2 = new KalypsoResourceSelectionDialog( getShell(), workspace.getRoot(), Messages.getString( "org.kalypso.ogc.gml.filterdialog.dialog.FilterDialog.choose" ), new String[] { "xml" }, workspace.getRoot(), new ResourceSelectionValidator() ); //$NON-NLS-1$ //$NON-NLS-2$
        final int open = dialog2.open();
        if( open == Window.OK )
        {
          final Object[] result = dialog2.getResult();
          final IPath path = (IPath) result[0];
          Filter filter = null;
          try
          {
            final IFile file = workspace.getRoot().getFile( path );
            filter = readFilterFragment( file.getContents() );
          }
          catch( final Exception e1 )
          {
            e1.printStackTrace();
          }
          m_root.addChild( filter );
          m_viewer.setContentProvider( m_contentProvider );
          m_viewer.setLabelProvider( m_labelProvider );
          m_viewer.setInput( new Object[] { m_root } );
          m_viewer.expandAll();
        }
      }

      public void widgetDefaultSelected( final SelectionEvent e )
      {
        widgetSelected( e );
      }
    } );
    final ToolItem m_saveFilterItem = new ToolItem( m_toolBar, SWT.NONE );
    m_saveFilterItem.setToolTipText( Messages.getString( "org.kalypso.ogc.gml.filterdialog.dialog.FilterDialog.save" ) ); //$NON-NLS-1$
    m_saveFilterItem.setImage( ImageProvider.IMAGE_STYLEEDITOR_SAVE.createImage() );
    m_saveFilterItem.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( final SelectionEvent e )
      {
        final IWorkspace workspace = ResourcesPlugin.getWorkspace();
        final SaveAsDialog dialog = new SaveAsDialog( getShell() );
        final int open = dialog.open();
        if( open == Window.OK )
        {
          final IPath result = dialog.getResult();
          final IFile file = workspace.getRoot().getFile( result );
          final String xml = m_root.toXML().toString();
          try
          {
            file.create( new ByteArrayInputStream( xml.getBytes() ), true, new NullProgressMonitor() );
          }
          catch( final CoreException e1 )
          {
            e1.printStackTrace();
          }
        }
      }

      public void widgetDefaultSelected( final SelectionEvent e )
      {
        widgetSelected( e );
      }
    } );
    createContextMenu();
    return m_main;
  }

  private void createContextMenu( )
  {
    final MenuManager menuManager = new MenuManager();
    menuManager.setRemoveAllWhenShown( true );
    menuManager.addMenuListener( new IMenuListener()
    {
      public void menuAboutToShow( final IMenuManager manager )
      {
        final IStructuredSelection selection = (IStructuredSelection) m_viewer.getSelection();
        if( selection.getFirstElement() instanceof FilterRootElement )
        {
          manager.add( new Separator( IWorkbenchActionConstants.MB_ADDITIONS ) );
          manager.add( new Separator() );
        }
        else
        {
          manager.add( m_action );
          manager.add( new Separator( IWorkbenchActionConstants.MB_ADDITIONS ) );
          manager.add( new Separator() );
        }
      }
    } );

    // Register the context menu at the active workbench part
    final IPartService workbenchPart = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getPartService();
    final IWorkbenchPartSite site = workbenchPart.getActivePart().getSite();
    if( site != null )
    {
      final Menu menu = menuManager.createContextMenu( m_viewer.getControl() );
      site.registerContextMenu( menuManager, m_viewer );
      m_viewer.getControl().setMenu( menu );
    }
  }

  @Override
  protected void okPressed( )
  {
    if( RESTOREABLE )
    {
      final IDialogSettings dialogSettings = KalypsoGisPlugin.getDefault().getDialogSettings();
      final Filter filter = getFilter();
      String xml = ""; //$NON-NLS-1$
      if( filter != null )
      {
        xml = filter.toXML().toString();
      }
      dialogSettings.put( FILTER_KEY, xml );
    }
    super.okPressed();
  }

  @Override
  protected void cancelPressed( )
  {
    super.cancelPressed();
  }

  @Override
  protected void configureShell( final Shell shell )
  {
    super.configureShell( shell );
    shell.setText( MessageBundle.STYLE_EDITOR_FILTER );
    shell.setSize( 650, 350 );
  }

  protected Action m_action = new Action( Messages.getString( "org.kalypso.ogc.gml.filterdialog.dialog.FilterDialog.del" ), ImageProvider.IMAGE_STYLEEDITOR_REMOVE ) //$NON-NLS-1$
  {
    @Override
    public void run( )
    {
      IStructuredSelection selection = (IStructuredSelection) m_viewer.getSelection();
      if( !selection.isEmpty() )
      {

        m_root.removeChild( selection.getFirstElement() );
        m_viewer.refresh( true );
        setErrorMessage( null );
        if( m_newOpsComposite != null )
          m_newOpsComposite.dispose();
        m_propGroup.setText( Messages.getString( "org.kalypso.ogc.gml.filterdialog.dialog.FilterDialog.unknown" ) ); //$NON-NLS-1$
      }
    }
  };

  public Filter getFilter( )
  {
    return m_root.getFilter();
  }

  /**
   * @see org.kalypso.ogc.gml.filterdialog.dialog.IErrorMessageReciever#setErrorMessage(java.lang.String)
   */
  @Override
  public void setErrorMessage( final String message )
  {
    super.setErrorMessage( message );

  }

  /**
   * @see org.kalypso.ogc.gml.filterdialog.dialog.IErrorMessageReciever#getErrorMessageReciever()
   */
  public IErrorMessageReciever getErrorMessageReciever( )
  {
    return this;
  }

  Filter readFilterFragment( final InputStream reader )
  {
    Filter filter = null;
    try
    {
      final Document asDOM = XMLHelper.getAsDOM( reader, true );
      final Element element = asDOM.getDocumentElement();

      filter = AbstractFilter.buildFromDOM( element );

    }
    catch( final FilterConstructionException e )
    {
      e.printStackTrace();
      MessageDialog.openWarning( getShell(), Messages.getString( "org.kalypso.ogc.gml.filterdialog.dialog.FilterDialog.dialog" ), Messages.getString( "org.kalypso.ogc.gml.filterdialog.dialog.FilterDialog.errorload" ) + e.getMessage() ); //$NON-NLS-1$ //$NON-NLS-2$
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      MessageDialog.openWarning( getShell(), Messages.getString( "org.kalypso.ogc.gml.filterdialog.dialog.FilterDialog.dialog" ), Messages.getString( "org.kalypso.ogc.gml.filterdialog.dialog.FilterDialog.errorload" ) + e.getMessage() ); //$NON-NLS-1$ //$NON-NLS-2$
    }
    return filter;
  }

  public boolean isRestorable( )
  {
    return RESTOREABLE;
  }

  public void refresh( )
  {
    m_viewer.refresh();
  }
}
