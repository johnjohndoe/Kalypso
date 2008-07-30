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
package org.kalypso.ui.dialog;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;

import javax.xml.namespace.QName;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.BusyIndicator;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.ui.dialogs.KalypsoResourceSelectionDialog;
import org.kalypso.contribs.eclipse.ui.dialogs.ResourceSelectionValidator;
import org.kalypso.contribs.java.net.IUrlResolver2;
import org.kalypso.contribs.java.net.UrlResolver;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.transformation.CRSHelper;
import org.kalypso.ui.editor.gmleditor.ui.FeatureAssociationTypeElement;
import org.kalypso.ui.editor.gmleditor.ui.GMLContentProvider;
import org.kalypso.ui.editor.gmleditor.ui.GMLLabelProvider;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.graphics.sld.Layer;
import org.kalypsodeegree.graphics.sld.Style;
import org.kalypsodeegree.graphics.sld.StyledLayerDescriptor;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.xml.XMLParsingException;
import org.kalypsodeegree_impl.gml.binding.commons.NamedFeatureHelper;
import org.kalypsodeegree_impl.graphics.sld.SLDFactory;
import org.kalypsodeegree_impl.model.feature.FeaturePath;

/**
 * @author kuepfer
 */
public class GmlShapeFileImportDialog extends Dialog
{

  private static final int SIZING_TEXT_FIELD_WIDTH = 250;

  private Group m_group;

  private Label m_sourceFileLabel;

  Text m_sourceFileText;

  private Button m_browseButton;

  private Label m_styleLabel;

  private Text m_styleTextField;

  private Button m_browseButton2;

  private Label m_styleNameLabel;

  private Combo m_styleNameCombo;

  private Button m_checkDefaultStyleButton;

  private IPath m_relativeSourcePath;

  private IPath m_stylePath;

  private final IWorkspaceRoot m_eclipseWorkspace = ResourcesPlugin.getWorkspace().getRoot();

  GMLWorkspace m_workspace;

  private final boolean m_withStyleSelection;

  private final boolean m_fromLocalFileSys;

  private final String[] m_fileExtensions;

  private final static String SHAPE_FILE_EXT = "shp"; //$NON-NLS-1$

  private final static String GML_FILE_EXT = "gml"; //$NON-NLS-1$

  private final boolean m_loadData;

  final boolean m_selectFeature;

  Object m_selectedObject = null;

  private Composite m_viewerComposite;

  TreeViewer m_treeViewer;

  private ViewerFilter m_filter;

  final Class< ? extends Object>[] m_selectableClasses;

  final IContainer m_root;

  private Combo m_checkCRS;

  private Group m_crsGroup;

  private Group m_styleGroup;

  private QName[] m_selectableQNames;

  public GmlShapeFileImportDialog( final Shell parentShell, final boolean withStyleSelection, final boolean chooseFromLocalFileSystem, final boolean loadData, final boolean selectFeature, final String[] extensions, final IContainer root, final Class< ? extends Object>[] selectableClasses )
  {
    super( parentShell );

    m_withStyleSelection = withStyleSelection;
    m_fromLocalFileSys = chooseFromLocalFileSystem;
    if( extensions != null && extensions.length > 0 )
      m_fileExtensions = extensions;
    else
      m_fileExtensions = new String[] { "*.*" }; //$NON-NLS-1$
    m_loadData = loadData;
    m_selectFeature = selectFeature;
    if( root != null )
      m_root = root;
    else
      m_root = ResourcesPlugin.getWorkspace().getRoot();
    m_selectableClasses = selectableClasses;
  }

  public GmlShapeFileImportDialog( final Shell parentShell, final boolean withStyleSelection, final boolean chooseFromLocalFileSystem, final boolean selectFeature, final IContainer root, final Class< ? extends Object>[] selectableClasses )
  {
    this( parentShell, withStyleSelection, chooseFromLocalFileSystem, true, selectFeature, new String[] { SHAPE_FILE_EXT, GML_FILE_EXT }, root, selectableClasses );

  }

  public GmlShapeFileImportDialog( final Shell parentShell, final boolean withStyleSeleciton, final boolean chooseFromLocalFileSystem, final boolean selectFeature, final String[] extensions, final IContainer root, final QName[] selectableQNames )
  {
    this( parentShell, withStyleSeleciton, chooseFromLocalFileSystem, true, selectFeature, extensions, root, new Class< ? >[0] );
    m_selectableQNames = selectableQNames;
  }

  public void setFilter( final ViewerFilter filter )
  {
    m_filter = filter;
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createDialogArea( final Composite parent )
  {
    final Composite topComposite = (Composite) super.createDialogArea( parent );
    topComposite.setFont( parent.getFont() );

    initializeDialogUnits( parent );

    topComposite.setLayout( new GridLayout() );
    topComposite.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    // build file group
    createFileGroup( topComposite );
    createCoordinateGroup( topComposite );
    if( m_withStyleSelection )
      createStyleGroup( topComposite );
    if( m_selectFeature )
      createGmlTreeView( topComposite );
    return topComposite;
  }

  private void createCoordinateGroup( final Composite topComposite )
  {
    m_crsGroup = new Group( topComposite, SWT.NULL );
    m_crsGroup.setLayout( new GridLayout( 3, false ) );
    final GridData crsData = new GridData();
    crsData.verticalSpan = 1;
    crsData.horizontalAlignment = SWT.FILL;
    m_crsGroup.setLayoutData( crsData );
    m_crsGroup.setText( Messages.getString( "org.kalypso.ui.dialog.GmlShapeFileImportDialog.3" ) ); //$NON-NLS-1$
    final Label crsLabel = new Label( m_crsGroup, SWT.NONE );
    crsLabel.setText( Messages.getString( "org.kalypso.ui.dialog.GmlShapeFileImportDialog.4" ) ); //$NON-NLS-1$

    m_checkCRS = new Combo( m_crsGroup, SWT.NONE );

    availableCoordinateSystems( m_checkCRS );

    final String defaultCS = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
    m_checkCRS.select( m_checkCRS.indexOf( defaultCS ) );

    m_checkCRS.setToolTipText( Messages.getString( "org.kalypso.ui.dialog.GmlShapeFileImportDialog.5" ) ); //$NON-NLS-1$
    final GridData data = new GridData( GridData.FILL_HORIZONTAL );
    data.widthHint = SIZING_TEXT_FIELD_WIDTH;
    m_checkCRS.setLayoutData( data );
    m_checkCRS.addSelectionListener( new SelectionAdapter()
    {

      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        loadWorkspace();
      }
    } );
    m_crsGroup.setVisible( false );
    m_crsGroup.pack();
  }

  private void createGmlTreeView( final Composite parent )
  {
    final Group treeGroup = new Group( parent, SWT.NULL );
    treeGroup.setLayout( new GridLayout() );
    treeGroup.setLayoutData( new GridData() );
    treeGroup.setText( Messages.getString( "org.kalypso.ui.dialog.GmlShapeFileImportDialog.6" ) ); //$NON-NLS-1$
    m_viewerComposite = new Composite( treeGroup, SWT.SINGLE );
    final GridLayout layout = new GridLayout();
    layout.numColumns = 1;
    layout.verticalSpacing = 2;
    layout.marginWidth = 0;
    layout.marginHeight = 2;
    m_viewerComposite.setLayout( layout );
    m_viewerComposite.addFocusListener( new FocusAdapter()
    {

      /**
       * @see org.eclipse.swt.events.FocusAdapter#focusGained(org.eclipse.swt.events.FocusEvent)
       */
      @Override
      public void focusGained( final FocusEvent e )
      {
        BusyIndicator.showWhile( getShell().getDisplay(), new Runnable()
        {

          public void run( )
          {
            if( m_sourceFileText.getText().length() > 0 )
              loadWorkspace();
            m_treeViewer.setInput( new CommandableWorkspace( m_workspace ) );
          }
        } );
      }
    } );
    m_treeViewer = new TreeViewer( m_viewerComposite );
    m_treeViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      /**
       * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
       */
      public void selectionChanged( final SelectionChangedEvent event )
      {
        validateSelection( (IStructuredSelection) event.getSelection() );
      }
    } );

    m_treeViewer.setContentProvider( new GMLContentProvider( true, false ) );
    m_treeViewer.setLabelProvider( new GMLLabelProvider() );
    m_treeViewer.setUseHashlookup( true );
    if( m_filter != null )
      m_treeViewer.addFilter( m_filter );

    GridData layoutData = new GridData();
    layoutData = new GridData();
    layoutData.widthHint = 450;
    layoutData.heightHint = 450;
    layoutData.grabExcessHorizontalSpace = true;
    layoutData.grabExcessVerticalSpace = true;
    layoutData.horizontalAlignment = GridData.FILL;
    layoutData.verticalAlignment = GridData.FILL;
    m_treeViewer.getControl().setLayoutData( layoutData );
    m_viewerComposite.pack();
  }

  /**
   * @param composite
   */
  private void createStyleGroup( final Composite composite )
  {
    // style
    m_styleGroup = new Group( composite, SWT.NULL );
    m_styleGroup.setText( Messages.getString( "org.kalypso.ui.dialog.GmlShapeFileImportDialog.7" ) ); //$NON-NLS-1$

    final GridData data3 = new GridData();
    data3.horizontalAlignment = GridData.FILL;
    data3.grabExcessHorizontalSpace = true;
    m_styleGroup.setLayoutData( data3 );
    final GridLayout gridLayout1 = new GridLayout();
    gridLayout1.numColumns = 3;
    m_styleGroup.setLayout( gridLayout1 );

    m_styleLabel = new Label( m_styleGroup, SWT.NONE );
    m_styleLabel.setText( Messages.getString( "org.kalypso.ui.dialog.GmlShapeFileImportDialog.8" ) ); //$NON-NLS-1$

    m_styleTextField = new Text( m_styleGroup, SWT.BORDER );
    final GridData data4 = new GridData();
    data4.horizontalAlignment = GridData.FILL;
    data4.grabExcessHorizontalSpace = true;
    m_styleTextField.setLayoutData( data4 );
    m_styleTextField.setEditable( false );

    m_browseButton2 = new Button( m_styleGroup, SWT.PUSH );
    m_browseButton2.setText( Messages.getString( "org.kalypso.ui.dialog.GmlShapeFileImportDialog.9" ) ); //$NON-NLS-1$
    m_browseButton2.setLayoutData( new GridData( GridData.END ) );
    m_browseButton2.addSelectionListener( new SelectionAdapter()
    {

      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        handleBrowseButton2Selected();
      }

    } );

    m_styleNameLabel = new Label( m_styleGroup, SWT.NONE );
    m_styleNameLabel.setText( Messages.getString( "org.kalypso.ui.dialog.GmlShapeFileImportDialog.10" ) ); //$NON-NLS-1$

    m_styleNameCombo = new Combo( m_styleGroup, SWT.READ_ONLY );
    final GridData data5 = new GridData();
    data5.horizontalAlignment = GridData.FILL;
    data5.grabExcessHorizontalSpace = true;
    m_styleNameCombo.setLayoutData( data5 );
    m_styleNameCombo.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        // TODO: handle the new style loaded from either the local file system or the workspace
      }
    } );

    final Label dummyLabel = new Label( m_styleGroup, SWT.NONE );
    dummyLabel.setText( "" ); //$NON-NLS-1$

    m_checkDefaultStyleButton = new Button( m_styleGroup, SWT.CHECK );
    m_checkDefaultStyleButton.setSelection( false );
    m_checkDefaultStyleButton.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        handleCheckDefaultStyleButtonSelected();
      }
    } );

    final Label defaultStyleLabel = new Label( m_styleGroup, SWT.NONE );
    defaultStyleLabel.setText( Messages.getString( "org.kalypso.ui.dialog.GmlShapeFileImportDialog.12" ) ); //$NON-NLS-1$

  }

  private void createFileGroup( final Composite parent )
  {
    m_group = new Group( parent, SWT.NULL );
    final GridLayout topGroupLayout = new GridLayout();
    final GridData topGroupData = new GridData();
    topGroupLayout.numColumns = 3;
    topGroupData.horizontalAlignment = GridData.FILL;
    m_group.setLayout( topGroupLayout );
    m_group.setLayoutData( topGroupData );
    m_group.setText( Messages.getString( "org.kalypso.ui.dialog.GmlShapeFileImportDialog.13" ) ); //$NON-NLS-1$
    m_sourceFileLabel = new Label( m_group, SWT.NONE );
    m_sourceFileLabel.setText( Messages.getString( "org.kalypso.ui.dialog.GmlShapeFileImportDialog.14" ) ); //$NON-NLS-1$

    // Set width of source path field
    final GridData data0 = new GridData( GridData.FILL_HORIZONTAL );
    data0.widthHint = SIZING_TEXT_FIELD_WIDTH;

    m_sourceFileText = new Text( m_group, SWT.BORDER );
    m_sourceFileText.setLayoutData( data0 );
    m_sourceFileText.setEditable( false );
    m_sourceFileText.addFocusListener( new FocusAdapter()
    {

      /**
       * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
       */
      @Override
      public void focusLost( final FocusEvent e )
      {

        if( m_selectFeature )
        {
          BusyIndicator.showWhile( getShell().getDisplay(), new Runnable()
          {

            public void run( )
            {
              final File file = getFile();
              if( file != null && m_workspace != null )
              {
                final IPath path = Path.fromOSString( file.toString() );
                if( path.isValidPath( m_sourceFileText.getText() ) )
                  loadWorkspace();
                m_treeViewer.setInput( new CommandableWorkspace( m_workspace ) );
              }
            }
          } );
        }
      }
    } );

    m_browseButton = new Button( m_group, SWT.PUSH );
    m_browseButton.setText( Messages.getString( "org.kalypso.ui.dialog.GmlShapeFileImportDialog.15" ) ); //$NON-NLS-1$
    m_browseButton.setLayoutData( new GridData( GridData.END ) );
    m_browseButton.addSelectionListener( new SelectionAdapter()
    {

      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        handleBrowseButtonSelected();
      }
    } );
    m_group.pack();

  }

  private void availableCoordinateSystems( final Combo checkCRS )
  {
    checkCRS.setItems( CRSHelper.getAllNames().toArray( new String[] {} ) );
  }

  protected KalypsoResourceSelectionDialog createResourceDialog( final String[] fileResourceExtensions )
  {
    if( m_root != null )
      return new KalypsoResourceSelectionDialog( getShell(), m_root, Messages.getString( "org.kalypso.ui.dialog.GmlShapeFileImportDialog.16" ), fileResourceExtensions, m_root, new ResourceSelectionValidator() ); //$NON-NLS-1$
    return new KalypsoResourceSelectionDialog( getShell(), m_eclipseWorkspace, Messages.getString( "org.kalypso.ui.dialog.GmlShapeFileImportDialog.17" ), fileResourceExtensions, m_eclipseWorkspace, new ResourceSelectionValidator() ); //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#okPressed()
   */
  @Override
  protected void okPressed( )
  {
    if( m_loadData )
    {
      loadWorkspace();
    }
    super.okPressed();
  }

  void loadWorkspace( )
  {
    try
    {
      final File file = getFile();
      if( file != null )
      {
        if( isShape() )
        {
          m_crsGroup.setVisible( true );
          m_crsGroup.pack();
          m_workspace = ShapeSerializer.deserialize( getFile().toString(), getCRS() );
        }
        else if( isGml() )
        {
          m_crsGroup.setVisible( false );
          m_workspace = GmlSerializer.createGMLWorkspace( file.toURL(), null );
        }
      }
    }
    catch( final Exception e )
    {
      final IStatus status = StatusUtilities.createErrorStatus( e.getLocalizedMessage() );
      ErrorDialog.openError( getShell(), Messages.getString( "org.kalypso.ui.dialog.GmlShapeFileImportDialog.18" ), e.getMessage(), status ); //$NON-NLS-1$
    }

  }

  private boolean isGml( )
  {
    return m_relativeSourcePath.getFileExtension().endsWith( GML_FILE_EXT );
  }

  public File getFile( ) throws NullPointerException
  {
    final String location = m_eclipseWorkspace.getLocation() + "/"; //$NON-NLS-1$

    String fileExtension = null;
    if( m_relativeSourcePath != null )
      fileExtension = m_relativeSourcePath.getFileExtension();
    if( fileExtension != null )
    {
      if( fileExtension.endsWith( SHAPE_FILE_EXT ) )
        return new File( location + FileUtilities.nameWithoutExtension( m_relativeSourcePath.toString() ) );
      return new File( location + m_relativeSourcePath.toString() );
    }
    return new File( location );
  }

  /**
   * TODO: This is not used! Can it be removed?
   */
// public GM_Object getGeometry( )
// {
// final Feature root = m_workspace.getRootFeature();
// final FeatureList featureLIST = (FeatureList) root.getProperty( DBaseFile.PROPERTY_CUSTOM_FEATURE_MEMBER );
// final Feature fe = (Feature) featureLIST.get( 0 );
// final IPropertyType geoPT = fe.getFeatureType().getProperty( ShapeSerializer.PROPERTY_GEOMETRY );
// return (GM_Object) fe.getProperty( geoPT );
// }
  public GMLWorkspace getWorkspace( )
  {
    return m_workspace;
  }

  protected void handleBrowseButton2Selected( )
  {
    Object[] result = null;
    if( !m_fromLocalFileSys )
    {
      final KalypsoResourceSelectionDialog dialog = createResourceDialog( new String[] { "sld" } ); //$NON-NLS-1$
      dialog.open();
      result = dialog.getResult();
    }
    else
    {
      final FileDialog dialog = new FileDialog( getShell() );
      result = new Object[] { dialog.open() };
    }
    if( result != null )
    {
      final IPath resultPath = (Path) result[0];
      m_styleTextField.setText( resultPath.toString() );
      m_stylePath = resultPath;

      InputStream sldIS = null;
      try
      {
        final IPath basePath = m_eclipseWorkspace.getLocation();
        final String styleURLAsString = basePath.toFile().toURL() + m_stylePath.removeFirstSegments( 1 ).toString();
        final URL styleURL = new URL( styleURLAsString );
        sldIS = styleURL.openStream();

        final IUrlResolver2 resolver = new IUrlResolver2()
        {
          public URL resolveURL( final String href ) throws MalformedURLException
          {
            return UrlResolverSingleton.resolveUrl( styleURL, href );
          }
        };

        final StyledLayerDescriptor styledLayerDescriptor = SLDFactory.createSLD( resolver, sldIS );
        sldIS.close();

        final Layer[] layers = styledLayerDescriptor.getLayers();
        final Vector<String> styleNameVector = new Vector<String>();
        for( final Layer layer : layers )
        {
          final Style[] styles = layer.getStyles();
          for( final Style element : styles )
          {
            styleNameVector.add( element.getName() );
          }
        }
        final String[] styleNames = new String[styleNameVector.size()];
        for( int k = 0; k < styleNameVector.size(); k++ )
        {
          styleNames[k] = styleNameVector.get( k );
        }
        m_styleNameCombo.setItems( styleNames );
        m_styleNameCombo.select( 0 );
      }
      catch( final MalformedURLException e1 )
      {
        e1.printStackTrace();
      }
      catch( final IOException ioEx )
      {
        ioEx.printStackTrace();
      }
      catch( final XMLParsingException xmlEx )
      {
        xmlEx.printStackTrace();
      }
      finally
      {
        IOUtils.closeQuietly( sldIS );
      }
    }
  }

  protected void handleCheckDefaultStyleButtonSelected( )
  {
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

  protected void handleBrowseButtonSelected( )
  {
    Object[] result = null;
    if( !m_fromLocalFileSys )
    {
      final KalypsoResourceSelectionDialog dialog = createResourceDialog( m_fileExtensions );
      dialog.open();
      result = dialog.getResult();
    }
    else
    {
      final FileDialog dialog = new FileDialog( getShell() );
      result = new Object[] { dialog.open() };

    }
    if( result != null )
    {
      final Path resultPath = (Path) result[0];
      m_sourceFileText.setText( resultPath.toString() );
      m_relativeSourcePath = resultPath;
    }

    if( m_selectFeature )
    {
      loadWorkspace();
    }
  }

  private boolean isShape( )
  {
    return m_relativeSourcePath.getFileExtension().endsWith( SHAPE_FILE_EXT );
  }

  boolean validateFeaturePathFromSelection( final IStructuredSelection selection )
  {
    final List<String> pathList = new ArrayList<String>();
    final List<String> titleList = new ArrayList<String>();
    final Object firstElement = selection.getFirstElement();
    if( firstElement instanceof Feature )
    { // create featurepath for element
      final Feature feature = (Feature) firstElement;
      final FeaturePath featurepath = m_workspace.getFeaturepathForFeature( feature );
      final IFeatureType ft = feature.getFeatureType();
      // find title
      String title = NamedFeatureHelper.getName( feature );
      if( title == null || title.length() < 1 )
        title = ft.getAnnotation().getLabel();
      pathList.add( featurepath.toString() );
      titleList.add( title );
    }
    else if( firstElement instanceof FeatureAssociationTypeElement )
    {
      // create featurepath for association
      final FeatureAssociationTypeElement link = (FeatureAssociationTypeElement) firstElement;
      final Feature parent = link.getParentFeature();
      final FeaturePath parentFeaturePath = getWorkspace().getFeaturepathForFeature( parent );
      final IRelationType ftp = link.getAssociationTypeProperty();

      final IFeatureType associationFeatureType = ftp.getTargetFeatureType();
      final IFeatureType[] associationFeatureTypes = GMLSchemaUtilities.getSubstituts( associationFeatureType, null, false, true );

      for( final IFeatureType ft : associationFeatureTypes )
      {
        final String title = ft.getAnnotation().getLabel();
        final FeaturePath path = new FeaturePath( parentFeaturePath, ftp.getQName().getLocalPart() + "[" + ft.getQName().getLocalPart() + "]" ); //$NON-NLS-1$ //$NON-NLS-2$
        pathList.add( path.toString() );
        titleList.add( title );
      }
    }
    return !pathList.isEmpty();
  }

  public Object getSelectedObject( )
  {
    return m_selectedObject;

  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#cancelPressed()
   */
  @Override
  protected void cancelPressed( )
  {
    super.cancelPressed();
    m_workspace = null;
  }

  void validateSelection( final IStructuredSelection selection )
  {
    final Button button = getButton( IDialogConstants.OK_ID );
    button.setEnabled( false );
    final Object firstElement = selection.getFirstElement();
    if( m_selectableClasses != null && m_selectableClasses.length > 0 )
    {
      if( firstElement != null )
      {
        final Class< ? extends Object> class1 = firstElement.getClass();
        for( final Class< ? extends Object> clazz : m_selectableClasses )
        {
          boolean test = clazz.isAssignableFrom( class1 );
          if( test )
          {
            m_selectedObject = firstElement;
            button.setEnabled( test );
            return;
          }
          else
          {
            test = class1.isAssignableFrom( clazz );
            if( test )
            {
              m_selectedObject = firstElement;
              button.setEnabled( test );
              return;
            }
          }
        }
      }
    }
    else if( m_selectableQNames != null && m_selectableQNames.length > 0 )
    {
      for( final Iterator< ? > it = selection.iterator(); it.hasNext(); )
      {
        final Object o = it.next();
        if( o instanceof Feature )
        {
          boolean test = false;
          for( final QName qName : m_selectableQNames )
          {
            final Feature feature = ((Feature) o);
            final IFeatureType ft = feature.getFeatureType();
            test = ft.getQName().equals( qName );
            if( test )
            {
              m_selectedObject = feature;
              // System.out.println();
              break;
            }
            else
              try
              {
                m_selectedObject = feature.getProperty( qName );
              }
              catch( final IllegalArgumentException e )
              {
                m_selectedObject = null;
                test = m_selectedObject != null;
              }
          }
          button.setEnabled( test );
        }
      }
    }

  }

  public String getCRS( )
  {
    return m_checkCRS.getText();
  }

  public boolean isDefaultStyle( )
  {
    return m_checkDefaultStyleButton.getSelection();
  }
}