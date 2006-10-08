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

package org.kalypso.ui.wizard.gml;

import java.net.URL;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.commons.command.ICommand;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.ui.dialogs.KalypsoResourceSelectionDialog;
import org.kalypso.contribs.eclipse.ui.dialogs.ResourceSelectionValidator;
import org.kalypso.contribs.java.net.UrlResolver;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.AnnotationUtilities;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.action.AddThemeCommand;
import org.kalypso.ui.editor.gmleditor.ui.FeatureAssociationTypeElement;
import org.kalypso.ui.editor.gmleditor.ui.GMLEditorContentProvider2;
import org.kalypso.ui.editor.gmleditor.ui.GMLEditorLabelProvider2;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeaturePath;

/**
 * @author Kuepferle
 */
public class GmlFileImportPage extends WizardPage implements SelectionListener, ModifyListener
{
  private final static int DEFAULT_EXPANSION_LEVEL = 3;

  private IProject m_selectedProject;

  private Text m_sourceFileText;

  private Button m_browseButton;

  private static final int SIZING_TEXT_FIELD_WIDTH = 250;

  private Composite m_topComposite;

  private TreeViewer m_treeViewer;

  private Composite m_viewerComposite;

  private UrlResolver m_urlResolver = new UrlResolver();

  private GMLWorkspace m_workspace;

  private String m_source;

  private HashSet<Feature> m_feature = null;

  private IRelationType m_fatp = null;

  private HashSet<IRelationType> m_featureAssTypeProp = null;

  private List m_pathList;

  private List m_titleList;

  public GmlFileImportPage( String pageName )
  {
    super( pageName );

  }

  /*
   * @author kuepfer
   */
  protected GmlFileImportPage( String pageName, String title, ImageDescriptor titleImage )
  {
    super( pageName, title, titleImage );

  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( Composite parent )
  {
    m_topComposite = new Composite( parent, SWT.NULL );
    m_topComposite.setLayout( new GridLayout() );
    m_topComposite.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    createFileGroup( m_topComposite );
    createTreeView( m_topComposite );
    setControl( m_topComposite );
    setPageComplete( false );
  }

  private void createTreeView( Composite composite )
  {
    m_viewerComposite = new Composite( composite, SWT.SINGLE );
    GridLayout layout = new GridLayout();
    layout.numColumns = 1;
    layout.verticalSpacing = 2;
    layout.marginWidth = 0;
    layout.marginHeight = 2;
    m_viewerComposite.setLayout( layout );
    // m_viewer = new GmlTreeView( m_viewerComposite, false );
    m_treeViewer = new TreeViewer( m_viewerComposite );
    m_treeViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      /**
       * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
       */
      public void selectionChanged( SelectionChangedEvent event )
      {
        validateFeaturePathFromSelection( (IStructuredSelection) event.getSelection() );
      }
    } );
    m_treeViewer.setContentProvider( new GMLEditorContentProvider2() );
    m_treeViewer.setLabelProvider( new GMLEditorLabelProvider2() );
    m_treeViewer.setUseHashlookup( true );

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

  private void createFileGroup( Composite parent )
  {
    Group m_group = new Group( parent, SWT.NULL );
    GridLayout topGroupLayout = new GridLayout();
    GridData topGroupData = new GridData();
    topGroupLayout.numColumns = 3;
    topGroupData.horizontalAlignment = GridData.FILL;
    m_group.setLayout( topGroupLayout );
    m_group.setLayoutData( topGroupData );
    m_group.setText( "Datei ausw�hlen" );
    Label m_fileLabel = new Label( m_group, SWT.NONE );
    m_fileLabel.setText( "GML Datei: " );

    GridData data0 = new GridData( GridData.FILL_HORIZONTAL );
    data0.widthHint = SIZING_TEXT_FIELD_WIDTH;

    m_sourceFileText = new Text( m_group, SWT.BORDER );
    m_sourceFileText.setLayoutData( data0 );
    m_sourceFileText.setEditable( false );
    m_sourceFileText.addModifyListener( this );

    m_browseButton = new Button( m_group, SWT.PUSH );
    m_browseButton.setText( "Durchsuchen..." );
    m_browseButton.setLayoutData( new GridData( GridData.END ) );
    m_browseButton.addSelectionListener( this );
  }

  public void setProjectSelection( IProject project )
  {
    m_selectedProject = project;
  }

  /**
   * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
   */
  public void widgetSelected( SelectionEvent e )
  {
    if( e.widget == m_browseButton )
    {
      final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();

      final IContainer container = m_selectedProject == null ? root : m_selectedProject;

      final KalypsoResourceSelectionDialog dialog = new KalypsoResourceSelectionDialog( getShell(), null, "Ausw�hlen einer GML Datei", new String[] { "gml" }, container, new ResourceSelectionValidator() );
      dialog.open();
      // get first element, only one element possible
      final IPath selection = (IPath) dialog.getResult()[0];
      final IFile resource = root.getFile( selection );

      // create project path (Kalypso project-protocol)
      if( m_selectedProject == null )
        m_source = ResourceUtilities.createURLSpec( selection );
      else
        m_source = "project:/" + selection.removeFirstSegments( 1 ).toString();
      m_sourceFileText.setText( selection.toString() );

      try
      {
        final URL gmlURL = ResourceUtilities.createURL( resource );
        m_workspace = new CommandableWorkspace( GmlSerializer.createGMLWorkspace( gmlURL, m_urlResolver, null ) );
      }
      catch( Exception e1 )
      {
        e1.printStackTrace();
      }
      m_treeViewer.getTree().setVisible( true );
      m_treeViewer.setInput( m_workspace );
      // need to expand in order to load all elements into the treeviewers
      // cache
      m_treeViewer.expandToLevel( DEFAULT_EXPANSION_LEVEL );
    }
  }

  /**
   * @see org.eclipse.swt.events.SelectionListener#widgetDefaultSelected(org.eclipse.swt.events.SelectionEvent)
   */
  public void widgetDefaultSelected( SelectionEvent e )
  {
    // nothing to do
  }

  /**
   * @see org.eclipse.swt.events.ModifyListener#modifyText(org.eclipse.swt.events.ModifyEvent)
   */
  public void modifyText( ModifyEvent e )
  {
    // do nothing
  }

  void validateFeaturePathFromSelection( final IStructuredSelection selection )
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
      String title = null;
      try
      // guess title
      {
        title = (String) feature.getProperty( "name" );
      }
      catch( Exception e )
      {
        // nothing
      }
      if( title == null || title.length() < 1 )
        title = AnnotationUtilities.getAnnotation( ft ).getLabel();
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

      for( int i = 0; i < associationFeatureTypes.length; i++ )
      {
        final IFeatureType ft = associationFeatureTypes[i];
        final String title = AnnotationUtilities.getAnnotation( ft ).getLabel();
        final FeaturePath path = new FeaturePath( parentFeaturePath, ftp.getName() + "[" + ft.getName() + "]" );
        pathList.add( path.toString() );
        titleList.add( title );
      }
    }
    m_pathList = pathList;
    m_titleList = titleList;
    setPageComplete( !m_pathList.isEmpty() );
  }

  public String getSource( )
  {
    return m_source;
  }

  public IRelationType getFatp( )
  {
    return m_fatp;
  }

  public Feature[] getFeatures( )
  {
    return m_feature.toArray( new Feature[m_feature.size()] );
  }

  public void removerListeners( )
  {
    m_browseButton.removeSelectionListener( this );
    m_sourceFileText.removeModifyListener( this );
  }

  public IRelationType[] getFeatureAssociations( )
  {
    return m_featureAssTypeProp.toArray( new IRelationType[m_featureAssTypeProp.size()] );

  }

  private GMLWorkspace getWorkspace( )
  {
    return m_workspace;
  }

  public ICommand[] getCommands( final GisTemplateMapModell mapModell ) throws Exception
  {
    final ICommand[] result = new ICommand[m_pathList.size()];
    final Iterator titleIterator = m_titleList.iterator();
    int pos = 0;
    for( Iterator pathIterator = m_pathList.iterator(); pathIterator.hasNext(); pos++ )
    {
      final String title = (String) titleIterator.next();
      final String featurePath = (String) pathIterator.next();
      result[pos] = new AddThemeCommand( mapModell, title, "gml", featurePath, getSource() );
    }
    return result;
  }
}