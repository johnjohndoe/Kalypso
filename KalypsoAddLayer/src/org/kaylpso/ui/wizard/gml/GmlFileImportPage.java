package org.kaylpso.ui.wizard.gml;

import java.net.URL;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ISelection;
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
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.editor.gmleditor.ui.GMLEditorContentProvider;
import org.kalypso.ui.editor.gmleditor.ui.GMLEditorLabelProvider;
import org.kalypso.ui.editor.gmleditor.util.GMLReader;
import org.kalypso.ui.editor.gmleditor.util.model.FeatureElement;
import org.kalypso.ui.editor.gmleditor.util.model.IModel;
import org.kalypso.ui.editor.gmleditor.util.model.Model;
import org.kalypso.ui.editor.gmleditor.util.model.PropertyElement;
import org.kalypso.util.UrlResolver;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureAssociationTypeProperty;
import org.kaylpso.ui.dialog.KalypsoResourceSelectionDialog;

/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
public class GmlFileImportPage extends WizardPage implements SelectionListener, ModifyListener,
    ISelectionChangedListener
{

  private final static int DEFAULT_EXPANSION_LEVEL = 3;

  private IProject m_selectedProject;

  private Text m_sourceFileText;

  private Button m_browseButton;

  private URL m_activeMapContext;

  private static final int SIZING_TEXT_FIELD_WIDTH = 250;

  private Composite m_topComposite;

  private TreeViewer m_treeViewer;

  private Composite m_viewerComposite;

  private UrlResolver m_urlResolver = new UrlResolver();

  private IModel m_selection;

  private CommandableWorkspace m_workspace;

  private String m_source;

  private Feature m_feature = null;

  private FeatureAssociationTypeProperty m_fatp = null;

  public GmlFileImportPage( String pageName )
  {
    super( pageName );

  }

  /*
   * 
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
    // TODO add file group
    m_topComposite.setLayout( new GridLayout() );
    m_topComposite.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    createFileGroup( m_topComposite );
    createTreeView( m_topComposite );
    setControl( m_topComposite );
    setPageComplete( false );

  }

  private void createTreeView( Composite composite )
  {
    m_viewerComposite = new Composite( composite, SWT.NULL );
    GridLayout layout = new GridLayout();
    layout.numColumns = 1;
    layout.verticalSpacing = 2;
    layout.marginWidth = 0;
    layout.marginHeight = 2;
    m_viewerComposite.setLayout( layout );
    m_treeViewer = new TreeViewer( m_viewerComposite );
    m_treeViewer.addSelectionChangedListener( this );
    m_treeViewer.setContentProvider( new GMLEditorContentProvider() );
    m_treeViewer.setLabelProvider( new GMLEditorLabelProvider() );
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
    m_group.setText( "Datei ausw‰hlen" );
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

  public void setMapContextURL( URL context )
  {
    m_activeMapContext = context;
  }

  /**
   * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
   */
  public void widgetSelected( SelectionEvent e )
  {
    if( e.widget == m_browseButton )
    {
      KalypsoResourceSelectionDialog dialog = new KalypsoResourceSelectionDialog( getShell(), null,
          "Ausw‰hlen einer GML Datei", new String[]
          {
            "gml"
          }, m_selectedProject );
      dialog.open();
      //get first element, only one element possible
      IPath selection = (IPath)dialog.getResult()[0];
      //create project path (Kalypso project-protocol)
      m_source = "project:/" + selection.removeFirstSegments( 1 ).toString();
      m_sourceFileText.setText( selection.toString() );
      try
      {
        final URL gmlURL = m_urlResolver.resolveURL( m_activeMapContext, m_source );
        m_workspace = new CommandableWorkspace( GmlSerializer.createGMLWorkspace( gmlURL,
            m_urlResolver ) );
        //        System.out.println( "selection: " + selection + "\tsource:" +
        // m_source );
      }
      catch( Exception e1 )
      {
        e1.printStackTrace();
      }
      final FeatureElement root = GMLReader.getGMLDocument( m_workspace );
      m_treeViewer.getTree().setVisible( false );
      m_treeViewer.setInput( root );
      // need to expand in order to load all elements into the treeviewers
      // cache
      m_treeViewer.expandAll();
      m_treeViewer.collapseAll();

      m_treeViewer.expandToLevel( DEFAULT_EXPANSION_LEVEL );
      m_treeViewer.getTree().setVisible( true );
    }
  }

  /**
   * @see org.eclipse.swt.events.SelectionListener#widgetDefaultSelected(org.eclipse.swt.events.SelectionEvent)
   */
  public void widgetDefaultSelected( SelectionEvent e )
  {
    //nothing to do
  }

  /**
   * @see org.eclipse.swt.events.ModifyListener#modifyText(org.eclipse.swt.events.ModifyEvent)
   */
  public void modifyText( ModifyEvent e )
  {
    //do nothing
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
   */
  public void selectionChanged( SelectionChangedEvent event )
  {
    if( event.getSource() == m_treeViewer )
    {
      ISelection selection = event.getSelection();
      if( !selection.isEmpty() )
      {
        Object[] array = ( (IStructuredSelection)selection ).toArray();
        for( int i = 0; i < array.length; i++ )
        {
          Object o = array[i];
          if( o instanceof PropertyElement )
          {
            //            System.out.println( "PropertryElement" );
            m_selection = (Model)o;
            m_fatp = ( (PropertyElement)o ).getProperty();
            m_feature = null;
            setPageComplete( true );
          }
          else if( o instanceof FeatureElement )
          {
            //            System.out.println( "FeatureElement" );
            m_selection = (Model)o;
            m_feature = ( (FeatureElement)o ).getFeature();
            m_fatp = null;
            setPageComplete( true );
          }
          else
          {
            m_selection = null;
            setPageComplete( false );
          }

        }//for
      }//if
    }//if
  }//selectionChanged

  public IModel getSelection()
  {
    return m_selection;
  }

  public String getSource()
  {
    return m_source;
  }

  public FeatureAssociationTypeProperty getFatp()
  {
    return m_fatp;
  }

  public Feature getFeature()
  {
    return m_feature;
  }

  public void removerListeners()
  {
    m_browseButton.removeSelectionListener( this );
    m_sourceFileText.removeModifyListener( this );
    m_treeViewer.removeSelectionChangedListener( this );
  }
}