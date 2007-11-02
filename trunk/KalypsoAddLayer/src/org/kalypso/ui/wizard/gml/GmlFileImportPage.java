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

package org.kalypso.ui.wizard.gml;

import java.net.URL;

import javax.xml.namespace.QName;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.dialog.DialogPageUtilitites;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.contribs.eclipse.ui.dialogs.KalypsoResourceSelectionDialog;
import org.kalypso.contribs.eclipse.ui.dialogs.ResourceSelectionValidator;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.editor.gmleditor.ui.FeatureAssociationTypeElement;
import org.kalypso.ui.editor.gmleditor.ui.GMLContentProvider;
import org.kalypso.ui.editor.gmleditor.ui.GMLEditorLabelProvider2;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author Kuepferle
 */
public class GmlFileImportPage extends WizardPage
{
  private final static int DEFAULT_EXPANSION_LEVEL = 3;

  private IProject m_selectedProject;

  private Text m_sourceFileText;

  private TreeViewer m_treeViewer;

  private GMLWorkspace m_workspace;

  private String m_source;

  private QName[] m_validQnames = new QName[0];

  private IStructuredSelection m_currentSelection;

  private boolean m_validAllowFeature = true;

  private boolean m_validAllowFeatureAssociation = true;

  public GmlFileImportPage( final String pageName, final String title, final ImageDescriptor titleImage )
  {
    super( pageName, title, titleImage );
  }

  /** If set to non-<code>null</code>, only files from within this project may be selected. */
  public void setProjectSelection( final IProject project )
  {
    m_selectedProject = project;
  }

  public String getSource( )
  {
    return m_source;
  }

  public IStructuredSelection getSelection( )
  {
    return m_currentSelection;
  }

  public GMLWorkspace getWorkspace( )
  {
    return m_workspace;
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    final Composite topComposite = new Composite( parent, SWT.NULL );
    topComposite.setLayout( new GridLayout() );
    topComposite.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    final Control fileControl = createFileGroup( topComposite );
    fileControl.setLayoutData( new GridData( SWT.FILL, SWT.BEGINNING, true, false ) );

    final Control treeControl = createTreeView( topComposite );
    treeControl.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    setControl( topComposite );
    setPageComplete( false );
  }

  private Control createTreeView( final Composite composite )
  {
    m_treeViewer = new TreeViewer( composite );
    m_treeViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      /**
       * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
       */
      public void selectionChanged( final SelectionChangedEvent event )
      {
        handleTreeSelection( (IStructuredSelection) event.getSelection() );
      }
    } );
    m_treeViewer.setContentProvider( new GMLContentProvider() );
    m_treeViewer.setLabelProvider( new GMLEditorLabelProvider2() );
    m_treeViewer.setUseHashlookup( true );

    return m_treeViewer.getControl();
  }

  private Control createFileGroup( Composite parent )
  {
    final Group group = new Group( parent, SWT.NULL );
    final GridLayout topGroupLayout = new GridLayout( 3, false );
    group.setLayout( topGroupLayout );

    group.setText( "Datei auswählen" );
    final Label fileLabel = new Label( group, SWT.NONE );
    fileLabel.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, false, false ) );
    fileLabel.setText( "GML Datei: " );

    m_sourceFileText = new Text( group, SWT.BORDER );
    m_sourceFileText.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    m_sourceFileText.setEditable( false );

    final Button browseButton = new Button( group, SWT.PUSH );
    browseButton.setText( "&Durchsuchen..." );
    browseButton.setLayoutData( new GridData( SWT.END, SWT.CENTER, false, false ) );
    browseButton.addSelectionListener( new SelectionAdapter()
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

    return group;
  }

  protected void handleBrowseButtonSelected( )
  {
    final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();

    final IContainer container = m_selectedProject == null ? root : m_selectedProject;

    final KalypsoResourceSelectionDialog dialog = new KalypsoResourceSelectionDialog( getShell(), null, "Auswählen einer GML Datei", new String[] { "gml" }, container, new ResourceSelectionValidator() );
    if( dialog.open() != Window.OK )
      return;

    // get first element, only one element possible
    final IPath selection = (IPath) dialog.getResult()[0];
    final IFile resource = root.getFile( selection );

    // create project path (Kalypso project-protocol)
    final String source;
    if( m_selectedProject == null )
      source = ResourceUtilities.createURLSpec( selection );
    else
      source = "project:/" + selection.removeFirstSegments( 1 ).toString();
    m_sourceFileText.setText( selection.toString() );

    final ICoreRunnableWithProgress progress = new ICoreRunnableWithProgress()
    {
      public IStatus execute( final IProgressMonitor monitor )
      {
        try
        {
          monitor.beginTask( "GML wird geladen...", 1 );
          final URL gmlURL = ResourceUtilities.createURL( resource );
          final CommandableWorkspace workspace = new CommandableWorkspace( GmlSerializer.createGMLWorkspace( gmlURL, null ) );
          setWorkspace( workspace, source );
          return Status.OK_STATUS;
        }
        catch( final Exception e1 )
        {
          final IStatus status = StatusUtilities.statusFromThrowable( e1 );
          return status;
        }
      }
    };

    final IStatus status = RunnableContextHelper.execute( getContainer(), true, false, progress );
    ErrorDialog.openError( getShell(), getWizard().getWindowTitle(), "GML konnte nicht geladen werden", status );

    m_treeViewer.getTree().setVisible( true );
    m_treeViewer.setInput( m_workspace );
    // need to expand in order to load all elements into the treeviewers
    // cache
    m_treeViewer.expandToLevel( DEFAULT_EXPANSION_LEVEL );
    m_treeViewer.getTree().setFocus();
    m_treeViewer.setSelection( new StructuredSelection( m_workspace.getRootFeature() ) );
  }

  protected void setWorkspace( final CommandableWorkspace workspace, final String source )
  {
    m_workspace = workspace;
    m_source = source;

    m_currentSelection = null;
  }

  protected void handleTreeSelection( final IStructuredSelection selection )
  {
    m_currentSelection = selection;

    final IStatus status = validateCurrentSelection();
    if( status.isOK() )
      setMessage( getDescription() );
    else
      setMessage( status.getMessage(), DialogPageUtilitites.severityToMessagecode( status ) );

    setPageComplete( status.isOK() );
  }

  private IStatus validateCurrentSelection( )
  {
    if( m_currentSelection == null || m_currentSelection.isEmpty() )
      return StatusUtilities.createWarningStatus( "Kein Element gewählt" );

    final Object[] objects = m_currentSelection.toArray();
    for( final Object object : objects )
    {
      if( m_validAllowFeature && object instanceof Feature )
      {
        final Feature f = (Feature) object;
        final IFeatureType featureType = f.getFeatureType();

        if( !checkFeatureTypeValid( featureType ) )
          return StatusUtilities.createWarningStatus( "Gewähltes Element ungültig" );

        return Status.OK_STATUS;
      }
      else if( m_validAllowFeatureAssociation && object instanceof FeatureAssociationTypeElement )
      {
        final FeatureAssociationTypeElement fate = (FeatureAssociationTypeElement) object;
        final IRelationType associationRt = fate.getAssociationTypeProperty();
        final IFeatureType targetFeatureType = associationRt.getTargetFeatureType();
        if( !checkFeatureTypeValid( targetFeatureType ) )
          return StatusUtilities.createWarningStatus( "Gewähltes Element ungültig" );

        return Status.OK_STATUS;
      }

      return StatusUtilities.createWarningStatus( "Kein gültiges Element gewählt" );
    }

    return Status.OK_STATUS;
  }

  /**
   * Check if given feature type substitutes at least one of our valid qnames.
   * <p>
   * Returns always true if {@link #m_validQnames} is not set or empty.
   */
  private boolean checkFeatureTypeValid( final IFeatureType featureType )
  {
    if( m_validQnames == null || m_validQnames.length == 0 )
      return true;

    for( final QName qname : m_validQnames )
    {
      if( GMLSchemaUtilities.substitutes( featureType, qname ) )
        return true;
    }

    return false;
  }

  /** If set, all selected elements must substitute one of the given qnames. */
  public void setValidQNames( final QName[] validQnames )
  {
    m_validQnames = validQnames;
  }

  /** Determines what kind of objects may be selected */
  public void setValidKind( final boolean allowFeature, final boolean allowFeatureAssociation )
  {
    m_validAllowFeature = allowFeature;
    m_validAllowFeatureAssociation = allowFeatureAssociation;
  }
}