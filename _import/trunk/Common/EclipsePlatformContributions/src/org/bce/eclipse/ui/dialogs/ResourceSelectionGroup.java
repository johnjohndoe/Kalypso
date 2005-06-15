/*******************************************************************************
 * Copyright (c) 2000, 2003 IBM Corporation and others. All rights reserved.
 * This program and the accompanying materials are made available under the
 * terms of the Common Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors: IBM Corporation - initial API and implementation
 ******************************************************************************/
package org.bce.eclipse.ui.dialogs;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.internal.ide.IDEWorkbenchMessages;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.eclipse.ui.part.DrillDownComposite;

public class ResourceSelectionGroup extends Composite
{

  private Listener m_listener;

  private boolean m_allowNewResourceName;

  private boolean m_showClosedProjects;

  private static final String DEFAULT_MSG_NEW_ALLOWED = IDEWorkbenchMessages
      .getString( "ContainerGroup.message" ); //$NON-NLS-1$

  private static final String DEFAULT_MSG_SELECT_ONLY = IDEWorkbenchMessages
      .getString( "ContainerGroup.selectFolder" ); //$NON-NLS-1$

  private static final int SIZING_SELECTION_PANE_WIDTH = 320;

  private static final int SIZING_SELECTION_PANE_HEIGHT = 300;

  private Text resourceNameField;

  TreeViewer treeViewer;

  private IResource selectedResource;

  private String[] m_allowedResourceExtensions;

  private IContainer m_inputContainer;

  /*
   * übernommen von ContainerSelectionGroup und leicht verändert
   * @author peiler
   */
  public ResourceSelectionGroup( Composite parent, Listener listener,
      boolean allowNewContainerName, String message,
      boolean showClosedProjects, String[] allowedResourceExtensions,
      IContainer inputContainer )
  {
    this( parent, listener, allowNewContainerName, message, showClosedProjects,
        SIZING_SELECTION_PANE_HEIGHT, allowedResourceExtensions, inputContainer );
  }

  ResourceSelectionGroup( Composite parent, Listener listener,
      boolean allowNewResourceName, String message, boolean showClosedProjects,
      int heightHint, String[] allowedResourceExtensions,
      IContainer inputContainer )
  {
    super( parent, SWT.NONE );
    m_listener = listener;
    m_allowNewResourceName = allowNewResourceName;
    m_showClosedProjects = showClosedProjects;
    m_allowedResourceExtensions = allowedResourceExtensions;
    m_inputContainer = inputContainer;
    if( message != null )
      createContents( message, heightHint );
    else if( m_allowNewResourceName )
      createContents( DEFAULT_MSG_NEW_ALLOWED, heightHint );
    else
      createContents( DEFAULT_MSG_SELECT_ONLY, heightHint );
  }

  public void resourceSelectionChanged( IResource resource )
  {
    selectedResource = resource;

    if( m_allowNewResourceName )
    {
      if( resource == null )
        resourceNameField.setText( "" );//$NON-NLS-1$
      else
        resourceNameField.setText( resource.getFullPath().makeRelative()
            .toString() );
    }

    // fire an event so the parent can update its controls
    if( m_listener != null )
    {
      Event changeEvent = new Event();
      changeEvent.type = SWT.Selection;
      changeEvent.widget = this;
      m_listener.handleEvent( changeEvent );
    }
  }

  /**
   * Creates the contents of the composite.
   */
  public void createContents( String message )
  {
    createContents( message, SIZING_SELECTION_PANE_HEIGHT );
  }

  /**
   * Creates the contents of the composite.
   * 
   * @param heightHint height hint for the drill down composite
   */
  public void createContents( String message, int heightHint )
  {
    GridLayout layout = new GridLayout();
    layout.marginWidth = 0;
    setLayout( layout );
    setLayoutData( new GridData( GridData.FILL_BOTH ) );

    Label label = new Label( this, SWT.WRAP );
    label.setText( message );
    label.setFont( this.getFont() );

    if( m_allowNewResourceName )
    {
      resourceNameField = new Text( this, SWT.SINGLE | SWT.BORDER );
      resourceNameField
          .setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
      // TODO: add listener to control user input
      //resourceNameField.addListener( SWT.DefaultSelection, m_listener );
      resourceNameField.setFont( this.getFont() );
    }
    else
    {
      // filler...
      new Label( this, SWT.NONE );
    }

    createTreeViewer( heightHint );
    Dialog.applyDialogFont( this );
  }

  protected void createTreeViewer( int heightHint )
  {
    // Create drill down.
    DrillDownComposite drillDown = new DrillDownComposite( this, SWT.BORDER );
    GridData spec = new GridData( GridData.VERTICAL_ALIGN_FILL
        | GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL
        | GridData.GRAB_VERTICAL );
    spec.widthHint = SIZING_SELECTION_PANE_WIDTH;
    spec.heightHint = heightHint;
    drillDown.setLayoutData( spec );

    // Create tree viewer inside drill down.
    treeViewer = new TreeViewer( drillDown, SWT.NONE );
    drillDown.setChildTree( treeViewer );
    ResourceContentProvider cp = new ResourceContentProvider(
        m_allowedResourceExtensions );
    cp.showClosedProjects( m_showClosedProjects );
    treeViewer.setContentProvider( cp );
    treeViewer.setLabelProvider( WorkbenchLabelProvider
        .getDecoratingWorkbenchLabelProvider() );
    treeViewer.setSorter( new ViewerSorter() );
    treeViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( SelectionChangedEvent event )
      {
        IStructuredSelection selection = (IStructuredSelection)event
            .getSelection();
        resourceSelectionChanged( (IResource)selection.getFirstElement() ); // allow
        // null
      }
    } );
    treeViewer.addDoubleClickListener( new IDoubleClickListener()
    {
      public void doubleClick( DoubleClickEvent event )
      {
        ISelection selection = event.getSelection();
        if( selection instanceof IStructuredSelection )
        {
          Object item = ( (IStructuredSelection)selection ).getFirstElement();
          if( treeViewer.getExpandedState( item ) )
            treeViewer.collapseToLevel( item, 1 );
          else
            treeViewer.expandToLevel( item, 1 );
        }
      }
    } );

    // This has to be done after the viewer has been laid out
    //treeViewer.setInput( ResourcesPlugin.getWorkspace() );
    treeViewer.setInput( m_inputContainer );
  }

  public IPath getResourceFullPath()
  {
    IPath resourcePath = null;
    if( m_allowNewResourceName )
    {
      String pathName = resourceNameField.getText();
      if( pathName == null || pathName.length() < 1 )
      {
        // nothing
      }
      else
      {
        //The user may not have made this absolute so do it for them
        resourcePath = ( new Path( pathName ) ).makeAbsolute();
      }
    }
    else
    {
      if( selectedResource == null )
      {
        // nothing
      }
      else
        resourcePath = selectedResource.getFullPath();
    }
    return resourcePath;
  }

  /**
   * Gives focus to one of the widgets in the group, as determined by the group.
   */
  public void setInitialFocus()
  {
    if( m_allowNewResourceName )
      resourceNameField.setFocus();
    else
      treeViewer.getTree().setFocus();
  }

  /**
   * Sets the selected existing container.
   */
  public void setSelectedResource( IResource resource )
  {
    selectedResource = resource;

    //expand to and select the specified container
    List itemsToExpand = new ArrayList();
    IContainer parent = resource.getParent();
    while( parent != null )
    {
      itemsToExpand.add( 0, parent );
      parent = parent.getParent();
    }
    treeViewer.setExpandedElements( itemsToExpand.toArray() );
    treeViewer.setSelection( new StructuredSelection( resource ), true );
  }

  public IResource getSelectedResource()
  {
    return selectedResource;
  }

}