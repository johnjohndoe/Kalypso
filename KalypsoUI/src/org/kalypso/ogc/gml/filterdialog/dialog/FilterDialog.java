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
import java.util.ArrayList;

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
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.ui.IPartService;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.IWorkbenchPartSite;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.SaveAsDialog;
import org.kalypso.contribs.eclipse.ui.dialogs.KalypsoResourceSelectionDialog;
import org.kalypso.ogc.gml.filterdialog.model.FilterReader;
import org.kalypso.ogc.gml.filterdialog.model.FilterRootElement;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.editor.styleeditor.MessageBundle;
import org.kalypsodeegree.filterencoding.Filter;
import org.kalypsodeegree.filterencoding.Operation;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;
import org.kalypsodeegree_impl.filterencoding.FeatureFilter;

/**
 * @author kuepfer
 */
public class FilterDialog extends Dialog implements ModellEventListener
{
  protected final FeatureType m_featureType;

  protected FilterRootElement m_root;

  private Group m_treeGroup;

  protected Group m_propGroup;

  protected Composite m_main;

  protected TreeViewer m_viewer2;

  protected final FilterLabelProvider m_labelProvider = new FilterLabelProvider();

  protected final FilterContentProvider m_contentProvider = new FilterContentProvider();

  private ToolBar m_toolBar;

  private FilterCompositeFactory m_filterCompositeFactory = null;

  public FilterDialog( Shell parent, FeatureType featureType, Filter root )
  {
    super( parent );
    m_featureType = featureType;
    m_root = new FilterRootElement();
    if( root != null )
      m_root.addChild( root );
    m_filterCompositeFactory = FilterCompositeFactory.getInstance( null );
    m_filterCompositeFactory.addModellListener( this );
    setShellStyle( getShellStyle() | SWT.MAX );
  }

  protected Control createDialogArea( Composite parent )
  {
    m_main = (Composite)super.createDialogArea( parent );
    m_main.setLayout( new GridLayout( 2, true ) );
    //    applyDialogFont( m_main );
    //tree-group
    m_treeGroup = new Group( m_main, SWT.FILL );
    m_treeGroup.setText( "Filter für " + m_featureType.getName() );
    m_treeGroup.setLayoutData( new GridData( 280, 200 ) );
    m_treeGroup.setLayout( new GridLayout() );
    //tree-viewer
    m_viewer2 = new TreeViewer( m_treeGroup, SWT.FILL )
    {
      public ISelection getSelection()
      {
        return new TreeSelection( (IStructuredSelection)super.getSelection() )
        {
          public void contentChanged()
          {
            refresh( true );
            collapseAll();
            expandAll();
          }

          public void structureChanged()
          {
            refresh( true );
            collapseAll();
            expandAll();
          }

          public Object getModel()
          {
            return m_root;
          }
        };
      }
    };
    m_viewer2.setLabelProvider( m_labelProvider );
    m_viewer2.setContentProvider( m_contentProvider );
    m_viewer2.getControl().setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );
    m_viewer2.addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( SelectionChangedEvent event )
      {
        ISelection selection = event.getSelection();
        if( selection instanceof IStructuredSelection )
        {
          //erases the property group each time the selection changes
          if( m_propGroup != null && !m_propGroup.isDisposed() )
          {
            m_propGroup.dispose();
            m_propGroup = null;
          }
          m_propGroup = new Group( m_main, SWT.FILL );
          m_propGroup.setText( "Filter-Eigenschaften" );
          m_propGroup.setLayout( new GridLayout() );
          GridData data = new GridData( GridData.FILL_BOTH );
          data.heightHint = 150;
          data.widthHint = 250;
          data.grabExcessHorizontalSpace = true;
          m_propGroup.setLayoutData( data );
          m_propGroup.setLocation( 300, 5 );
          Object firstElement = ( (IStructuredSelection)selection ).getFirstElement();
          if( firstElement instanceof Operation )
          {
            Object oldValue = firstElement;
            FilterCompositeFactory.getInstance( null ).createFilterElementComposite( (Operation)firstElement,
                m_propGroup, m_featureType );
            //            m_root.firePropertyChange( FilterRootElement.OPERATION_ADDED, oldValue, firstElement );
          }
          else if( firstElement instanceof FeatureFilter )
          {
            FeatureFilter filter = (FeatureFilter)firstElement;
            m_propGroup.setText( "Eigenschaften-Feature Filter" );
            ArrayList featureIds = filter.getFeatureIds();
            Label featureFilterLabel = new Label( m_propGroup, SWT.NULL );
            featureFilterLabel.setText( "Feature-ID's: " );
            List idList = new List( m_propGroup, SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL );
            idList.setLayoutData( new GridData( 250, 80 ) );
            String[] items = (String[])featureIds.toArray( new String[featureIds.size()] );
            if( items.length > 0 )
              idList.setItems( items );
            else
              idList.add( "-EMPTY FEATURE ID LIST-" );
          }
          m_propGroup.pack();
        }
      }
    } );
    //property view
    m_viewer2.setInput( new Object[]
    { m_root } );
    m_viewer2.expandAll();
    //toolbar
    m_toolBar = new ToolBar( m_main, SWT.NULL | SWT.FLAT );
    ToolItem m_loadFilterItem = new ToolItem( m_toolBar, SWT.NONE );
    m_loadFilterItem.setToolTipText( "Filter aus dem Workspace laden" );
    m_loadFilterItem.setImage( ImageProvider.IMAGE_UTIL_IMPORT_WIZARD.createImage() );
    m_loadFilterItem.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
      {
        IWorkspace workspace = ResourcesPlugin.getWorkspace();
        KalypsoResourceSelectionDialog dialog2 = new KalypsoResourceSelectionDialog( getShell(), workspace.getRoot(),
            "Filter auswählen", new String[]
            { "xml" }, workspace.getRoot() );
        int open = dialog2.open();
        if( open == Window.OK )
        {
          Object[] result = dialog2.getResult();
          IPath path = (IPath)result[0];
          Filter filter = null;
          try
          {
            IFile file = workspace.getRoot().getFile( path );
            filter = FilterReader.readFilterFragment( file.getContents() );
          }
          catch( Exception e1 )
          {
            e1.printStackTrace();
          }
          m_root.addChild( filter );
          m_viewer2.setContentProvider( m_contentProvider );
          m_viewer2.setLabelProvider( m_labelProvider );
          m_viewer2.setInput( new Object[]
          { m_root } );
          m_viewer2.expandAll();
        }
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        widgetSelected( e );
      }
    } );
    ToolItem m_saveFilterItem = new ToolItem( m_toolBar, SWT.NONE );
    m_saveFilterItem.setToolTipText( "Filter speichern" );
    m_saveFilterItem.setImage( ImageProvider.IMAGE_STYLEEDITOR_SAVE.createImage() );
    m_saveFilterItem.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
      {
        IWorkspace workspace = ResourcesPlugin.getWorkspace();
        SaveAsDialog dialog = new SaveAsDialog( getShell() );
        int open = dialog.open();
        if( open == Window.OK )
        {
          IPath result = dialog.getResult();
          IFile file = workspace.getRoot().getFile( result );
          String xml = m_root.toXML().toString();
          try
          {
            file.create( new ByteArrayInputStream( xml.getBytes() ), true, new NullProgressMonitor() );
          }
          catch( CoreException e1 )
          {
            e1.printStackTrace();
          }
        }
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        widgetSelected( e );
      }
    } );
    createContextMenu();
    return m_main;
  }

  private void createContextMenu()
  {
    final MenuManager menuManager = new MenuManager();
    menuManager.setRemoveAllWhenShown( true );
    menuManager.addMenuListener( new IMenuListener()
    {
      public void menuAboutToShow( IMenuManager manager )
      {
        IStructuredSelection selection = (IStructuredSelection)m_viewer2.getSelection();
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

    //Register the context menu at the active workbench part
    IPartService workbenchPart = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getPartService();
    IWorkbenchPartSite site = workbenchPart.getActivePart().getSite();
    if( site != null )
    {
      final Menu menu = menuManager.createContextMenu( m_viewer2.getControl() );
      site.registerContextMenu( menuManager, m_viewer2 );
      m_viewer2.getControl().setMenu( menu );
    }
  }

  protected void okPressed()
  {
    super.okPressed();
  }

  protected void cancelPressed()
  {
    super.cancelPressed();
  }

  protected void configureShell( Shell shell )
  {
    super.configureShell( shell );
    shell.setText( MessageBundle.STYLE_EDITOR_FILTER );
    shell.setSize( 650, 350 );
  }

  protected Action m_action = new Action( "&Löschen", ImageProvider.IMAGE_STYLEEDITOR_REMOVE )
  {
    public void run()
    {
      IStructuredSelection selection = (IStructuredSelection)m_viewer2.getSelection();
      if( !selection.isEmpty() )
      {

        m_root.removeChild( selection.getFirstElement() );
        m_viewer2.refresh();
      }
    }
  };

  public Filter getFilter()
  {
    return m_root.getFilter();
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( ModellEvent modellEvent )
  {
    if( modellEvent.getEventSource().equals( m_filterCompositeFactory ) )
    {
      m_viewer2.refresh();
      m_viewer2.expandAll();

    }

  }
}
