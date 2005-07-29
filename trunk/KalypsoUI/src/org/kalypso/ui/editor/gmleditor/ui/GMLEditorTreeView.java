package org.kalypso.ui.editor.gmleditor.ui;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.IPostSelectionProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;
import org.kalypso.contribs.eclipse.jface.viewers.SelectionProviderAdapter;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.CommandableFeatureSelection;
import org.kalypso.ui.editor.gmleditor.util.Clipboard;
import org.kalypso.ui.editor.gmleditor.util.GMLReader;
import org.kalypso.ui.editor.gmleditor.util.actions.AddFeatureAction;
import org.kalypso.ui.editor.gmleditor.util.actions.AddLinkAction;
import org.kalypso.ui.editor.gmleditor.util.actions.CopyFeatureAction;
import org.kalypso.ui.editor.gmleditor.util.actions.EditFeatureAction;
import org.kalypso.ui.editor.gmleditor.util.actions.PasteFeatureAction;
import org.kalypso.ui.editor.gmleditor.util.command.DeleteFeatureCommand;
import org.kalypso.ui.editor.gmleditor.util.command.MoveFeatureCommand;
import org.kalypso.ui.editor.gmleditor.util.model.FeatureElement;
import org.kalypso.ui.editor.gmleditor.util.model.GMLDocumentEvent;
import org.kalypso.ui.editor.gmleditor.util.model.IGMLDocumentListener;
import org.kalypso.ui.editor.gmleditor.util.model.IModel;
import org.kalypso.ui.editor.gmleditor.util.model.LinkedFeatureElement;
import org.kalypso.ui.editor.gmleditor.util.model.Model;
import org.kalypso.ui.editor.gmleditor.util.model.PropertyElement;
import org.kalypso.ui.editor.gmleditor.util.model.visitors.FindDataElementsVisitor;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;
import org.kalypsodeegree.model.feature.event.ModellEventProvider;
import org.kalypsodeegree.model.feature.event.ModellEventProviderAdapter;

/**
 * Is among others a {@link org.eclipse.jface.viewers.IPostSelectionProvider}. The returned selection consists of
 * {@link org.kalypsodeegree.model.feature.Feature}and
 * {@link org.kalypsodeegree.model.feature.FeatureAssociationTypeProperty}objects. Changing the selection from outside
 * is not supported (todo).
 * 
 * @author <verschiedene>
 */
public class GMLEditorTreeView extends SelectionProviderAdapter implements IGMLDocumentListener, ModellEventListener,
    ModellEventProvider, IPostSelectionProvider
{
  private final static int DEFAULT_EXPANSION_LEVEL = 3;

  protected TreeViewer m_treeViewer;

  protected GMLEditorLabelProvider m_labelProvider;

  protected FeatureElement m_root;

  protected CommandableWorkspace m_workspace = null;

  protected final Composite m_composite;

  protected Action deleteFeatureAction = null;

  protected GMLReader m_reader = null;

  protected Action moveFeatureUpAction = null;

  protected Action moveFeatureDownAction = null;

  protected Action addFeatureActions[] = null;

  protected Action addLinkActions[] = null;

  protected Action copyFeatureAction = null;

  protected Action editFeatureAction = null;

  protected Action pasteFeatureAction = null;

  protected Clipboard clipboard = new Clipboard();

  private final ModellEventProviderAdapter myEventProvider = new ModellEventProviderAdapter();

  public GMLEditorTreeView( final Composite composite )
  {
    m_composite = composite;

    final GridLayout layout = new GridLayout();
    layout.numColumns = 1;
    layout.verticalSpacing = 2;
    layout.marginWidth = 0;
    layout.marginHeight = 2;
    m_composite.setLayout( layout );

    m_treeViewer = new TreeViewer( m_composite );
    m_treeViewer.setContentProvider( new GMLEditorContentProvider() );
    m_labelProvider = new GMLEditorLabelProvider();
    m_treeViewer.setLabelProvider( m_labelProvider );
    m_treeViewer.setUseHashlookup( true );

    final GridData layoutData = new GridData();
    layoutData.grabExcessHorizontalSpace = true;
    layoutData.grabExcessVerticalSpace = true;
    layoutData.horizontalAlignment = GridData.FILL;
    layoutData.verticalAlignment = GridData.FILL;
    m_treeViewer.getControl().setLayoutData( layoutData );

    hookListeners();
    createActions();
    createMenu( m_treeViewer.getControl() );
  }

  public void setGmlReader( final GMLReader reader )
  {
    if( m_reader != null )
      m_reader.removeGMLDocuentListener( this );

    m_reader = reader;

    if( m_reader != null )
    {
      m_reader.addGMLDocumentListener( this );
      onChange( new GMLDocumentEvent( GMLReader.getGMLDocument( getWorkspace() ), m_reader.getGMLWorkspace() ) );
    }
  }

  protected void createActions()
  {
    deleteFeatureAction = new Action( "L�schen" )
    {
      public void run()
      {
        if( m_treeViewer.getSelection().isEmpty() )
          return;
        IStructuredSelection selection = (IStructuredSelection)m_treeViewer.getSelection();
        for( Iterator iterator = selection.iterator(); iterator.hasNext(); )
        {
          Model model = (Model)iterator.next();

          if( model instanceof FeatureElement || model instanceof LinkedFeatureElement )
          {
            Object childItem = null;
            Feature parentFeature = null;

            if( model instanceof FeatureElement )
            {
              childItem = ( (FeatureElement)model ).getFeature();
              parentFeature = ( (FeatureElement)model.getParent().getParent() ).getFeature();
            }
            else
            {
              childItem = ( (LinkedFeatureElement)model ).getName();
              parentFeature = ( (FeatureElement)model.getParent().getParent() ).getFeature();
            }
            DeleteFeatureCommand command = new DeleteFeatureCommand( m_workspace, parentFeature,
                ( (PropertyElement)model.getParent() ).getProperty().getName(), childItem );
            try
            {
              m_workspace.postCommand( command );
              //m_gmlEditor.postCommand(command, null);
              return;
            }
            catch( Exception e )
            {
              e.printStackTrace();
            }
          }
        }
      }
    };

    moveFeatureUpAction = new Action( "Nach oben verschieben" )
    {
      public void run()
      {
        if( m_treeViewer.getSelection().isEmpty() )
          return;
        IStructuredSelection selection = (IStructuredSelection)m_treeViewer.getSelection();
        for( Iterator iterator = selection.iterator(); iterator.hasNext(); )
        {
          Model model = (Model)iterator.next();

          if( model instanceof FeatureElement || model instanceof LinkedFeatureElement )
          {
            Object childItem = null;
            Feature parentFeature = null;

            if( model instanceof FeatureElement )
            {
              childItem = ( (FeatureElement)model ).getFeature();
              parentFeature = ( (FeatureElement)model.getParent().getParent() ).getFeature();
            }
            else
            {
              childItem = ( (LinkedFeatureElement)model ).getName();
              parentFeature = ( (FeatureElement)model.getParent().getParent() ).getFeature();
            }

            MoveFeatureCommand command = new MoveFeatureCommand( m_workspace, parentFeature, ( (PropertyElement)model
                .getParent() ).getProperty().getName(), childItem, MoveFeatureCommand.UP );
            try
            {
              m_workspace.postCommand( command );
              return;
            }
            catch( Exception e )
            {
              e.printStackTrace();
            }
          }
        }
      }
    };

    moveFeatureDownAction = new Action( "Nach unten verschieben" )
    {
      public void run()
      {
        if( m_treeViewer.getSelection().isEmpty() )
          return;
        IStructuredSelection selection = (IStructuredSelection)m_treeViewer.getSelection();
        for( Iterator iterator = selection.iterator(); iterator.hasNext(); )
        {
          Model model = (Model)iterator.next();

          if( model instanceof FeatureElement || model instanceof LinkedFeatureElement )
          {
            Object childItem = null;
            Feature parentFeature = null;

            if( model instanceof FeatureElement )
            {
              childItem = ( (FeatureElement)model ).getFeature();
              parentFeature = ( (FeatureElement)model.getParent().getParent() ).getFeature();
            }
            else
            {
              childItem = ( (LinkedFeatureElement)model ).getName();
              parentFeature = ( (FeatureElement)model.getParent().getParent() ).getFeature();
            }

            MoveFeatureCommand command = new MoveFeatureCommand( m_workspace, parentFeature, ( (PropertyElement)model
                .getParent() ).getProperty().getName(), childItem, MoveFeatureCommand.DOWN );

            try
            {
              m_workspace.postCommand( command );
              return;
            }
            catch( Exception e )
            {
              e.printStackTrace();
            }
          }
        }
      }
    };
  }

  protected void createMenu( final Control control )
  {
    final MenuManager rootMenuManager = new MenuManager( "#PopUp" );
    rootMenuManager.setRemoveAllWhenShown( true );
    rootMenuManager.addMenuListener( new IMenuListener()
    {
      public void menuAboutToShow( IMenuManager mgr )
      {
        if( editFeatureAction != null )
          mgr.add( editFeatureAction );
        mgr.add( deleteFeatureAction );
        mgr.add( moveFeatureUpAction );
        mgr.add( moveFeatureDownAction );
        if( addFeatureActions != null )
        {
          for( int i = 0; i < addFeatureActions.length; i++ )
            mgr.add( addFeatureActions[i] );
        }
        if( addLinkActions != null )
        {
          for( int i = 0; i < addLinkActions.length; i++ )
            mgr.add( addLinkActions[i] );
        }
        if( copyFeatureAction != null )
          mgr.add( copyFeatureAction );
        if( pasteFeatureAction != null )
          mgr.add( pasteFeatureAction );
      }
    } );
    final Menu menu = rootMenuManager.createContextMenu( control );
    control.setMenu( menu );
  }

  protected void hookListeners()
  {
    m_treeViewer.addDoubleClickListener( new IDoubleClickListener()
    {
      public void doubleClick( final DoubleClickEvent event )
      {
        if( event.getSelection() instanceof IStructuredSelection )
        {
          final IStructuredSelection selection = (IStructuredSelection)event.getSelection();
          final Object obj = selection.getFirstElement();
          if( obj instanceof LinkedFeatureElement )
          {
            final StructuredSelection ss = new StructuredSelection( findFeatureElement( (LinkedFeatureElement)obj ) );
            m_treeViewer.setSelection( ss, true );
          }
        }
      }
    } );

    m_treeViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( final SelectionChangedEvent event )
      {
        onTreeSelectionChanged( event );
      }
    } );

    m_treeViewer.addPostSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( final SelectionChangedEvent event )
      {
        firePostSelectionChanged();
      }
    } );
  }

  protected void onTreeSelectionChanged( final SelectionChangedEvent event )
  {
    // first inform my listeners
    final IStructuredSelection selection = (IStructuredSelection)event.getSelection();
    final ISelection unwrappedSelection = new CommandableFeatureSelection( getWorkspace(), new StructuredSelection(
        getDataFromElements( selection.toArray() ) ), null, null);
    setSelection( unwrappedSelection );

    // now update actions

    final Object obj = selection.getFirstElement();
    if( obj instanceof PropertyElement )
    {
      deleteFeatureAction.setEnabled( false );
      moveFeatureUpAction.setEnabled( false );
      moveFeatureDownAction.setEnabled( false );

      FeatureType types[] = ( (PropertyElement)obj ).getProperty().getAssociationFeatureTypes();
      Feature parentFeature = ( (FeatureElement)( (PropertyElement)obj ).getParent() ).getFeature();

      addFeatureActions = new Action[types.length];
      addLinkActions = new Action[types.length];
      for( int i = 0; i < types.length; i++ )
      {
        addFeatureActions[i] = new AddFeatureAction( types[i], m_workspace, parentFeature, ( (PropertyElement)obj )
            .getProperty().getName(), 0, m_composite.getShell() );
        addLinkActions[i] = new AddLinkAction( types[i], m_workspace, parentFeature, ( (PropertyElement)obj )
            .getProperty().getName(), 0, m_composite.getShell() );
      }

      pasteFeatureAction = new PasteFeatureAction( m_workspace, parentFeature, ( (PropertyElement)obj ).getProperty()
          .getName(), clipboard );

      if( clipboard.getClipboardFeature() != null )
        pasteFeatureAction.setEnabled( true );
      else
        pasteFeatureAction.setEnabled( false );

      if( copyFeatureAction != null )
        copyFeatureAction.setEnabled( false );
      if( editFeatureAction != null )
        editFeatureAction.setEnabled( false );
    }
    else
    {
      deleteFeatureAction.setEnabled( true );
      moveFeatureUpAction.setEnabled( true );
      moveFeatureDownAction.setEnabled( true );
      addFeatureActions = null;
      addLinkActions = null;
      if( copyFeatureAction != null )
        copyFeatureAction.setEnabled( false );
      if( editFeatureAction != null )
        editFeatureAction.setEnabled( false );
      if( obj instanceof FeatureElement )
      {
        editFeatureAction = new EditFeatureAction( m_workspace, m_reader,
            m_composite.getShell() );
        editFeatureAction.setEnabled( true );
        copyFeatureAction = new CopyFeatureAction( ( (FeatureElement)obj ).getFeature(), m_workspace, clipboard );
        copyFeatureAction.setEnabled( true );
      }
      if( pasteFeatureAction != null )
        pasteFeatureAction.setEnabled( false );
    }
  }

  protected FeatureElement findFeatureElement( final LinkedFeatureElement lfe )
  {
    return findFeatureElement( ( (FeatureElement)m_root.getChildren()[0] ), lfe.getName() );
  }

  private FeatureElement findFeatureElement( final FeatureElement root, final String searchString )
  {
    if( root.getName().equals( searchString ) )
      return root;

    Object[] pe = root.getChildren(); // PropertyElement[]
    for( int i = 0; i < pe.length; i++ )
    {
      Object[] model = ( (PropertyElement)pe[i] ).getChildren();
      for( int j = 0; j < model.length; j++ )
      {
        if( model[j] instanceof FeatureElement )
        {
          FeatureElement fe = findFeatureElement( (FeatureElement)model[j], searchString );
          if( fe != null )
            return fe;
        }
      }
    }
    return null;
  }

  /**
   * @see org.kalypso.ui.editor.gmleditor.util.model.IGMLDocumentListener#onChange(org.kalypso.ui.editor.gmleditor.util.model.GMLDocumentEvent)
   */
  public void onChange( GMLDocumentEvent event )
  {
    m_root = event.getGmlDocument();
    if( m_workspace != null )
      m_workspace.removeModellListener( this );
    m_workspace = event.getWorkspace();
    if( m_workspace == null )
      return;

    m_workspace.addModellListener( this );

    if( m_composite == null || m_composite.isDisposed() )
      return;

    m_composite.getDisplay().asyncExec( new Runnable()
    {
      public void run()
      {
        m_treeViewer.getTree().setVisible( false );
        m_treeViewer.setInput( m_root );
        // need to expand in order to load all elements into the treeviewers
        // cache
        m_treeViewer.expandAll();
        m_treeViewer.collapseAll();

        m_treeViewer.expandToLevel( DEFAULT_EXPANSION_LEVEL );
        m_treeViewer.getTree().setVisible( true );

        fireModellEvent( new ModellEvent( GMLEditorTreeView.this, ModellEvent.FULL_CHANGE ) );
      }
    } );

  }

  public void dispose()
  {
    m_composite.dispose();
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    if( modellEvent.getEventSource() instanceof CommandableWorkspace )
    {
      if( !m_composite.isDisposed() )
        m_composite.getDisplay().asyncExec( new Runnable()
        {
          public void run()
          {
            // das selektierte Feature merken und dann wieder anzeigen
            final Object[] expandedElements = m_treeViewer.getExpandedElements();
            final Object[] expandedData = getDataFromElements( expandedElements );

            final IStructuredSelection selection = (IStructuredSelection)m_treeViewer.getSelection();
            final Object[] selecteddata = getDataFromElements( selection.toArray() );

            final FeatureElement root = GMLReader.getGMLDocument( (CommandableWorkspace)modellEvent.getEventSource() );
            if( root != m_root )
            {
              m_root = root;
              m_treeViewer.setInput( root );

              m_treeViewer.setExpandedElements( findDataElements( expandedData ) );
              m_treeViewer.setSelection( new StructuredSelection( findDataElements( selecteddata ) ) );
            }

            fireModellEvent( modellEvent );
          }
        } );
    }
  }

  protected static Object[] getDataFromElements( final Object[] elements )
  {
    final Collection data = new LinkedList();
    for( int i = 0; i < elements.length; i++ )
    {
      final Object object = elements[i];
      if( object instanceof FeatureElement )
        data.add( ( (FeatureElement)object ).getFeature() );
      else if( object instanceof PropertyElement )
        data.add( ( (PropertyElement)object ).getProperty() );
    }

    return data.toArray();
  }

  protected IModel[] findDataElements( final Object[] data )
  {
    final FindDataElementsVisitor visitor = new FindDataElementsVisitor( data );
    m_root.accept( visitor );
    return visitor.getResults();
  }

  public void saveData( final IProgressMonitor monitor ) throws CoreException
  {
    m_reader.saveFeatures( monitor );
  }

  public CommandableWorkspace getWorkspace()
  {
    return m_workspace;
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventProvider#addModellListener(org.kalypsodeegree.model.feature.event.ModellEventListener)
   */
  public void addModellListener( ModellEventListener listener )
  {
    myEventProvider.addModellListener( listener );
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventProvider#removeModellListener(org.kalypsodeegree.model.feature.event.ModellEventListener)
   */
  public void removeModellListener( ModellEventListener listener )
  {
    myEventProvider.removeModellListener( listener );
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventProvider#fireModellEvent(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void fireModellEvent( ModellEvent event )
  {
    myEventProvider.fireModellEvent( event );
  }

  public TreeViewer getTreeViewer()
  {
    return m_treeViewer;
  }
}