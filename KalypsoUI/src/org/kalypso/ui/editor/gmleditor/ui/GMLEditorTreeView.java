package org.kalypso.ui.editor.gmleditor.ui;

import java.util.Iterator;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.event.ModellEvent;
import org.deegree.model.feature.event.ModellEventListener;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
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
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.editor.gmleditor.util.Clipboard;
import org.kalypso.ui.editor.gmleditor.util.GMLReader;
import org.kalypso.ui.editor.gmleditor.util.actions.AddFeatureAction;
import org.kalypso.ui.editor.gmleditor.util.actions.CopyFeatureAction;
import org.kalypso.ui.editor.gmleditor.util.actions.PasteFeatureAction;
import org.kalypso.ui.editor.gmleditor.util.command.DeleteFeatureCommand;
import org.kalypso.ui.editor.gmleditor.util.command.MoveFeatureCommand;
import org.kalypso.ui.editor.gmleditor.util.model.FeatureElement;
import org.kalypso.ui.editor.gmleditor.util.model.GMLDocumentEvent;
import org.kalypso.ui.editor.gmleditor.util.model.IGMLDocumentListener;
import org.kalypso.ui.editor.gmleditor.util.model.LinkedFeatureElement;
import org.kalypso.ui.editor.gmleditor.util.model.Model;
import org.kalypso.ui.editor.gmleditor.util.model.PropertyElement;

public class GMLEditorTreeView implements IGMLDocumentListener, ModellEventListener
{
  protected TreeViewer treeViewer;

  protected GMLEditorLabelProvider labelProvider;

  protected FeatureElement root;

  protected CommandableWorkspace workspace = null;

  protected Composite composite = null;

  private final static int DEFAULT_EXPANSION_LEVEL = 3;

  protected Action deleteFeatureAction = null;

  protected GMLReader reader = null;

  protected Action moveFeatureUpAction = null;

  protected Action moveFeatureDownAction = null;
  
  protected Action addFeatureActions[] = null;
  
  protected Action copyFeatureAction = null;
  
  protected Action pasteFeatureAction = null;
  
  protected Clipboard clipboard = null;

  /**
   * The constructor.
   * 
   * @param m_composite
   */
  public GMLEditorTreeView( Composite m_composite )
  {
    clipboard = new Clipboard();
    
    composite = m_composite;
    GridLayout layout = new GridLayout();
    layout.numColumns = 1;
    layout.verticalSpacing = 2;
    layout.marginWidth = 0;
    layout.marginHeight = 2;
    composite.setLayout( layout );

    treeViewer = new TreeViewer( composite );
    treeViewer.setContentProvider( new GMLEditorContentProvider() );
    labelProvider = new GMLEditorLabelProvider();
    treeViewer.setLabelProvider( labelProvider );
    treeViewer.setUseHashlookup( true );

    GridData layoutData = new GridData();
    layoutData = new GridData();
    layoutData.grabExcessHorizontalSpace = true;
    layoutData.grabExcessVerticalSpace = true;
    layoutData.horizontalAlignment = GridData.FILL;
    layoutData.verticalAlignment = GridData.FILL;
    treeViewer.getControl().setLayoutData( layoutData );    
    
    hookListeners();
    createActions();
    createMenu( treeViewer.getControl() );
  }

  public void setGmlReader( GMLReader m_reader )
  {
    reader = m_reader;
    reader.addGMLDocumentListener( this );
    reader.load();
  }

  protected void createActions()
  {
    deleteFeatureAction = new Action( "Delete" )
    {
      public void run()
      {
        if( treeViewer.getSelection().isEmpty() )
          return;
        IStructuredSelection selection = (IStructuredSelection)treeViewer.getSelection();
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
            DeleteFeatureCommand command = new DeleteFeatureCommand( workspace, parentFeature,
                ( (PropertyElement)model.getParent() ).getProperty().getName(), childItem );
            try
            {
              workspace.postCommand( command );
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

    moveFeatureUpAction = new Action( "MoveUp" )
    {
      public void run()
      {
        if( treeViewer.getSelection().isEmpty() )
          return;
        IStructuredSelection selection = (IStructuredSelection)treeViewer.getSelection();
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

            MoveFeatureCommand command = new MoveFeatureCommand( workspace, parentFeature,
                ( (PropertyElement)model.getParent() ).getProperty().getName(), childItem,
                MoveFeatureCommand.UP );
            try
            {
              workspace.postCommand( command );
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

    moveFeatureDownAction = new Action( "MoveDown" )
    {
      public void run()
      {
        if( treeViewer.getSelection().isEmpty() )
          return;
        IStructuredSelection selection = (IStructuredSelection)treeViewer.getSelection();
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

            MoveFeatureCommand command = new MoveFeatureCommand( workspace, parentFeature,
                ( (PropertyElement)model.getParent() ).getProperty().getName(), childItem,
                MoveFeatureCommand.DOWN );

            try
            {
              workspace.postCommand( command );
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

  protected void createMenu( Control control )
  {
    MenuManager rootMenuManager = new MenuManager( "#PopUp" );
    rootMenuManager.setRemoveAllWhenShown( true );
    rootMenuManager.addMenuListener( new IMenuListener()
    {
      public void menuAboutToShow( IMenuManager mgr )
      {
        mgr.add( deleteFeatureAction );
        mgr.add( moveFeatureUpAction );
        mgr.add( moveFeatureDownAction );
        if(addFeatureActions != null)
        {
          for(int i=0; i<addFeatureActions.length; i++)
            mgr.add(addFeatureActions[i]);
        }
        if(copyFeatureAction != null)
          mgr.add(copyFeatureAction);
        if(pasteFeatureAction != null)
          mgr.add(pasteFeatureAction);
      }
    } );
    Menu menu = rootMenuManager.createContextMenu( control );
    control.setMenu( menu );
  }

  protected void hookListeners()
  {
    //    treeViewer.addSelectionChangedListener( new ISelectionChangedListener()
    treeViewer.addDoubleClickListener( new IDoubleClickListener()
    {
      public void doubleClick( DoubleClickEvent event )
      {
        if( event.getSelection() instanceof IStructuredSelection )
        {
          IStructuredSelection selection = (IStructuredSelection)event.getSelection();
          Iterator iterator = selection.iterator();
          Object obj = null;
          if( iterator.hasNext() )
            obj = iterator.next();

          if( obj instanceof LinkedFeatureElement )
          {
            StructuredSelection ss = new StructuredSelection(
                findFeatureElement( (LinkedFeatureElement)obj ) );
            treeViewer.setSelection( ss, true );
          }
        }
      }
    } );

    treeViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( SelectionChangedEvent event )
      {
        IStructuredSelection selection = (IStructuredSelection)event.getSelection();
        Iterator iterator = selection.iterator();
        Object obj = null;
        if( iterator.hasNext() )
          obj = iterator.next();
        if( obj instanceof PropertyElement )
        {
          deleteFeatureAction.setEnabled( false );
          moveFeatureUpAction.setEnabled( false );
          moveFeatureDownAction.setEnabled( false );
          
          FeatureType types[] = ((PropertyElement) obj).getProperty().getAssociationFeatureTypes();
          Feature parentFeature = ((FeatureElement)((PropertyElement) obj).getParent()).getFeature();
          
          addFeatureActions = new Action[types.length];                              
          for(int i=0; i<types.length; i++)
          {            
            addFeatureActions[i] = new AddFeatureAction(types[i], workspace, parentFeature, ((PropertyElement) obj).getProperty().getName(), 0, composite.getShell());        
          }      
          
          pasteFeatureAction = new PasteFeatureAction(workspace, parentFeature,((PropertyElement) obj).getProperty().getName(),clipboard);
          
          if(clipboard.getClipboardFeature() != null)
          {                       
            pasteFeatureAction.setEnabled(true);
          }
          else
          {            
            pasteFeatureAction.setEnabled(false);
          }
          
          if(copyFeatureAction != null)
            copyFeatureAction.setEnabled(false);          
        }
        else
        {
          deleteFeatureAction.setEnabled( true );
          moveFeatureUpAction.setEnabled( true );
          moveFeatureDownAction.setEnabled( true ); 
          addFeatureActions = null;
          if(copyFeatureAction != null)
            copyFeatureAction.setEnabled(false);
          
          if(obj instanceof FeatureElement)
          {
            copyFeatureAction = new CopyFeatureAction(((FeatureElement)obj).getFeature(),workspace, clipboard);
            copyFeatureAction.setEnabled(true);
          }
          if(pasteFeatureAction != null)
           pasteFeatureAction.setEnabled(false);                    
        }
      }
    } );
  }

  protected FeatureElement findFeatureElement( LinkedFeatureElement lfe )
  {
    return findFeatureElement( ( (FeatureElement)root.getElements()[0] ), lfe.getName() );
  }

  FeatureElement findFeatureElement( FeatureElement m_root, String searchString )
  {
    if( m_root.getName().equals( searchString ) )
      return m_root;
    Object[] pe = m_root.getElements(); // PropertyElement[]
    for( int i = 0; i < pe.length; i++ )
    {
      Object[] model = ( (PropertyElement)pe[i] ).getElements();
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

  /*
   * @see IWorkbenchPart#setFocus()
   */
  public void setFocus()
  {/**/}

  /**
   * @see org.kalypso.ui.editor.gmleditor.util.model.IGMLDocumentListener#onChange(org.kalypso.ui.editor.gmleditor.util.model.GMLDocumentEvent)
   */
  public void onChange( GMLDocumentEvent event )
  {
    root = event.getGmlDocument();
    if( workspace != null )
    {
      workspace.removeModellListener( this );
    }
    workspace = event.getWorkspace();
    workspace.addModellListener( this );

    composite.getDisplay().asyncExec( new Runnable()
    {
      public void run()
      {
        treeViewer.getTree().setVisible( false );
        treeViewer.setInput( root );
        // need to expand in order to load all elements into the treeviewers
        // cache
        treeViewer.expandAll();
        treeViewer.collapseAll();
        treeViewer.expandToLevel( DEFAULT_EXPANSION_LEVEL );
        treeViewer.getTree().setVisible( true );
      }
    } );
  }

  public void dispose()
  {
    composite.dispose();
  }

  /**
   * @see org.deegree.model.feature.event.ModellEventListener#onModellChange(org.deegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    if( modellEvent.getEventSource() instanceof CommandableWorkspace )
    {
      composite.getDisplay().asyncExec( new Runnable()
      {
        public void run()
        {
          treeViewer.setInput( reader.getGMLDocument( (CommandableWorkspace)modellEvent
              .getEventSource() ) );
        }
      } );
    }
    System.out.println( "model changed " + modellEvent.getType() );
  }
}