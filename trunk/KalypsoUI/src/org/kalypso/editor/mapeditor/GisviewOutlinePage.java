package org.kalypso.editor.mapeditor;

import java.util.Iterator;
import java.util.List;

import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;
import org.kalypso.editor.mapeditor.commands.AddLayerAction;
import org.kalypso.editor.mapeditor.commands.MoveLayerAction;
import org.kalypso.editor.mapeditor.commands.RemoveLayerAction;
import org.kalypso.editor.mapeditor.commands.SetVisibleCommand;
import org.kalypso.plugin.ImageProvider;
import org.kalypso.xml.types.GisviewLayerType;

/**
 * OutlinePage für das MapView-Template
 * 
 * @author gernot
 */
public class GisviewOutlinePage implements IContentOutlinePage, ICheckStateListener,
    IGisviewEditorListener, IDoubleClickListener
{
  private MoveLayerAction m_moveOneDownAction = null;

  private MoveLayerAction m_moveOneUpAction = null;

  private MoveLayerAction m_moveDownAction = null;

  private MoveLayerAction m_moveUpAction = null;

  private RemoveLayerAction m_removeAction = null;

  private AddLayerAction m_addAction = null;

  private CheckboxTableViewer m_tableViewer = null;
  
  private final LayerlistLabelProvider m_mapViewLabelProvider = new LayerlistLabelProvider();
  
  private final LayerlistContentProvider m_mapViewContentProvider = new LayerlistContentProvider();

  private final GisMapEditor m_gisEditor;

  public GisviewOutlinePage( final GisMapEditor gisEditor )
  {
    m_gisEditor = gisEditor;

    m_gisEditor.addGisMapEditorListener( this );
  }

  /**
   * @see org.eclipse.ui.part.IPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( Composite parent )
  {
    if( m_tableViewer == null )
    {
      m_tableViewer = CheckboxTableViewer.newCheckList( parent, SWT.NONE );
      m_tableViewer.addCheckStateListener( this );
      m_tableViewer.addDoubleClickListener( this );
    }

    
    m_tableViewer.setLabelProvider( m_mapViewLabelProvider );
    m_tableViewer.setContentProvider( m_mapViewContentProvider );

    m_tableViewer.setInput( m_gisEditor );

    m_moveOneDownAction = new MoveLayerAction( "Move one down",
        ImageProvider.IMAGE_MAPVIEW_OUTLINE_DOWN, "Thema eins nach unten verschieben", 1, m_gisEditor, this, m_gisEditor );

    m_moveOneUpAction = new MoveLayerAction( "Move on up", ImageProvider.IMAGE_MAPVIEW_OUTLINE_UP,
        "Thema eins nach oben verschieben", -1, m_gisEditor, this, m_gisEditor );

    m_moveDownAction = new MoveLayerAction( "Move down", ImageProvider.IMAGE_MAPVIEW_OUTLINE_DOWN,
        "Thema nach unten verschieben", Integer.MAX_VALUE, m_gisEditor, this, m_gisEditor );

    m_moveUpAction = new MoveLayerAction( "Move up", ImageProvider.IMAGE_MAPVIEW_OUTLINE_UP,
        "Thema nach oben verschieben", Integer.MIN_VALUE, m_gisEditor, this, m_gisEditor );

    m_removeAction = new RemoveLayerAction( m_gisEditor, m_gisEditor, this );
    
    m_addAction = new AddLayerAction( m_gisEditor, m_gisEditor, this, m_gisEditor );

    onGisviewChanged();
  }

  /**
   * @see org.eclipse.ui.part.IPage#dispose()
   */
  public void dispose()
  {
    m_gisEditor.removeGisMapEditorListener( this );

    if( m_tableViewer != null )
    {
      m_tableViewer.removeCheckStateListener( this );
      m_tableViewer.removeDoubleClickListener( this );
    }

    if( m_moveOneDownAction != null )
      m_moveOneDownAction.dispose();

    if( m_moveOneUpAction != null )
      m_moveOneUpAction.dispose();

    if( m_moveDownAction != null )
      m_moveDownAction.dispose();

    if( m_moveUpAction != null )
      m_moveUpAction.dispose();

    if( m_removeAction != null )
      m_removeAction.dispose();
    
    m_mapViewContentProvider.dispose();
    m_mapViewLabelProvider.dispose();
  }

  /**
   * @see org.eclipse.ui.part.IPage#getControl()
   */
  public Control getControl()
  {
    return m_tableViewer.getControl();
  }

  /**
   * @see org.eclipse.ui.part.IPage#setActionBars(org.eclipse.ui.IActionBars)
   */
  public void setActionBars( IActionBars actionBars )
  {
    actionBars.setGlobalActionHandler( ActionFactory.UNDO.getId(), m_gisEditor.m_undoAction );
    actionBars.setGlobalActionHandler( ActionFactory.REDO.getId(), m_gisEditor.m_redoAction );

//    actionBars.getToolBarManager().add( m_addAction );
    m_addAction.getClass(); // unused

    actionBars.getToolBarManager().add( m_moveUpAction );
    actionBars.getToolBarManager().add( m_moveOneUpAction );
    actionBars.getToolBarManager().add( m_moveOneDownAction );
    actionBars.getToolBarManager().add( m_moveDownAction );

//    actionBars.getToolBarManager().add( m_removeAction );

    actionBars.updateActionBars();
  }

  /**
   * @see org.eclipse.ui.part.IPage#setFocus()
   */
  public void setFocus()
  {
    // nichts tun
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#addSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  public void addSelectionChangedListener( ISelectionChangedListener listener )
  {
    m_tableViewer.addSelectionChangedListener( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#getSelection()
   */
  public ISelection getSelection()
  {
    return m_tableViewer.getSelection();
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#removeSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  public void removeSelectionChangedListener( ISelectionChangedListener listener )
  {
    m_tableViewer.removeSelectionChangedListener( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#setSelection(org.eclipse.jface.viewers.ISelection)
   */
  public void setSelection( ISelection selection )
  {
    m_tableViewer.setSelection( selection );
  }

  /**
   * @see org.eclipse.jface.viewers.ICheckStateListener#checkStateChanged(org.eclipse.jface.viewers.CheckStateChangedEvent)
   */
  public void checkStateChanged( final CheckStateChangedEvent event )
  {
    m_gisEditor.postCommand( new SetVisibleCommand( (GisviewLayerType)event.getElement(), event.getChecked() ) );
  }

  public GisMapEditor getGisEditor()
  {
    return m_gisEditor;
  }

  /**
   * @see org.kalypso.editor.mapeditor.IGisviewEditorListener#onGisviewChanged()
   */
  public void onGisviewChanged()
  {
    final List layerlist = m_gisEditor.getLayerlist();
    if( m_tableViewer == null || layerlist == null )
      return;

    m_tableViewer.setInput( m_gisEditor );
    
    for( final Iterator lIt = layerlist.iterator(); lIt.hasNext(); )
    {
      final GisviewLayerType layer = (GisviewLayerType)lIt.next();

      m_tableViewer.setChecked( layer, layer.isVisible() );
    }
    
    m_tableViewer.refresh();
  }

  /**
   * @see org.eclipse.jface.viewers.IDoubleClickListener#doubleClick(org.eclipse.jface.viewers.DoubleClickEvent)
   */
  public void doubleClick( final DoubleClickEvent event )
  {
    /*
    final IStructuredSelection sel = (IStructuredSelection)event.getSelection();
    if( !sel.isEmpty() )
      m_gisEditor.postCommand( new EditPropertiesCommand( getControl().getShell(), (LayerType)sel.getFirstElement() ) );
    */
  }
}