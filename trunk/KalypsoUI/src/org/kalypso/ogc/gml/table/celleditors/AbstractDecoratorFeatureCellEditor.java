package org.kalypso.ogc.gml.table.celleditors;

import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ICellEditorListener;
import org.eclipse.jface.viewers.ICellEditorValidator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

/**
 * @author gernot
 */
public abstract class AbstractDecoratorFeatureCellEditor extends CellEditor
{
  private CellEditor m_cellEditor;  

  public void setCellEditor( final CellEditor cellEditor )
  {
    m_cellEditor = cellEditor;
  }
  
  protected CellEditor getEditor()
  {
    return m_cellEditor;
  }

  /**
   * @see org.eclipse.jface.viewers.CellEditor#activate()
   */
  public void activate()
  {
    if( m_cellEditor != null )
    m_cellEditor.activate();
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#addListener(org.eclipse.jface.viewers.ICellEditorListener)
   */
  public void addListener( ICellEditorListener listener )
  {
    if( m_cellEditor != null )
    m_cellEditor.addListener( listener );
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#addPropertyChangeListener(org.eclipse.jface.util.IPropertyChangeListener)
   */
  public void addPropertyChangeListener( IPropertyChangeListener listener )
  {
    if( m_cellEditor != null )
    m_cellEditor.addPropertyChangeListener( listener );
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#create(org.eclipse.swt.widgets.Composite)
   */
  public void create( final Composite parent )
  {
    if( m_cellEditor != null )
    m_cellEditor.create( parent );
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#deactivate()
   */
  public void deactivate()
  {
    if( m_cellEditor != null )
      m_cellEditor.deactivate();
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#dispose()
   */
  public void dispose()
  {
    if( m_cellEditor != null )
    m_cellEditor.dispose();
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#getControl()
   */
  public Control getControl()
  {
    if( m_cellEditor != null )
    return m_cellEditor.getControl();
    
    return null;
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#getErrorMessage()
   */
  public String getErrorMessage()
  {
    if( m_cellEditor != null )
    return m_cellEditor.getErrorMessage();
    
    return null;
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#getLayoutData()
   */
  public LayoutData getLayoutData()
  {
    if( m_cellEditor != null )
    return m_cellEditor.getLayoutData();
    
    return null;
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#getStyle()
   */
  public int getStyle()
  {
    if( m_cellEditor != null )
    return m_cellEditor.getStyle();
    
    return SWT.NONE;
  }
  
  /**
   * @see org.eclipse.jface.viewers.CellEditor#getValidator()
   */
  public ICellEditorValidator getValidator()
  {
    if( m_cellEditor != null )
    return m_cellEditor.getValidator();
    
    return null;
  }
  
  /**
   * @see org.eclipse.jface.viewers.CellEditor#isActivated()
   */
  public boolean isActivated()
  {
    if( m_cellEditor != null )
    return m_cellEditor.isActivated();
    
    return false;
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#isCopyEnabled()
   */
  public boolean isCopyEnabled()
  {
    if( m_cellEditor != null )
    return m_cellEditor.isCopyEnabled();
    
    return false;
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#isCutEnabled()
   */
  public boolean isCutEnabled()
  {
    if( m_cellEditor != null )
    return m_cellEditor.isCutEnabled();
    
    return false;
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#isDeleteEnabled()
   */
  public boolean isDeleteEnabled()
  {
    if( m_cellEditor != null )
    return m_cellEditor.isDeleteEnabled();
    
    return false;
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#isDirty()
   */
  public boolean isDirty()
  {
    if( m_cellEditor != null )
    return m_cellEditor.isDirty();
    
    return false;
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#isFindEnabled()
   */
  public boolean isFindEnabled()
  {
    if( m_cellEditor != null )
    return m_cellEditor.isFindEnabled();
    
    return false;
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#isPasteEnabled()
   */
  public boolean isPasteEnabled()
  {
    if( m_cellEditor != null )
    return m_cellEditor.isPasteEnabled();
    
    return false;
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#isRedoEnabled()
   */
  public boolean isRedoEnabled()
  {
    if( m_cellEditor != null )
    return m_cellEditor.isRedoEnabled();
    
    return false;
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#isSelectAllEnabled()
   */
  public boolean isSelectAllEnabled()
  {
    if( m_cellEditor != null )
    return m_cellEditor.isSelectAllEnabled();
    
    return false;
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#isUndoEnabled()
   */
  public boolean isUndoEnabled()
  {
    if( m_cellEditor != null )
    return m_cellEditor.isUndoEnabled();
    
    return false;
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#isValueValid()
   */
  public boolean isValueValid()
  {
    if( m_cellEditor != null )
    return m_cellEditor.isValueValid();
    
    return true;
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#performCopy()
   */
  public void performCopy()
  {
    if( m_cellEditor != null )
    m_cellEditor.performCopy();
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#performCut()
   */
  public void performCut()
  {
    if( m_cellEditor != null )
    m_cellEditor.performCut();
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#performDelete()
   */
  public void performDelete()
  {
    if( m_cellEditor != null )
    m_cellEditor.performDelete();
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#performFind()
   */
  public void performFind()
  {
    if( m_cellEditor != null )
    m_cellEditor.performFind();
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#performPaste()
   */
  public void performPaste()
  {
    if( m_cellEditor != null )
    m_cellEditor.performPaste();
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#performRedo()
   */
  public void performRedo()
  {
    if( m_cellEditor != null )
    m_cellEditor.performRedo();
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#performSelectAll()
   */
  public void performSelectAll()
  {
    if( m_cellEditor != null )
    m_cellEditor.performSelectAll();
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#performUndo()
   */
  public void performUndo()
  {
    if( m_cellEditor != null )
    m_cellEditor.performUndo();
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#removeListener(org.eclipse.jface.viewers.ICellEditorListener)
   */
  public void removeListener( ICellEditorListener listener )
  {
    if( m_cellEditor != null )
    m_cellEditor.removeListener( listener );
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#removePropertyChangeListener(org.eclipse.jface.util.IPropertyChangeListener)
   */
  public void removePropertyChangeListener( IPropertyChangeListener listener )
  {
    if( m_cellEditor != null )
    m_cellEditor.removePropertyChangeListener( listener );
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#setFocus()
   */
  public void setFocus()
  {
    if( m_cellEditor != null )
    m_cellEditor.setFocus();
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#setStyle(int)
   */
  public void setStyle( int style )
  {
    if( m_cellEditor != null )
      m_cellEditor.setStyle( style );
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#setValidator(org.eclipse.jface.viewers.ICellEditorValidator)
   */
  public void setValidator( ICellEditorValidator validator )
  {
    if( m_cellEditor != null )
    m_cellEditor.setValidator( validator );
  }

  /**
   * @see org.eclipse.jface.viewers.CellEditor#createControl(org.eclipse.swt.widgets.Composite)
   */
  protected Control createControl( Composite parent )
  {
    // sollte nie aufgerufen werden
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.eclipse.jface.viewers.CellEditor#doSetFocus()
   */
  protected void doSetFocus()
  {
    throw new UnsupportedOperationException();
    // brauchen wir das?  
  }
}
