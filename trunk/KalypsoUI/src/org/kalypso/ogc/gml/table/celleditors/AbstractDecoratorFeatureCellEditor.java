package org.kalypso.ogc.gml.table.celleditors;

import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ICellEditorListener;
import org.eclipse.jface.viewers.ICellEditorValidator;
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
    m_cellEditor.activate();
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#addListener(org.eclipse.jface.viewers.ICellEditorListener)
   */
  public void addListener( ICellEditorListener listener )
  {
    m_cellEditor.addListener( listener );
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#addPropertyChangeListener(org.eclipse.jface.util.IPropertyChangeListener)
   */
  public void addPropertyChangeListener( IPropertyChangeListener listener )
  {
    m_cellEditor.addPropertyChangeListener( listener );
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#create(org.eclipse.swt.widgets.Composite)
   */
  public void create( Composite parent )
  {
    m_cellEditor.create( parent );
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#deactivate()
   */
  public void deactivate()
  {
    m_cellEditor.deactivate();
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#dispose()
   */
  public void dispose()
  {
    m_cellEditor.dispose();
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#getControl()
   */
  public Control getControl()
  {
    return m_cellEditor.getControl();
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#getErrorMessage()
   */
  public String getErrorMessage()
  {
    return m_cellEditor.getErrorMessage();
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#getLayoutData()
   */
  public LayoutData getLayoutData()
  {
    return m_cellEditor.getLayoutData();
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#getStyle()
   */
  public int getStyle()
  {
    return m_cellEditor.getStyle();
  }
  
  /**
   * @see org.eclipse.jface.viewers.CellEditor#getValidator()
   */
  public ICellEditorValidator getValidator()
  {
    return m_cellEditor.getValidator();
  }
  
  /**
   * @see org.eclipse.jface.viewers.CellEditor#isActivated()
   */
  public boolean isActivated()
  {
    return m_cellEditor.isActivated();
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#isCopyEnabled()
   */
  public boolean isCopyEnabled()
  {
    return m_cellEditor.isCopyEnabled();
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#isCutEnabled()
   */
  public boolean isCutEnabled()
  {
    return m_cellEditor.isCutEnabled();
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#isDeleteEnabled()
   */
  public boolean isDeleteEnabled()
  {
    return m_cellEditor.isDeleteEnabled();
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#isDirty()
   */
  public boolean isDirty()
  {
    return m_cellEditor.isDirty();
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#isFindEnabled()
   */
  public boolean isFindEnabled()
  {
    return m_cellEditor.isFindEnabled();
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#isPasteEnabled()
   */
  public boolean isPasteEnabled()
  {
    return m_cellEditor.isPasteEnabled();
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#isRedoEnabled()
   */
  public boolean isRedoEnabled()
  {
    return m_cellEditor.isRedoEnabled();
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#isSelectAllEnabled()
   */
  public boolean isSelectAllEnabled()
  {
    return m_cellEditor.isSelectAllEnabled();
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#isUndoEnabled()
   */
  public boolean isUndoEnabled()
  {
    return m_cellEditor.isUndoEnabled();
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#isValueValid()
   */
  public boolean isValueValid()
  {
    return m_cellEditor.isValueValid();
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#performCopy()
   */
  public void performCopy()
  {
    m_cellEditor.performCopy();
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#performCut()
   */
  public void performCut()
  {
    m_cellEditor.performCut();
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#performDelete()
   */
  public void performDelete()
  {
    m_cellEditor.performDelete();
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#performFind()
   */
  public void performFind()
  {
    m_cellEditor.performFind();
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#performPaste()
   */
  public void performPaste()
  {
    m_cellEditor.performPaste();
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#performRedo()
   */
  public void performRedo()
  {
    m_cellEditor.performRedo();
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#performSelectAll()
   */
  public void performSelectAll()
  {
    m_cellEditor.performSelectAll();
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#performUndo()
   */
  public void performUndo()
  {
    m_cellEditor.performUndo();
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#removeListener(org.eclipse.jface.viewers.ICellEditorListener)
   */
  public void removeListener( ICellEditorListener listener )
  {
    m_cellEditor.removeListener( listener );
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#removePropertyChangeListener(org.eclipse.jface.util.IPropertyChangeListener)
   */
  public void removePropertyChangeListener( IPropertyChangeListener listener )
  {
    m_cellEditor.removePropertyChangeListener( listener );
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#setFocus()
   */
  public void setFocus()
  {
    m_cellEditor.setFocus();
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#setStyle(int)
   */
  public void setStyle( int style )
  {
    m_cellEditor.setStyle( style );
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#setValidator(org.eclipse.jface.viewers.ICellEditorValidator)
   */
  public void setValidator( ICellEditorValidator validator )
  {
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
