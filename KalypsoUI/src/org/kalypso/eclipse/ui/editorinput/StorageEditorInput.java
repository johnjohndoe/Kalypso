package org.kalypso.eclipse.ui.editorinput;

import org.eclipse.core.resources.IStorage;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.IPersistableElement;
import org.eclipse.ui.IStorageEditorInput;

/**
 * TODO: normally should also override equals() according to
 * interface specification... Check if this is possible here.
 * 
 * @author belger
 */
public class StorageEditorInput implements IStorageEditorInput
{
  private final IStorage m_storage;

  public StorageEditorInput( final IStorage storage )
  {
    m_storage = storage;
  }

  /**
   * @see org.eclipse.ui.IStorageEditorInput#getStorage()
   */
  public IStorage getStorage()
  {
    return m_storage;
  }

  /**
   * @see org.eclipse.ui.IEditorInput#exists()
   */
  public boolean exists()
  {
    return true;
  }

  /**
   * @see org.eclipse.ui.IEditorInput#getImageDescriptor()
   */
  public ImageDescriptor getImageDescriptor()
  {
    return null;
  }

  /**
   * @see org.eclipse.ui.IEditorInput#getName()
   */
  public String getName()
  {
    return m_storage.getName();
  }

  /**
   * @see org.eclipse.ui.IEditorInput#getPersistable()
   */
  public IPersistableElement getPersistable()
  {
    return null;
  }

  /**
   * @see org.eclipse.ui.IEditorInput#getToolTipText()
   */
  public String getToolTipText()
  {
    // null not allowed, so return name of storage
    return m_storage.getName();
  }

  /**
   * @see org.eclipse.core.runtime.IAdaptable#getAdapter(java.lang.Class)
   */
  public Object getAdapter( Class adapter )
  {
    return null;
  }
}