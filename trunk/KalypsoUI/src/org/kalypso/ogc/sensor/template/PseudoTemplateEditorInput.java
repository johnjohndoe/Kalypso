package org.kalypso.ogc.sensor.template;

import org.eclipse.core.resources.IStorage;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.IPersistableElement;
import org.eclipse.ui.IStorageEditorInput;
import org.kalypso.java.io.FileUtilities;

/**
 * PseudoTemplateEditorInput
 * 
 * @author schlienger
 */
public class PseudoTemplateEditorInput implements IStorageEditorInput
{
  private final TemplateStorage m_storage;

  private final String m_fileExtension;

  /**
   * Constructor
   * 
   * @param storage
   *          template storage on which this pseudo template is based
   * @param fileExtension
   *          name of the file extension that this template should have once
   *          saved
   */
  public PseudoTemplateEditorInput( final TemplateStorage storage,
      final String fileExtension )
  {
    m_storage = storage;
    m_fileExtension = fileExtension;
  }

  /**
   * @see org.eclipse.ui.IStorageEditorInput#getStorage()
   */
  public IStorage getStorage( ) throws CoreException
  {
    return m_storage;
  }

  /**
   * @see org.eclipse.ui.IEditorInput#exists()
   */
  public boolean exists( )
  {
    return false;
  }

  /**
   * @see org.eclipse.ui.IEditorInput#getImageDescriptor()
   */
  public ImageDescriptor getImageDescriptor( )
  {
    return null;
  }

  /**
   * @see org.eclipse.ui.IEditorInput#getName()
   */
  public String getName( )
  {
    return "Vorlage für "
        + FileUtilities.nameWithoutExtension( m_storage.getName() ) + m_fileExtension;
  }

  /**
   * @see org.eclipse.ui.IEditorInput#getPersistable()
   */
  public IPersistableElement getPersistable( )
  {
    return null;
  }

  /**
   * @see org.eclipse.ui.IEditorInput#getToolTipText()
   */
  public String getToolTipText( )
  {
    return m_storage.getHref();
  }

  /**
   * @see org.eclipse.core.runtime.IAdaptable#getAdapter(java.lang.Class)
   */
  public Object getAdapter( Class adapter )
  {
    return null;
  }

  /**
   * Call is delegated to Object class since we always want new pseudo templates
   * even if the underlying file is the same one.
   * 
   * @see java.lang.Object#equals(java.lang.Object)
   */
  public boolean equals( Object obj )
  {
    return super.equals( obj );
  }
}