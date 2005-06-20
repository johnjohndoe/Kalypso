package org.bce.eclipse.platform.adapter;

import org.eclipse.core.runtime.IAdapterFactory;
import org.eclipse.core.runtime.IPath;
import org.eclipse.ui.editors.text.ILocationProvider;
import org.eclipse.ui.internal.editors.text.JavaFileEditorInput;

/**
 * AdapterFactory to adapt {@link org.eclipse.ui.internal.editors.text.JavaFileEditorInput}to
 * {@link org.eclipse.core.runtime.IPath}
 * 
 * <p>
 * This may be needed when you want your editor to open files via 'Open external editor' (which proved a
 * JavaFileEditorInput) but your plugins should only depend on Eclipse-RCP. In this case, you can deploy your
 * application with this additional plugin and put
 * <code>Platform.getAdapterManager().loadAdapter( input, IPath.class.getName() ) in your
 * setInput Method of your editor.</p>
 * 
 * @author gernot
 *
 */
public class JavaFileEditorInputAdapter implements IAdapterFactory
{
  /**
   * @see org.eclipse.core.runtime.IAdapterFactory#getAdapter(java.lang.Object, java.lang.Class)
   */
  public Object getAdapter( final Object adaptableObject, final Class adapterType )
  {
    if( adaptableObject instanceof JavaFileEditorInput )
    {
      final JavaFileEditorInput input = (JavaFileEditorInput)adaptableObject;
      final ILocationProvider lp = (ILocationProvider)input.getAdapter( ILocationProvider.class );
      if( lp != null )
        return lp.getPath( input );
    }

    return null;
  }

  /**
   * @see org.eclipse.core.runtime.IAdapterFactory#getAdapterList()
   */
  public Class[] getAdapterList()
  {
    return new Class[]
    { IPath.class };
  }
}
