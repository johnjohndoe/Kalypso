package org.kalypso.ogc.gml.gui;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.kalypso.gmlschema.types.ITypeHandlerFactory;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.TypeRegistryException;
import org.kalypso.gmlschema.types.TypeRegistry_impl;

/**
 * Dies sollte irgenwo zentral liegen oder in eine andere solche Klasse integriert werden
 * 
 * @author belger
 */
public class GuiTypeRegistrySingleton
{
  private static final String TYPE_HANDLERS_EXTENSION_POINT = "org.kalypso.ui.typeHandlers"; //$NON-NLS-1$

  private static final String TYPE_HANDLER_FACTORY_CLASS = "factory"; //$NON-NLS-1$

  private static ITypeRegistry<IGuiTypeHandler> m_typeRegistry = null;

  private GuiTypeRegistrySingleton( )
  {
    // wird nicht instantiiert
  }

  @SuppressWarnings("unchecked") //$NON-NLS-1$
  public static synchronized ITypeRegistry<IGuiTypeHandler> getTypeRegistry( )
  {
    if( m_typeRegistry == null )
    {
      m_typeRegistry = new TypeRegistry_impl<IGuiTypeHandler>();
      final IExtensionRegistry registry = Platform.getExtensionRegistry();
      final IExtensionPoint extensionPoint = registry.getExtensionPoint( TYPE_HANDLERS_EXTENSION_POINT );
      final IConfigurationElement[] configurationElements = extensionPoint.getConfigurationElements();
      for( final IConfigurationElement element : configurationElements )
      {
        try
        {
          final ITypeHandlerFactory<IGuiTypeHandler> factory = (ITypeHandlerFactory<IGuiTypeHandler>) element.createExecutableExtension( TYPE_HANDLER_FACTORY_CLASS );
          factory.registerTypeHandlers( m_typeRegistry );
        }
        catch( final CoreException e )
        {
          // TODO handle exception
          e.printStackTrace();
        }
        catch( final TypeRegistryException e )
        {
          // TODO handle exception
          e.printStackTrace();
        }
      }
    }
    return m_typeRegistry;
  }

  public static synchronized void reloadTypeRegistry( )
  {
    m_typeRegistry = null;
    getTypeRegistry();
  }
}