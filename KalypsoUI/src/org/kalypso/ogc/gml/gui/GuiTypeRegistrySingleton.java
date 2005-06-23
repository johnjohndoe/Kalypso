package org.kalypso.ogc.gml.gui;

import org.kalypsodeegree_impl.extension.ITypeRegistry;
import org.kalypsodeegree_impl.extension.TypeRegistry_impl;

/**
 * Dies sollte irgenwo zentral liegen oder in eine andere solche Klasse integriert werden
 * 
 * @author belger
 */
public class GuiTypeRegistrySingleton
{
  private static ITypeRegistry m_typeRegistry = null;

  // load default types
//  static
//  {
//    ITypeRegistry typeRegistry = getTypeRegistry();
//    try
//    {
//      typeRegistry.registerTypeHandler( new GMLBoundingShapeTypeHandler() );
//    }
//    catch( TypeRegistryException e )
//    {
//      e.printStackTrace();
//    }
//
//  }

  private GuiTypeRegistrySingleton()
  {
  // wird nicht instantiiert
  }

  public static ITypeRegistry getTypeRegistry()
  {
    if( m_typeRegistry == null )
      m_typeRegistry = new TypeRegistry_impl();

    return m_typeRegistry;
  }
}