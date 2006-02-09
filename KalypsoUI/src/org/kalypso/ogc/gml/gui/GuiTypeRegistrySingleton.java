package org.kalypso.ogc.gml.gui;

import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.TypeRegistry_impl;

/**
 * Dies sollte irgenwo zentral liegen oder in eine andere solche Klasse integriert werden
 * 
 * @author belger
 */
public class GuiTypeRegistrySingleton
{
  private static ITypeRegistry m_typeRegistry = null;

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