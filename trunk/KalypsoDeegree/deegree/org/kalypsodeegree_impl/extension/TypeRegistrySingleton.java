package org.deegree_impl.extension;

/**
 * Dies sollte irgenwo zentral liegen oder in eine andere solche Klasse
 * integriert werden
 * 
 * @author belger
 */
public class TypeRegistrySingleton
{
  private static ITypeRegistry m_typeRegistry = null;

  private TypeRegistrySingleton()
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