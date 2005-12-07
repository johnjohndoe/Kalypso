package org.kalypsodeegree_impl.extension;

import java.util.HashMap;
import java.util.Map;

/**
 * Standardimplementation von {@link org.kalypsodeegree_impl.extension.ITypeRegistry}.
 * 
 * @author belger
 */
public class TypeRegistry_impl implements ITypeRegistry
{
  /** typeName -> handler */
  private Map m_typeMap = new HashMap();

  /** className -> handler */
  private Map m_classMap = new HashMap();

  /**
   * Falls TypeName oder ClassName bereits belegt sind
   * 
   * @throws TypeRegistryException
   * 
   * @see org.kalypsodeegree_impl.extension.ITypeRegistry#registerTypeHandler(org.kalypsodeegree_impl.extension.IMarshallingTypeHandler)
   */
  public void registerTypeHandler( final ITypeHandler typeHandler ) throws TypeRegistryException
  {
    final String typeName = typeHandler.getTypeName();
    final String className = typeHandler.getClassName();

    if( m_typeMap.containsKey( typeName ) )
      throw new TypeRegistryException( "Typname wurde bereits registriert: " + typeName );

    if( m_classMap.containsKey( className ) )
      throw new TypeRegistryException( "Classname wurde bereits registriert: " + className );

    m_typeMap.put( typeName, typeHandler );
    m_classMap.put( className, typeHandler );
  }

  /**
   * @see org.kalypsodeegree_impl.extension.ITypeRegistry#getTypeHandlerForTypeName(java.lang.String)
   */
  public ITypeHandler getTypeHandlerForTypeName( final String typeName )
  {
    if( !hasTypeName( typeName ) )
      return null;
    return (ITypeHandler)m_typeMap.get( typeName );
  }

  /**
   * @see org.kalypsodeegree_impl.extension.ITypeRegistry#getTypeHandlerForClassName(java.lang.String)
   */
  public ITypeHandler getTypeHandlerForClassName( final String className )
  {
    if( !hasClassName( className ) )
      return null;

    return (ITypeHandler)m_classMap.get( className );
  }

  /**
   * @see org.kalypsodeegree_impl.extension.ITypeRegistry#unregisterTypeHandler(org.kalypsodeegree_impl.extension.IMarshallingTypeHandler)
   */
  public void unregisterTypeHandler( ITypeHandler typeHandler )
  {
    m_typeMap.remove( typeHandler.getTypeName() );
    m_classMap.remove( typeHandler.getClassName() );
  }

  /**
   * @see org.kalypsodeegree_impl.extension.ITypeRegistry#hasTypeName(java.lang.String)
   */
  public boolean hasTypeName( final String typeName )
  {
    return m_typeMap.containsKey( typeName );
  }

  /**
   * @see org.kalypsodeegree_impl.extension.ITypeRegistry#hasClassName(java.lang.String)
   */
  public boolean hasClassName( final String className )
  {
    return m_classMap.containsKey( className );
  }
}